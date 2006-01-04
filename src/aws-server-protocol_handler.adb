------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2005                          --
--                                 AdaCore                                  --
--                                                                          --
--  This library is free software; you can redistribute it and/or modify    --
--  it under the terms of the GNU General Public License as published by    --
--  the Free Software Foundation; either version 2 of the License, or (at   --
--  your option) any later version.                                         --
--                                                                          --
--  This library is distributed in the hope that it will be useful, but     --
--  WITHOUT ANY WARRANTY; without even the implied warranty of              --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU       --
--  General Public License for more details.                                --
--                                                                          --
--  You should have received a copy of the GNU General Public License       --
--  along with this library; if not, write to the Free Software Foundation, --
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.          --
--                                                                          --
--  As a special exception, if other files instantiate generics from this   --
--  unit, or you link this unit with other files to produce an executable,  --
--  this  unit  does not  by itself cause  the resulting executable to be   --
--  covered by the GNU General Public License. This exception does not      --
--  however invalidate any other reasons why the executable file  might be  --
--  covered by the  GNU Public License.                                     --
------------------------------------------------------------------------------

--  This procedure is responsible of handling the HTTP protocol. Every
--  responses and incoming requests are parsed/formated here.

with AWS.Config;
with AWS.Log;
with AWS.Messages;
with AWS.MIME;
with AWS.OS_Lib;
with AWS.Resources;
with AWS.Server.HTTP_Utils;
with AWS.Session;
with AWS.Status.Set;
with AWS.Templates;
with AWS.Utils;

separate (AWS.Server)

procedure Protocol_Handler
  (HTTP_Server : in out HTTP;
   Index       : in     Positive;
   Keep_Alive  : in     Boolean)
is
   use AWS.Server.HTTP_Utils;

   use type Resources.Content_Length_Type;

   Case_Sensitive_Parameters : constant Boolean
     := CNF.Case_Sensitive_Parameters (HTTP_Server.Properties);

   C_Stat         : aliased AWS.Status.Data; -- Connection status

   Sock_Ptr       : constant Socket_Access
     := HTTP_Server.Slots.Get (Index => Index).Sock;

   Socket_Taken   : Boolean := False;
   --  Set to True if a socket has been reserved for a push session.

   Will_Close     : Boolean := True;
   --  Will_Close is set to true when the connection will be closed by the
   --  server. It means that the server is about to send the latest message
   --  to the client using this socket. The value will be changed by
   --  Set_Close_Status.

   Data_Sent      : Boolean := False;
   --  Will be set to true when some data will have been sent back to the
   --  client. At this point it is not possible to send an unexpected
   --  exception message to the client. The only option in case of problems is
   --  to close the connection.

begin
   --  This new connection has been initialized because some data are being
   --  sent. We are by default using HTTP/1.1 persistent connection. We will
   --  exit this loop only if the client request so or if we time-out on
   --  waiting for a request.

   Line_Attribute.Set_Value
     ((HTTP_Server.Self, Index, C_Stat'Unchecked_Access));

   For_Every_Request : loop

      begin
         Data_Sent := False;

         HTTP_Server.Slots.Mark_Phase (Index, Wait_For_Client);

         Status.Set.Reset (C_Stat);

         --  Set status peername

         Status.Set.Peername (C_Stat, HTTP_Server.Slots.Get_Peername (Index));
         Status.Set.Socket (C_Stat, Sock_Ptr);

         Status.Set.Case_Sensitive_Parameters
           (C_Stat, Case_Sensitive_Parameters);

         Get_Message_Header (HTTP_Server, Index, C_Stat);

         HTTP_Server.Slots.Increment_Slot_Activity_Counter (Index);

         Set_Close_Status (C_Stat, Keep_Alive, Will_Close);

         HTTP_Server.Slots.Mark_Phase (Index, Client_Data);

         Get_Message_Data (HTTP_Server, Index, C_Stat);

         Status.Set.Keep_Alive (C_Stat, not Will_Close);

         HTTP_Server.Slots.Mark_Phase (Index, Server_Response);

         Answer_To_Client
           (HTTP_Server, Index, C_Stat, Socket_Taken, Will_Close, Data_Sent);

      exception
            --  We must never exit the loop with an exception. This loop is
            --  supposed to be used for the keep-alive connection. We must exit
            --  properly and the slot will be closed. An exception propagated
            --  outside of this loop will kill definitely one of the server's
            --  slot.

         when Net.Socket_Error =>
            --  Exit from keep-alive loop in case of socket error
            exit For_Every_Request;

         when E : others =>
            declare
               use type Response.Data_Mode;

               Answer : Response.Data;
            begin
               --  Log this error

               AWS.Log.Write
                 (HTTP_Server.Error_Log,
                  C_Stat,
                  Utils.CRLF_2_Spaces
                    (Ada.Exceptions.Exception_Information (E)));

               --  Call exception handler

               HTTP_Server.Exception_Handler
                 (E,
                  HTTP_Server.Error_Log,
                  AWS.Exceptions.Data'(False, Index, C_Stat),
                  Answer);

               --  We have an exception while sending data back to the
               --  client. This is most probably an exception coming
               --  from a user's stream. The only option is to exit and
               --  close the connection, we can't recover in a middle of
               --  a response.

               exit For_Every_Request when Data_Sent;

               if Response.Mode (Answer) /= Response.No_Data then
                  HTTP_Server.Slots.Mark_Phase (Index, Server_Response);

                  Send
                    (Answer, HTTP_Server, Index, C_Stat,
                     Socket_Taken, Will_Close, Data_Sent);
               end if;

            exception
               when Net.Socket_Error =>
                  --  There is nothing further we can do. The socket has
                  --  certainly been closed while sending back the answer.
                  exit For_Every_Request;

               when E : others =>
                  --  Here we got an exception (other than Net.Socket_Error).
                  --  It is probably due to a problem in a user's stream
                  --  implementation. Just log the problem and exit.
                  AWS.Log.Write
                    (HTTP_Server.Error_Log,
                     C_Stat,
                     "Exception handler bug "
                     & Utils.CRLF_2_Spaces
                       (Ada.Exceptions.Exception_Information (E)));
                  exit For_Every_Request;
            end;
      end;

      --  Exit if connection has not the Keep-Alive status or we are working
      --  on HTTP/1.0 protocol or we have a single slot.

      exit For_Every_Request when Will_Close
        or else Socket_Taken
        or else HTTP_Server.Shutdown;

   end loop For_Every_Request;

   --  Release memory for local objects

   Status.Set.Free (C_Stat);
end Protocol_Handler;
