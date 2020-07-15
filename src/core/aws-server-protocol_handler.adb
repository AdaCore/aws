------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2018, AdaCore                     --
--                                                                          --
--  This library is free software;  you can redistribute it and/or modify   --
--  it under terms of the  GNU General Public License  as published by the  --
--  Free Software  Foundation;  either version 3,  or (at your  option) any --
--  later version. This library is distributed in the hope that it will be  --
--  useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                    --
--                                                                          --
--  As a special exception under Section 7 of GPL version 3, you are        --
--  granted additional permissions described in the GCC Runtime Library     --
--  Exception, version 3.1, as published by the Free Software Foundation.   --
--                                                                          --
--  You should have received a copy of the GNU General Public License and   --
--  a copy of the GCC Runtime Library Exception along with this program;    --
--  see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see   --
--  <http://www.gnu.org/licenses/>.                                         --
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

with Ada.Exceptions;

with AWS.Log;
with AWS.Messages;
with AWS.Net.Buffered;
with AWS.Parameters;
with AWS.Response.Set;
with AWS.Server.HTTP_Utils;
with AWS.Server.Status;
with AWS.Status.Set;
with AWS.Utils;

separate (AWS.Server)

procedure Protocol_Handler (LA : in out Line_Attribute_Record) is

   use AWS.Server.HTTP_Utils;

   Case_Sensitive_Parameters : constant Boolean :=
                                 CNF.Case_Sensitive_Parameters
                                   (LA.Server.Properties);

   Sock_Ptr     : Socket_Access;

   Socket_Taken : Boolean := False;
   --  Set to True if a socket has been reserved for a push session

   Will_Close   : Boolean := True;
   --  Will_Close is set to true when the connection will be closed by the
   --  server. It means that the server is about to send the latest message
   --  to the client using this socket. The value will be changed by
   --  Set_Close_Status.

   Keep_Alive_Limit : constant Natural :=
                        CNF.Free_Slots_Keep_Alive_Limit (LA.Server.Properties);

   Free_Slots   : Natural;

   Extended_Log : constant Boolean :=
                    CNF.Log_Extended_Fields_Length (LA.Server.Properties) > 0;

   Multislots   : constant Boolean :=
                    CNF.Max_Connection (LA.Server.Properties) > 1;

begin
   --  This new connection has been initialized because some data are being
   --  sent. We are by default using HTTP/1.1 persistent connection. We will
   --  exit this loop only if the client request so or if we time-out on
   --  waiting for a request.

   LA.Log_Data := AWS.Log.Empty_Fields_Table;

   For_Every_Request : loop
      declare
         use Ada.Streams;
         use type Response.Data_Mode;

         Expectation_Failed : exception;

         Error_Answer : Response.Data;
         Back_OK      : Boolean;
         First_Line   : Boolean := True;
         Switch       : constant array (Boolean) of
                          not null access function
                            (Socket : Net.Socket_Type'Class;
                             Events : Net.Wait_Event_Set) return Net.Event_Set
                          := (True  => Net.Wait'Access,
                              False => Net.Check'Access);

         function Send_Error_Answer return Boolean;
         --  Send Error_Answer to the client, returns False if an(other)
         --  error occurs while trying to send the error answer.

         -----------------------
         -- Send_Error_Answer --
         -----------------------

         function Send_Error_Answer return Boolean is
         begin
            Send
              (Error_Answer, LA.Server.all, LA.Line, LA.Stat, Socket_Taken,
               Will_Close);
            return True;

         exception
            when Net.Socket_Error =>
               Will_Close := True;
               return False;

            when E : others =>
               AWS.Log.Write
                 (LA.Server.Error_Log,
                  LA.Stat,
                  Utils.CRLF_2_Spaces
                    (Ada.Exceptions.Exception_Information (E)));
               return False;
         end Send_Error_Answer;

      begin
         Response.Set.Mode (Error_Answer, Response.No_Data);

         LA.Server.Slots.Mark_Phase (LA.Line, Client_Header);

         if Sock_Ptr = null then
            --  First arrived. We do not need to wait for fast comming next
            --  keep alive request.

            Sock_Ptr := LA.Server.Slots.Get (Index => LA.Line).Sock;

         else
            --  Wait/Check to get header (or timeout) and put the socket
            --  back into the acceptor to wait for next client request.

            if not Switch (LA.Server.Slots.Free_Slots > 0)
              (Sock_Ptr.all,
               (Net.Input => True, others => False)) (Net.Input)
            then
               LA.Server.Slots.Prepare_Back (LA.Line, Back_OK);

               if Back_OK then
                  Net.Acceptors.Give_Back
                    (LA.Server.Acceptor, Sock_Ptr, Back_OK);

                  if not Back_OK then
                     AWS.Log.Write
                       (LA.Server.Error_Log,
                        "Could not put socket back into acceptor, line"
                        & LA.Line'Img);

                     Sock_Ptr.Shutdown;
                  end if;
               end if;

               exit For_Every_Request;
            end if;
         end if;

         AWS.Status.Set.Reset (LA.Stat);

         --  Set status socket and peername

         AWS.Status.Set.Socket (LA.Stat, Sock_Ptr);

         if Extended_Log then
            AWS.Log.Set_Field (LA.Server.Log, LA.Log_Data,
               "c-ip", Net.Peer_Addr (Sock_Ptr.all));

            AWS.Log.Set_Field (LA.Server.Log, LA.Log_Data,
               "c-port", Utils.Image (Net.Peer_Port (Sock_Ptr.all)));

            AWS.Log.Set_Field (LA.Server.Log, LA.Log_Data,
               "s-ip", Net.Get_Addr (Sock_Ptr.all));

            AWS.Log.Set_Field (LA.Server.Log, LA.Log_Data,
               "s-port", Utils.Image (Net.Get_Port (Sock_Ptr.all)));
         end if;

         AWS.Status.Set.Case_Sensitive_Parameters
           (LA.Stat, Case_Sensitive_Parameters);

         Get_Request_Line (LA.Stat);

         First_Line := False;

         AWS.Status.Set.Read_Header (Socket => Sock_Ptr.all, D => LA.Stat);

         AWS.Status.Set.Connection_Data
           (LA.Stat,
            CNF.Server_Host (LA.Server.Properties),
            AWS.Server.Status.Port (LA.Server.all),
            CNF.Security (LA.Server.Properties));

         LA.Server.Slots.Increment_Slot_Activity_Counter (LA.Line, Free_Slots);

         --  If there is no more slot available and we have many
         --  of them, try to abort one of them.

         if Multislots and then Free_Slots = 0 then
            Force_Clean (LA.Server.all);
         end if;

         if Extended_Log then
            AWS.Log.Set_Field
              (LA.Server.Log, LA.Log_Data,
               "s-free-slots", Utils.Image (Free_Slots));
         end if;

         Set_Close_Status
           (LA.Stat,
            Keep_Alive => Free_Slots >= Keep_Alive_Limit,
            Will_Close => Will_Close);

         --  Is there something to read ?

         if AWS.Status.Content_Length (LA.Stat) = 0
           and then AWS.Status.Transfer_Encoding (LA.Stat) /= "chunked"
         then
            LA.Server.Slots.Mark_Phase (LA.Line, Server_Processing);

         else
            declare
               Expect : constant String := AWS.Status.Expect (LA.Stat);
            begin
               LA.Expect_100 := Expect = Messages.S100_Continue;

               if not LA.Expect_100 and then Expect /= "" then
                  Will_Close := True;

                  Error_Answer := Response.Build
                    (Status_Code  => Messages.S417,
                     Content_Type => "text/plain",
                     Message_Body => "Unknown Expect header value " & Expect);

                  raise Expectation_Failed;
               end if;
            end;

            LA.Server.Slots.Mark_Phase (LA.Line, Client_Data);

            if AWS.Status.Content_Length (LA.Stat)
               <= Stream_Element_Count
                    (CNF.Upload_Size_Limit (LA.Server.Properties))
            then
               Get_Message_Data
                 (LA.Server.all, LA.Line, LA.Stat, LA.Expect_100);
            end if;
         end if;

         AWS.Status.Set.Keep_Alive (LA.Stat, not Will_Close);

         Answer_To_Client
           (LA.Server.all, LA.Line, LA.Stat, Socket_Taken, Will_Close);

      exception
            --  We must never exit the loop with an exception. This loop is
            --  supposed to be used for the keep-alive connection. We must exit
            --  properly and the slot will be closed. An exception propagated
            --  outside of this loop will kill definitely one of the server's
            --  slot.

         when Net.Socket_Error =>
            --  Exit from keep-alive loop in case of socket error
            exit For_Every_Request;

         when Expectation_Failed =>
            exit For_Every_Request when not Send_Error_Answer;

         when E : Wrong_Request_Line =>
            AWS.Log.Write
              (LA.Server.Error_Log,
               LA.Stat,
               Utils.CRLF_2_Spaces
                 (Ada.Exceptions.Exception_Information (E)));

            Will_Close := True;

            Error_Answer := Response.Build
              (Status_Code  => Messages.S400,
               Content_Type => "text/plain",
               Message_Body => Ada.Exceptions.Exception_Message (E));

            LA.Server.Slots.Mark_Phase (LA.Line, Server_Response);

            exit For_Every_Request when not Send_Error_Answer;

         when E : Net.Buffered.Data_Overflow
           | Parameters.Too_Long_Parameter
           | Parameters.Too_Many_Parameters
           =>
            AWS.Log.Write
              (LA.Server.Error_Log,
               LA.Stat,
               Utils.CRLF_2_Spaces
                 (Ada.Exceptions.Exception_Information (E)));

            Will_Close := True;

            if First_Line then
               Error_Answer := Response.Build
                 (Status_Code  => Messages.S414,
                  Content_Type => "text/plain",
                  Message_Body => Ada.Exceptions.Exception_Message (E));

            elsif
              Exception_Identity (E) =
                Parameters.Too_Many_Parameters'Identity
            then
               Error_Answer := Response.Build
                 (Status_Code  => Messages.S403,
                  Content_Type => "text/plain",
                  Message_Body => Ada.Exceptions.Exception_Message (E));

            else
               Error_Answer := Response.Build
                 (Status_Code  => Messages.S400,
                  Content_Type => "text/plain",
                  Message_Body => Ada.Exceptions.Exception_Message (E));
            end if;

            LA.Server.Slots.Mark_Phase (LA.Line, Server_Response);

            exit For_Every_Request when not Send_Error_Answer;

         when E : others =>
            declare
               Phase : constant Slot_Phase := LA.Server.Slots.Phase (LA.Line);
            begin
               --  Log this error

               AWS.Log.Write
                 (LA.Server.Error_Log,
                  LA.Stat,
                  Utils.CRLF_2_Spaces
                    (Ada.Exceptions.Exception_Information (E)));

               --  Call exception handler

               LA.Server.Exception_Handler
                 (E,
                  LA.Server.Error_Log,
                  AWS.Exceptions.Data'(False, LA.Line, LA.Stat),
                  Error_Answer);

               --  We have an exception while sending data back to the
               --  client. This is most probably an exception coming
               --  from a user's stream. The only option is to exit and
               --  close the connection, we can't recover in a middle of
               --  a response.

               exit For_Every_Request when Phase = Server_Response;

               if Phase = Client_Data then
                  Will_Close := True;
               end if;

               if Response.Mode (Error_Answer) /= Response.No_Data then
                  LA.Server.Slots.Mark_Phase (LA.Line, Server_Response);

                  exit For_Every_Request when not Send_Error_Answer;
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
                    (LA.Server.Error_Log,
                     LA.Stat,
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
        or else LA.Server.Shutdown;

   end loop For_Every_Request;

   --  Release memory for local objects

   AWS.Status.Set.Free (LA.Stat);
end Protocol_Handler;
