------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                            Copyright (C) 2000                            --
--                               Pascal Obry                                --
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

--  $Id$

with Ada.Text_IO;
with Ada.Strings.Fixed;

with AWS.Messages;
with AWS.Status;

package body AWS.Connection is

   use Ada;
   use Ada.Strings;

   End_Of_Message : constant String := "";

   Host : constant String := "Host: ";
   subtype Host_Range is Positive range Host'Range;

   Get : constant String := "GET ";
   subtype Get_Range is Positive range Get'Range;

   ----------
   -- Line --
   ----------

   task body Line is

      Sock    : Sockets.Socket_FD;
      Handler : Ressources.Callback;
      C_Stat  : Status.Data;         --  connection status

      procedure Parse (Command : in String);
      --  parse a line sent by the client and do what is needed

      procedure Parse (Command : in String) is
      begin
         if Command (Host_Range) = Host then
            Status.Set_Host (C_Stat,
                             Command (Host'Length + 1 .. Command'Last));

         elsif Command (Get_Range) = Get then
            declare
               Params : constant String
                 := Command (Get'Length + 1 .. Command'Last);
            begin
               Status.Set_Get
                 (C_Stat,
                  Params (Params'First .. Fixed.Index (Params, " ") - 1));
            end;
         end if;
      end Parse;

   begin
      select
         accept Start (FD : in Sockets.Socket_FD;
                       CB : in Ressources.Callback) do
            Sock    := FD;
            Handler := CB;
         end Start;
      or
         terminate;
      end select;

      loop
         declare
            Data : constant String := Sockets.Get_Line (Sock);
         begin
            if Data = End_Of_Message then
               Text_IO.Put_Line ("Ok let me answer to that...");

               --  First let's pretend that there is not problem

               Sockets.Put_Line (Sock, Messages.Status_Line (Messages.S200));

               --  Get the message body from user's callback

               declare
                  Message : constant String :=
                    Handler (Status.Get (C_Stat));
               begin

                  --  Now we output the message body length

                  Sockets.Put_Line (Sock, "Content-Length: "
                                    & Positive'Image (Message'Length));

                  --  We handle only text/html message type

                  Sockets.Put_Line (Sock, "Content-Type: text/html");

                  --  End of header

                  Sockets.New_Line (Sock);

                  Sockets.Put_Line (Sock, Message);
               end;
            else
               Text_IO.Put_Line (Data);
               Parse (Data);
            end if;
         end;
      end loop;

   exception
      when Sockets.Connection_Closed =>
         Text_IO.Put_Line ("Connection closed");
         Sockets.Shutdown (Sock, Sockets.Both);
   end Line;

end AWS.Connection;
