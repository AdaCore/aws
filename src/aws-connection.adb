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
with Ada.Strings.Unbounded;

with AWS.Messages;
with AWS.Status;

package body AWS.Connection is

   use Ada;
   use Ada.Strings;
   use Ada.Strings.Unbounded;

   End_Of_Message : constant String := "";

   Host_Token : constant String := "Host: ";
   subtype Host_Range is Positive range Host_Token'Range;

   Get_Token : constant String := "GET ";
   subtype Get_Range is Positive range Get_Token'Range;

   Connection_Token : constant String := "Connection: ";
   subtype Connection_Range is Positive range Connection_Token'Range;

   ----------
   -- Line --
   ----------

   task body Line is

      Sock    : Sockets.Socket_FD;
      Handler : Response.Callback;
      C_Stat  : Status.Data;         --  connection status

      procedure Parse (Command : in String);
      --  parse a line sent by the client and do what is needed

      -----------
      -- Parse --
      -----------

      procedure Parse (Command : in String) is

         I1, I2 : Natural;
         --  index of first space and second space

         I3 : Natural;
         --  index of ? if present in the URI (means that there is some
         --  parameters)

         procedure Cut_Command;
         --  parse Command and set I1, I2 and I3

         function URI return String;
         pragma Inline (URI);
         --  returns first parameter. parameters are separated by spaces.

         function Parameters return String;
         --  returns parameters if some where specified in the URI.

         function HTTP_Version return String;
         pragma Inline (HTTP_Version);
         --  returns second parameter. parameters are separated by spaces.

         function Parse_Request_Line (Command : in String) return Boolean;
         --  parse the request line:
         --  Request-Line = Method SP Request-URI SP HTTP-Version CRLF

         -----------------
         -- Cut_Command --
         -----------------

         procedure Cut_Command is
         begin
            I1 := Fixed.Index (Command, " ");
            I2 := Fixed.Index (Command (I1 + 1 .. Command'Last), " ");
            I3 := Fixed.Index (Command (I1 + 1 .. I2), "?");
         end Cut_Command;

         ---------
         -- URI --
         ---------

         function URI return String is
         begin
            if I3 = 0 then
               return Command (I1 + 1 .. I2 - 1);
            else
               return Command (I1 + 1 .. I3 - 1);
            end if;
         end URI;

         ------------------
         -- HTTP_Version --
         ------------------

         function HTTP_Version return String is
         begin
            return Command (I2 + 1 .. Command'Last);
         end HTTP_Version;

         ----------------
         -- Parameters --
         ----------------

         function Parameters return String is
         begin
            if I3 = 0 then
               return "";
            else
               return Command (I3 + 1 .. I2 - 1);
            end if;
         end Parameters;

         ------------------------
         -- Parse_Request_Line --
         ------------------------

         function Parse_Request_Line (Command : in String) return Boolean is
         begin
            Cut_Command;

            if Command (Get_Range) = Get_Token Then
               Status.Set_Request (C_Stat, Status.GET,
                                   URI, HTTP_Version, Parameters);
               return True;
            else
               return False;
            end if;
         end Parse_Request_Line;

      begin
         if Command (Host_Range) = Host_Token then
            Status.Set_Host (C_Stat,
                             Command (Host_Token'Length + 1 .. Command'Last));

         elsif Parse_Request_Line (Command) then
            null;

         elsif Command (Connection_Range) = Connection_Token then
            Status.Set_Connection
              (C_Stat,
               Command (Connection_Token'Length + 1 .. Command'Last));

         end if;
      end Parse;

   begin
      select
         accept Start (FD : in Sockets.Socket_FD;
                       CB : in Response.Callback) do
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

               --  First let's pretend that there is no problem

               Sockets.Put_Line (Sock, Messages.Status_Line (Messages.S200));

               --  Get the message body from user's callback

               declare
                  Answer : constant AWS.Response.Data := Handler (C_Stat);
               begin

                  --  Now we output the message body length

                  Sockets.Put_Line (Sock,
                                    Messages.Content_Length
                                     (Response.Content_Length (Answer)));

                  --  We handle only text/html message type

                  Sockets.Put_Line (Sock,
                                    Messages.Content_Type
                                     (Response.Content_Type (Answer)));

                  --  End of header

                  Sockets.New_Line (Sock);

                  Sockets.Put_Line (Sock, Response.Message_Body (Answer));
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
