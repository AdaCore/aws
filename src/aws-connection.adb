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
with Ada.Integer_Text_IO;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Streams.Stream_IO;

with AWS.Messages;
with AWS.Status;
with AWS.Translater;

package body AWS.Connection is

   use Ada;
   use Ada.Strings;
   use Ada.Strings.Unbounded;

   End_Of_Message : constant String := "";

   Host_Token : constant String := "Host: ";
   subtype Host_Range is Positive range Host_Token'Range;

   Get_Token : constant String := "GET ";
   subtype Get_Range is Positive range Get_Token'Range;

   Post_Token : constant String := "POST ";
   subtype Post_Range is Positive range Post_Token'Range;

   Connection_Token : constant String := "Connection: ";
   subtype Connection_Range is Positive range Connection_Token'Range;

   Content_Length_Token : constant String := "Content-Length: ";
   subtype Content_Length_Range is Positive range Content_Length_Token'Range;

   ----------
   -- Line --
   ----------

   task body Line is

      Sock    : Sockets.Socket_FD;
      Handler : Response.Callback;
      C_Stat  : Status.Data;         --  connection status

      procedure Parse (Command : in String);
      --  parse a line sent by the client and do what is needed

      procedure Send_File (Filename : in String);
      --  send content of filename as chunk data

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

         function Is_Match (Str, Pattern : in String) return Boolean;
         pragma Inline (Is_Match);
         --  returns True if Pattern matches the begining of Str.

         -----------------
         -- Cut_Command --
         -----------------

         procedure Cut_Command is
         begin
            I1 := Fixed.Index (Command, " ");
            I2 := Fixed.Index (Command (I1 + 1 .. Command'Last), " ");
            I3 := Fixed.Index (Command (I1 + 1 .. I2), "?");
         end Cut_Command;

         --------------
         -- Is_Match --
         --------------

         function Is_Match (Str, Pattern : in String) return Boolean is
         begin
            return Pattern'Length <= Str'Length
              and then Str (1 .. Pattern'Length) = Pattern;
         end Is_Match;

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
               return Translater.Decode_URL (Command (I3 + 1 .. I2 - 1));
            end if;
         end Parameters;

         ------------------------
         -- Parse_Request_Line --
         ------------------------

         function Parse_Request_Line (Command : in String) return Boolean is
         begin
            Cut_Command;

            if Is_Match (Command, Get_Token) then
               Status.Set_Request (C_Stat, Status.GET,
                                   URI, HTTP_Version, Parameters);
               return True;

            elsif Is_Match (Command, Post_Token) then
               Status.Set_Request (C_Stat, Status.POST,
                                   URI, HTTP_Version, "");
               return True;

            else
               return False;
            end if;
         end Parse_Request_Line;

      begin
         if Parse_Request_Line (Command) then
            null;

         elsif Is_Match (Command, Host_Token) then
            Status.Set_Host (C_Stat,
                             Command (Host_Token'Length + 1 .. Command'Last));

         elsif Is_Match (Command, Connection_Token) then
            Status.Set_Connection
              (C_Stat,
               Command (Connection_Token'Length + 1 .. Command'Last));

         elsif Is_Match (Command, Content_Length_Token) then
            Status.Set_Content_Length
              (C_Stat,
               Natural'Value
               (Command (Content_Length_Token'Length + 1 .. Command'Last)));

         end if;
      end Parse;

      ---------------
      -- Send_File --
      ---------------

      procedure Send_File (Filename : in String) is

         function Hex (V : in Natural) return String;
         --  returns the hexadecimal string representation of the decimal
         --  number V.

         File   : Streams.Stream_IO.File_Type;
         Buffer : Streams.Stream_Element_Array (1 .. 256);
         Last   : Streams.Stream_Element_Offset;

         function Hex (V : in Natural) return String is
            Hex_V : String (1 .. 8);
         begin
            Integer_Text_IO.Put (Hex_V, V, 16);
            return Hex_V (Fixed.Index (Hex_V, "#") + 1 ..
                          Fixed.Index (Hex_V, "#", Strings.Backward) - 1);
         end Hex;

      begin
         Streams.Stream_IO.Open (File, Streams.Stream_IO.In_File, Filename);

         loop
            Streams.Stream_IO.Read (File, Buffer, Last);

            exit when Integer (Last) = 0;

            Sockets.Put_Line (Sock, Hex (Natural (Last)));

            Sockets.Send (Sock, Buffer (1 .. Last));
            Sockets.New_Line (Sock);
         end loop;

         --  last chunk
         Sockets.Put_Line (Sock, "0");
         Sockets.New_Line (Sock);
      end Send_File;

      use type Status.Request_Method;

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

      --  this new connection has been initialized because some data are
      --  beeing sent. read the header now.

      Text_IO.Put_Line ("New connection...");

      loop

         Text_IO.Put_Line ("*** Read request...");

         Get_Message_Header : loop
            declare
               Data : constant String := Sockets.Get_Line (Sock);
            begin
               exit when Data = End_Of_Message;

               Text_IO.Put_Line ("H=" & Data);
               Parse (Data);
            end;
         end loop Get_Message_Header;

         Text_IO.New_Line;
         Text_IO.Put_Line ("*** Ok let me answer to that...");

         if Status.Method (C_Stat) = Status.POST
           and then Status.Content_Length (C_Stat) /= 0
         then

            Text_IO.Put_Line ("*** Now read POST data...");

            declare
               Data : constant Streams.Stream_Element_Array
                 := Sockets.Receive (Sock);
               Char_Data : String (1 .. Data'Length);
               CDI       : Positive := 1;
            begin
               CDI := 1;
               for K in Data'Range loop
                  Char_Data (CDI) := Character'Val (Data (K));
                  CDI := CDI + 1;
               end loop;
               Status.Set_Parameters (C_Stat,
                                      Translater.Decode_URL (Char_Data));
            end;

         end if;

         --  Get the message body from user's callback

         declare
            use type Messages.Status_Code;
            use type Response.Data_Mode;

            Answer : constant Response.Data := Handler (C_Stat);

            Status : constant Messages.Status_Code :=
              Response.Status_Code (Answer);
         begin

            --  First let's output the status line

            Sockets.Put_Line (Sock, Messages.Status_Line (Status));

            Sockets.Put_Line (Sock,
                              "Date: Sun, 15 Jan 2000 22:00:00 GMT");

            Sockets.Put_Line (Sock,
                              "Server: AWS (Ada Web Server) v"
                              & Version);

            if Response.Mode (Answer) = Response.Message then

               --  Now we output the message body length

               Sockets.Put_Line (Sock,
                                 Messages.Content_Length
                                 (Response.Content_Length (Answer)));

               --  We handle only text/html message type

               Sockets.Put_Line (Sock,
                                 Messages.Content_Type
                                 (Response.Content_Type (Answer)));

               if Status = Messages.S401 then
                  Sockets.Put_Line (Sock,
                                    "Www-Authenticate: Basic realm="""
                                    & Response.Realm (Answer)
                                    & """");
               end if;

               --  End of header

               Sockets.New_Line (Sock);

               Sockets.Put_Line (Sock, Response.Message_Body (Answer));

            elsif Response.Mode (Answer) = Response.File then

               --  we are sending binary data as chunk

               Sockets.Put_Line (Sock, "Transfer-Encoding: chunked");

               Sockets.Put_Line (Sock,
                                 Messages.Content_Type
                                 (Response.Content_Type (Answer)));

               Sockets.New_Line (Sock);

               Send_File (Response.Message_Body (Answer));

            else
               raise Constraint_Error;
            end if;

         end;

         exit when Status.Connection (C_Stat) /= "Keep-Alive";

      end loop;

      Sockets.Shutdown (Sock);

   exception
      when Sockets.Connection_Closed =>
         Text_IO.Put_Line ("Connection closed");
         Sockets.Shutdown (Sock, Sockets.Both);
      when E : others =>
         Text_IO.Put_Line ("Connection error...");
         Sockets.Shutdown (Sock, Sockets.Both);
   end Line;

end AWS.Connection;
