------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                            Copyright (C) 2000                            --
--                      Dmitriy Anisimkov & Pascal Obry                     --
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

with Ada.Integer_Text_IO;
with Ada.Streams.Stream_IO;
with Ada.Strings.Fixed;

with POSIX;
with POSIX_File_Status;
with POSIX_Calendar;

with AWS.Messages;
with AWS.Status;
with AWS.Translater;

separate (AWS.Server)

procedure Protocol_Handler (Sock    : in Sockets.Socket_FD'Class;
                            Handler : in Response.Callback;
                            Slots   : in Slots_Access;
                            Index   : in Positive)
is

   use Ada;
   use Ada.Strings;

   End_Of_Message : constant String := "";

   HTTP_10 : constant String := "HTTP/1.0";

   C_Stat  : AWS.Status.Data;         --  connection status

   procedure Parse (Command : in String);
   --  parse a line sent by the client and do what is needed

   procedure Send_File (Filename          : in String;
                        HTTP_Version      : in String);
   --  send content of filename as chunk data

   procedure Answer_To_Client;
   --  This procedure use the C_Stat status data to send the correct answer
   --  to the client.

   procedure Get_Message_Header;
   --  parse HTTP message header. This procedure fill in the C_Stat status
   --  data.

   procedure Get_Message_Data;
   --  If the client sent us some data read them. Right now only the
   --  POST method is handled. This procedure fill in the C_Stat status
   --  data.

   function File_Timestamp (Filename : in String)
                           return Calendar.Time;
   --  returns the last modification time stamp for filename.

   ----------------------
   -- Answer_To_Client --
   ----------------------

   procedure Answer_To_Client is
      use type Messages.Status_Code;
      use type Response.Data_Mode;

      Answer : constant Response.Data := Handler (C_Stat);

      Status : constant Messages.Status_Code :=
        Response.Status_Code (Answer);

      procedure Header_Date_Serv;
      --  send the Date: and Server: data

      procedure Send_Connection;
      --  send the Connection: data

      procedure Send_Header;
      --  send HTTP message header.

      procedure Send_File;
      --  send a binary file to the client

      procedure Send_Message;
      --  answer by a text or HTML message.

      ----------------------
      -- Header_Date_Serv --
      ----------------------

      procedure Header_Date_Serv is
      begin
         Sockets.Put_Line (Sock,
                           "Date: "
                           & Messages.To_HTTP_Date (Calendar.Clock));

         Sockets.Put_Line (Sock,
                           "Server: AWS (Ada Web Server) v"
                           & Version);
      end Header_Date_Serv;

      ---------------------
      -- Send_Connection --
      ---------------------

      procedure Send_Connection is
      begin
         if AWS.Status.Connection (C_Stat) = "" then
            Sockets.Put_Line (Sock, Messages.Connection_Token & "close");
         else
            Sockets.Put_Line
              (Sock,
               Messages.Connection (AWS.Status.Connection (C_Stat)));
         end if;
      end Send_Connection;

      ---------------
      -- Send_File --
      ---------------

      procedure Send_File is
         use type Calendar.Time;
         use type AWS.Status.Request_Method;
      begin
         AWS.Status.Set_File_Up_To_Date
           (C_Stat,
            AWS.Status.If_Modified_Since (C_Stat) /= ""
            and then
           File_Timestamp (Response.Message_Body (Answer))
            >= Messages.To_Time (AWS.Status.If_Modified_Since (C_Stat)));

         if AWS.Status.File_Up_To_Date (C_Stat) then
            Sockets.Put_Line (Sock,
                              Messages.Status_Line (Messages.S304));
            Sockets.New_Line (Sock);
            return;
         else
            Sockets.Put_Line (Sock, Messages.Status_Line (Status));
         end if;

         Header_Date_Serv;

         Send_Connection;

         Sockets.Put_Line (Sock,
                           Messages.Content_Type
                           (Response.Content_Type (Answer)));

         --  send message body only if needed

         if AWS.Status.Method (C_Stat) /= AWS.Status.HEAD then
            Send_File (Response.Message_Body (Answer),
                       AWS.Status.HTTP_Version (C_Stat));
         end if;

      end Send_File;

      -----------------
      -- Send_Header --
      -----------------

      procedure Send_Header is
         use type AWS.Status.Request_Method;
      begin
         --  First let's output the status line

         Sockets.Put_Line (Sock, Messages.Status_Line (Status));

         Header_Date_Serv;

         --  There is no content

         Sockets.Put_Line (Sock, Messages.Content_Length (0));

         --  the message content type

         if Status = Messages.S401 then
            Sockets.Put_Line
              (Sock,
               Messages.Www_Authenticate (Response.Realm (Answer)));
         end if;

         --  End of header

         Sockets.New_Line (Sock);
      end Send_Header;

      ------------------
      -- Send_Message --
      ------------------

      procedure Send_Message is
         use type AWS.Status.Request_Method;
      begin
         --  First let's output the status line

         Sockets.Put_Line (Sock, Messages.Status_Line (Status));

         Header_Date_Serv;

         --  Now we output the message body length

         Sockets.Put_Line
           (Sock,
            Messages.Content_Length (Response.Content_Length (Answer)));

         --  the message content type

         Sockets.Put_Line
           (Sock,
            Messages.Content_Type (Response.Content_Type (Answer)));

         if Status = Messages.S401 then
            Sockets.Put_Line
              (Sock,
               Messages.Www_Authenticate (Response.Realm (Answer)));
         end if;

         --  End of header

         Sockets.New_Line (Sock);

         --  send message body only if needed

         if AWS.Status.Method (C_Stat) /= AWS.Status.HEAD then
            Sockets.Put_Line (Sock, Response.Message_Body (Answer));
         end if;
      end Send_Message;

   begin
      if Response.Mode (Answer) = Response.Message then
         Send_Message;

      elsif Response.Mode (Answer) = Response.File then
         Send_File;

      elsif Response.Mode (Answer) = Response.Header then
         Send_Header;

      else
         raise Constraint_Error;
      end if;
   end Answer_To_Client;

   --------------------
   -- File_Timestamp --
   --------------------

   function File_Timestamp (Filename : in String)
                           return Calendar.Time is
   begin
      return POSIX_Calendar.To_Time
        (POSIX_File_Status.Last_Modification_Time_Of
         (POSIX_File_Status.Get_File_Status
          (POSIX.To_POSIX_String (Filename))));
   end File_Timestamp;

   ----------------------
   -- Get_Message_Data --
   ----------------------

   procedure Get_Message_Data is
      use type Status.Request_Method;

   begin
      --  is there something to read ?

      if Status.Content_Length (C_Stat) /= 0 then

         if Status.Method (C_Stat) = Status.POST
           and then Status.Content_Type (C_Stat) = Messages.Form_Data

         then
            --  read data from the stream and convert it to a string as
            --  these are a POST form parameters

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

         else
            --  let's suppose for now that all others content type data are
            --  binary data.

            declare
               Data : constant Streams.Stream_Element_Array
                 := Sockets.Receive (Sock);
            begin
               Status.Set_Parameters (C_Stat, Data);
            end;

         end if;
      end if;
   end Get_Message_Data;

   ------------------------
   -- Get_Message_Header --
   ------------------------

   procedure Get_Message_Header is
   begin
      loop
         begin
            declare
               Data : constant String := Sockets.Get_Line (Sock);
            begin
               --  a request by the client has been received, do not abort
               --  until this request is handled.

               Slots.Set_Abortable (Index, False);

               exit when Data = End_Of_Message;

               Parse (Data);
            end;

            Slots.Mark_Activity_Time (Index);

         exception
            when Constraint_Error =>
               --  here we time-out on Sockets.Get_Line
               raise Sockets.Connection_Closed;
         end;
      end loop;
   end Get_Message_Header;

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
            return Translater.Decode_URL (Command (I3 + 1 .. I2 - 1));
         end if;
      end Parameters;

      ------------------------
      -- Parse_Request_Line --
      ------------------------

      function Parse_Request_Line (Command : in String) return Boolean is
      begin
         Cut_Command;

         if Messages.Is_Match (Command, Messages.Get_Token) then
            Status.Set_Request (C_Stat, Status.GET,
                                URI, HTTP_Version, Parameters);
            return True;

         elsif Messages.Is_Match (Command, Messages.Head_Token) then
            Status.Set_Request (C_Stat, Status.HEAD,
                                URI, HTTP_Version, "");
            return True;

         elsif Messages.Is_Match (Command, Messages.Post_Token) then
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

      elsif Messages.Is_Match (Command, Messages.Host_Token) then
         Status.Set_Host
           (C_Stat,
            Command (Messages.Host_Token'Length + 1 .. Command'Last));

      elsif Messages.Is_Match (Command, Messages.Connection_Token) then
         Status.Set_Connection
           (C_Stat,
            Command (Messages.Connection_Token'Length + 1 .. Command'Last));

      elsif Messages.Is_Match (Command, Messages.Content_Length_Token) then
         Status.Set_Content_Length
           (C_Stat,
            Natural'Value
            (Command (Messages.Content_Length_Token'Length + 1
                      .. Command'Last)));

      elsif Messages.Is_Match (Command, Messages.Content_Type_Token) then
         Status.Set_Content_Type
           (C_Stat,
            Command
            (Messages.Content_Type_Token'Length + 1 .. Command'Last));

      elsif Messages.Is_Match
        (Command, Messages.If_Modified_Since_Token)
      then
         Status.Set_If_Modified_Since
           (C_Stat,
            Command (Messages.If_Modified_Since_Token'Length + 1
                     .. Command'Last));

      end if;
   exception
      when others =>
         raise Internal_Error;
   end Parse;

   ---------------
   -- Send_File --
   ---------------

   procedure Send_File (Filename          : in String;
                        HTTP_Version      : in String)
   is

      procedure Send_File;
      --  send file in one part

      procedure Send_File_Chunked;
      --  send file in chunk (HTTP/1.1 only)

      File : Streams.Stream_IO.File_Type;
      Last : Streams.Stream_Element_Offset;

      ---------------
      -- Send_File --
      ---------------

      procedure Send_File is

         use POSIX_File_Status;

         File_Size : Streams.Stream_Element_Offset :=
           Streams.Stream_Element_Offset
           (Size_Of (Get_File_Status (POSIX.To_POSIX_String (Filename))));

         Buffer : Streams.Stream_Element_Array (1 .. File_Size);

      begin

         Streams.Stream_IO.Read (File, Buffer, Last);

         --  terminate header

         Sockets.Put_Line (Sock, "Content-Length:"
                           & Natural'Image (Natural (File_Size)));
         Sockets.New_Line (Sock);

         --  send file content

         Sockets.Send (Sock, Buffer (1 .. Last));
         Sockets.New_Line (Sock);
      end Send_File;

      ---------------------
      -- Send_File_Chunk --
      ---------------------

      procedure Send_File_Chunked is

         function Hex (V : in Natural) return String;
         --  returns the hexadecimal string representation of the decimal
         --  number V.

         Buffer : Streams.Stream_Element_Array (1 .. 1_024);

         function Hex (V : in Natural) return String is
            Hex_V : String (1 .. 8);
         begin
            Integer_Text_IO.Put (Hex_V, V, 16);
            return Hex_V (Fixed.Index (Hex_V, "#") + 1 ..
                          Fixed.Index (Hex_V, "#", Strings.Backward) - 1);
         end Hex;

      begin
         --  terminate header

         Sockets.Put_Line (Sock, "Transfer-Encoding: chunked");
         Sockets.New_Line (Sock);

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
      end Send_File_Chunked;

   begin
      Streams.Stream_IO.Open (File, Streams.Stream_IO.In_File, Filename);

      Sockets.Put_Line
        (Sock,
         "Last-Modified: " &
         Messages.To_HTTP_Date (File_Timestamp (Filename)));

      if HTTP_Version = HTTP_10 then
         Send_File;
      else
         Send_File_Chunked;
      end if;

      Streams.Stream_IO.Close (File);
   end Send_File;

begin

   C_Stat := Status.No_Data;

   --  this new connection has been initialized because some data are
   --  beeing sent. We are by default using HTTP/1.1 persistent
   --  connection. We will exit this loop only if the client request
   --  so or if we time-out on waiting for a request.

   For_Every_Request : loop

      Slots.Set_Abortable (Index, True);

      Get_Message_Header;

      Get_Message_Data;

      Answer_To_Client;

      --  exit if connection has not the Keep-Alive status or we are working
      --  on HTTP/1.0 protocol or we have a single slot.

      exit when Status.Connection (C_Stat) /= "Keep-Alive"
        or else Status.HTTP_Version (C_Stat) = HTTP_10
        or else Slots.N = 1;

   end loop For_Every_Request;

end Protocol_Handler;
