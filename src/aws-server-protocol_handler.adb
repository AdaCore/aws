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

with Ada.Streams.Stream_IO;
with Ada.Integer_Text_IO;
with Ada.Streams.Stream_IO;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Interfaces.C;

with AWS.Log;
with AWS.Messages;
with AWS.OS_Lib;
with AWS.Session;
with AWS.Status;
with AWS.Server.Config;
with AWS.Server.Get_Status;
with AWS.Utils;

separate (AWS.Server)

procedure Protocol_Handler
  (Sock        : in     Sockets.Socket_FD'Class;
   HTTP_Server : in out HTTP;
   Index       : in     Positive)
is

   use Ada;
   use Ada.Strings;

   Admin_URI      : constant String := To_String (HTTP_Server.Admin_URI);
   End_Of_Message : constant String := "";
   HTTP_10        : constant String := "HTTP/1.0";

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

   procedure Send_File_Time (Sock     : in Sockets.Socket_FD'Class;
                             Filename : in String);
   --  Send Last-Modified: header

   procedure Send_File_Size (Sock     : in Sockets.Socket_FD'Class;
                             Filename : in String);
   --  Send Content-Length: header

   function Is_Valid_HTTP_Date (HTTP_Date : in String) return Boolean;
   --  Check the date format as some Web brower seems to return invalid date
   --  field.

   ----------------------
   -- Answer_To_Client --
   ----------------------

   procedure Answer_To_Client is

      use type Messages.Status_Code;
      use type Response.Data_Mode;

      Answer : Response.Data;

      Status : Messages.Status_Code;

      Send_Session_Cookie : Boolean := False;
      --  will be set to True if a session Cookie must be sent in the header.

      procedure Create_Session;
      --  create a session if needed

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

      --------------------
      -- Create_Session --
      --------------------

      procedure Create_Session is
      begin
         if HTTP_Server.Session
           and then (AWS.Status.Session (C_Stat) = ""
                     or else not Session.Exist (AWS.Status.Session (C_Stat)))
         then
            declare
               Cookie : constant String := Session.Image (Session.Create);
            begin
               AWS.Status.Set_Session (C_Stat, Cookie);
               Send_Session_Cookie := True;
            end;
         end if;
      end Create_Session;

      ----------------------
      -- Header_Date_Serv --
      ----------------------

      procedure Header_Date_Serv is
      begin
         --  This is an HTTP connection with session but there is no session
         --  ID set yet.

         if HTTP_Server.Session and then Send_Session_Cookie then
            --  send cookie to client browser
            Sockets.Put_Line
              (Sock,
               "Set-Cookie: AWS=" & AWS.Status.Session (C_Stat));
         end if;

         Sockets.Put_Line
           (Sock,
            "Date: " & Messages.To_HTTP_Date (OS_Lib.GMT_Clock));

         Sockets.Put_Line (Sock,
                           "Server: AWS (Ada Web Server) v" & Version);
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
            Is_Valid_HTTP_Date (AWS.Status.If_Modified_Since (C_Stat))
            and then
           OS_Lib.File_Timestamp (Response.Message_Body (Answer))
            <= Messages.To_Time (AWS.Status.If_Modified_Since (C_Stat)));

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

         Sockets.Put_Line
           (Sock, Messages.Content_Type (Response.Content_Type (Answer)));

         --  send message body only if needed

         if AWS.Status.Method (C_Stat) = AWS.Status.HEAD then
            --  Send file info and terminate header

            Send_File_Time (Sock, Response.Message_Body (Answer));
            Send_File_Size (Sock, Response.Message_Body (Answer));
            Sockets.New_Line (Sock);

         else
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

         if Status = Messages.S301 then
            Sockets.Put_Line
              (Sock,
               Messages.Location (Response.Location (Answer)));
         end if;

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

      URI : constant String := AWS.Status.URI (C_Stat);

   begin
      --  Check if the status page, status page logo or status page images are
      --  requested. These are AWS internal data that should not be handled by
      --  AWS users.

      if URI = Admin_URI then
         --  status page
         Answer := Response.Build
           (Content_Type => "text/html",
            Message_Body => Get_Status (HTTP_Server));

      elsif URI = Admin_URI & "-logo" then
         --  status page logo
         Answer := Response.File
           (Content_Type => "image/gif",
            Filename     => "logo.gif");

      elsif URI = Admin_URI & "-uparr" then
         --  status page hotplug up-arrow
         Answer := Response.File
           (Content_Type => "image/gif",
            Filename     => "up.gif");

      elsif URI = Admin_URI & "-downarr" then
         --  status page hotplug down-arrow
         Answer := Response.File
           (Content_Type => "image/gif",
            Filename     => "down.gif");

      elsif URI = Admin_URI & "-HPup" then
         --  status page hotplug up message
         Hotplug.Move_Up
           (HTTP_Server.Filters,
            Positive'Value (AWS.Status.Parameter (C_Stat, "N")));
         Answer := Response.Moved (Admin_URI);

      elsif URI = Admin_URI & "-HPdown" then
         --  status page hotplug down message
         Hotplug.Move_Down
           (HTTP_Server.Filters,
            Positive'Value (AWS.Status.Parameter (C_Stat, "N")));
         Answer := Response.Moved (Admin_URI);

      else
         --  otherwise, check if a session needs to be created

         Create_Session;

         --  and get answer from client callback

         declare
            Found : Boolean;
         begin
            --  check the hotplug filters

            Hotplug.Apply (HTTP_Server.Filters,
                           AWS.Status.URI (C_Stat), Found, Answer);

            --  if no one applied, run the default callback
            if not Found then
               Answer := HTTP_Server.CB (C_Stat);
            end if;
         end;
      end if;

      Status := Response.Status_Code (Answer);

      Log.Write (C_Stat, Status, HTTP_Server.Slots.Get_Peername (Index));

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

   ----------------------
   -- Get_Message_Data --
   ----------------------

   procedure Get_Message_Data is

      use type Status.Request_Method;

      procedure File_Upload (Start_Boundary, End_Boundary : in String;
                             Parse_Boundary               : in Boolean);
      --  handle file upload data coming from the client browser.

      function Value_For (Name : in String; Into : in String) return String;
      --  Returns the value for the variable named "Name" into the string
      --  "Into". The data format is: name1="value2"; name2="value2"...

      -----------------
      -- File_Upload --
      -----------------

      procedure File_Upload (Start_Boundary, End_Boundary : in String;
                             Parse_Boundary               : in Boolean)
      is
         --  ??? Implementation would be more efficient if the input socket
         --  stream was cached. Here the socket is read char by char.

         Name           : Unbounded_String;
         Filename       : Unbounded_String;
         Content_Type   : Unbounded_String;
         File           : Streams.Stream_IO.File_Type;
         Is_File_Upload : Boolean;

         procedure Get_File_Data;
         --  read file data from the stream.

         function Target_Filename (Filename : in String) return String;
         --  Returns the full path name for the file as stored on the
         --  server side.

         --------------
         -- Get_Data --
         --------------

         procedure Get_File_Data is

            use type Streams.Stream_Element;
            use type Streams.Stream_Element_Offset;
            use type Streams.Stream_Element_Array;

            function Check_EOF return Boolean;
            --  Returns True if we have reach the end of file data.

            function End_Boundary_Signature
              return Streams.Stream_Element_Array;
            --  Returns the end signature string as a element array.

            Buffer : Streams.Stream_Element_Array (1 .. 4096);
            Index  : Streams.Stream_Element_Offset := Buffer'First;

            Data   : Streams.Stream_Element_Array (1 .. 1);

            ---------------
            -- Check_EOF --
            ---------------

            function Check_EOF return Boolean is
               Signature : Streams.Stream_Element_Array :=
                 (1 => 13, 2 => 10) & End_Boundary_Signature;

               Buffer : Streams.Stream_Element_Array (1 .. Signature'Length);
               Index  : Streams.Stream_Element_Offset := Buffer'First;

               procedure Write_Data;
               --  Put buffer data into the main buffer (Get_Data.Buffer). If
               --  the main buffer is not big enough, it will write the buffer
               --  into the file bdefore.

               ----------------
               -- Write_Data --
               ----------------

               procedure Write_Data is
               begin
                  if Get_File_Data.Buffer'Last
                    < Get_File_Data.Index + Index - 1
                  then
                     Streams.Stream_IO.Write
                       (File, Get_File_Data.Buffer
                        (Get_File_Data.Buffer'First
                         .. Get_File_Data.Index - 1));
                     Get_File_Data.Index := Get_File_Data.Buffer'First;
                  end if;

                  Get_File_Data.Buffer (Get_File_Data.Index
                                        .. Get_File_Data.Index + Index - 2)
                    := Buffer (Buffer'First .. Index - 1);
                  Get_File_Data.Index := Get_File_Data.Index + Index - 1;
               end Write_Data;

            begin
               Buffer (Index) := 13;
               Index := Index + 1;

               loop
                  Sockets.Receive (Sock, Data);

                  if Data (1) = 13 then
                     Write_Data;
                     return False;
                  end if;

                  Buffer (Index) := Data (1);

                  if Index = Buffer'Last then
                     if Buffer = Signature then
                        return True;
                     else
                        Write_Data;
                        return False;
                     end if;
                  end if;

                  Index := Index + 1;

               end loop;
            end Check_EOF;

            ----------------------------
            -- End_Boundary_Signature --
            ----------------------------

            function End_Boundary_Signature
              return Streams.Stream_Element_Array
            is
               use Streams;
               End_Signature    : constant String := Start_Boundary;
               Stream_Signature : Stream_Element_Array
                 (Stream_Element_Offset (End_Signature'First)
                  .. Stream_Element_Offset (End_Signature'Last));
            begin
               for K in End_Signature'Range loop
                  Stream_Signature (Stream_Element_Offset (K))
                    := Stream_Element (Character'Pos (End_Signature (K)));
               end loop;
               return Stream_Signature;
            end End_Boundary_Signature;

         begin
            Streams.Stream_IO.Create (File,
                                      Streams.Stream_IO.Out_File,
                                      Target_Filename (To_String (Filename)));

            Read_File : loop
               Sockets.Receive (Sock, Data);

               while Data (1) = 13 loop
                  exit Read_File when Check_EOF;
               end loop;

               Buffer (Index) := Data (1);
               Index := Index + 1;

               if Index > Buffer'Last then
                  Streams.Stream_IO.Write (File, Buffer);
                  Index := Buffer'First;
               end if;
            end loop Read_File;

            if not (Index = Buffer'First) then
               Streams.Stream_IO.Write
                 (File, Buffer (Buffer'First .. Index - 1));
            end if;

            Streams.Stream_IO.Close (File);
         end Get_File_Data;

         ---------------------
         -- Target_Filename --
         ---------------------

         function Target_Filename (Filename : in String) return String is
            I   : Natural := Fixed.Index (Filename,
                                          Maps.To_Set ("/\"),
                                          Going => Strings.Backward);
            UID : Natural;
         begin
            File_Upload_UID.Get (UID);
            if I = 0 then
               return Server.Config.Upload_Directory (HTTP_Server)
                 & Utils.Image (UID) & '_'
                 & Filename;
            else
               return Server.Config.Upload_Directory (HTTP_Server)
                 & Utils.Image (UID) & '_'
                 & Filename (I + 1 .. Filename'Last);
            end if;
         end Target_Filename;

      begin
         --  reach the boundary

         if Parse_Boundary then
            loop
               declare
                  Data : constant String := Sockets.Get_Line (Sock);
               begin
                  exit when Data = Start_Boundary;

                  if Data = End_Boundary then
                     --  this is the end of the multipart data
                     return;
                  end if;
               end;
            end loop;
         end if;

         --  read file upload parameters

         declare
            Data : constant String := Sockets.Get_Line (Sock);
         begin
            Is_File_Upload := Fixed.Index (Data, "filename=") /= 0;

            if not Parse_Boundary then
               if Data = "--" then
                  --  check if this is the end of the finish boundary string.
                  return;
               else
                  --  data should be CR+LF here
                  declare
                     Data : constant String := Sockets.Get_Line (Sock);
                  begin
                     Name
                       := To_Unbounded_String (Value_For ("name", Data));
                     Filename
                       := To_Unbounded_String (Value_For ("filename", Data));
                  end;
               end if;
            else
               Name     := To_Unbounded_String (Value_For ("name", Data));
               Filename := To_Unbounded_String (Value_For ("filename", Data));
            end if;
         end;

         --  reach the data

         loop
            declare
               Data : constant String := Sockets.Get_Line (Sock);
            begin
               if Data = "" then
                  exit;
               else
                  Content_Type := To_Unbounded_String
                    (Data
                     (Messages.Content_Type_Token'Length + 1 .. Data'Last));
               end if;
            end;
         end loop;

         --  read file/field data

         if Is_File_Upload then
            --  this part of the multipart message contains file data.

            if To_String (Filename) /= "" then
               AWS.Status.Set_Parameters
                 (C_Stat,
                  To_String (Name), Target_Filename (To_String (Filename)));

               Get_File_Data;

               File_Upload ("--" & Status.Multipart_Boundary (C_Stat),
                            "--" & Status.Multipart_Boundary (C_Stat) & "--",
                            False);
            else
               --  there is no file for this multipart, user did not enter
               --  something in the field.

               File_Upload ("--" & Status.Multipart_Boundary (C_Stat),
                            "--" & Status.Multipart_Boundary (C_Stat) & "--",
                            True);
            end if;

         else
            --  this part of the multipart message contains field value.

            declare
               Value : constant String := Sockets.Get_Line (Sock);
            begin
               AWS.Status.Set_Parameters (C_Stat, To_String (Name), Value);
            end;

            File_Upload ("--" & Status.Multipart_Boundary (C_Stat),
                         "--" & Status.Multipart_Boundary (C_Stat) & "--",
                         True);
         end if;

      end File_Upload;

      ---------------
      -- Value_For --
      ---------------

      function Value_For (Name : in String; Into : in String) return String is
         Pos   : constant Natural := Fixed.Index (Into, Name & '=');
         Start : constant Natural := Pos + Name'Length + 2;
      begin
         if Pos = 0 then
            return "";
         else
            return Into (Start
                         .. Fixed.Index (Into (Start .. Into'Last), """") - 1);
         end if;
      end Value_For;

   begin
      --  is there something to read ?

      if Status.Content_Length (C_Stat) /= 0 then

         if Status.Method (C_Stat) = Status.POST
           and then Status.Content_Type (C_Stat) = Messages.Form_Data

         then
            --  read data from the stream and convert it to a string as
            --  these are a POST form parameters.
            --  The body as the format: name1=value1;name2=value2...

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

               Status.Set_Parameters (C_Stat, Char_Data);
            end;

         elsif Status.Method (C_Stat) = Status.POST
           and then Status.Content_Type (C_Stat) = Messages.Multipart_Form_Data
         then
            --  this is a file upload.

            File_Upload ("--" & Status.Multipart_Boundary (C_Stat),
                         "--" & Status.Multipart_Boundary (C_Stat) & "--",
                         True);

         else
            --  let's suppose for now that all others content type data are
            --  binary data.

            begin
               declare
                  Data : constant Streams.Stream_Element_Array
                    := Sockets.Receive (Sock);
               begin
                  Status.Set_Parameters (C_Stat, Data);
               end;

            exception
               when others =>
                  raise Connection_Error;
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
            HTTP_Server.Slots.Set_Abortable (Index, True);

            declare
               Data : constant String := Sockets.Get_Line (Sock);
            begin
               --  a request by the client has been received, do not abort
               --  until this request is handled.

               HTTP_Server.Slots.Set_Abortable (Index, False);

               exit when Data = End_Of_Message;

               Parse (Data);
            end;

            HTTP_Server.Slots.Mark_Activity_Time (Index);

         exception
            when others =>
               --  here we time-out on Sockets.Get_Line
               raise Sockets.Connection_Closed;
         end;
      end loop;
   end Get_Message_Header;

   ------------------------
   -- Is_Valid_HTTP_Date --
   ------------------------

   function Is_Valid_HTTP_Date (HTTP_Date : in String) return Boolean is
      Mask   : constant String := "Aaa, 99 Aaa 9999 99:99:99 GMT";
      Offset : constant Integer := HTTP_Date'First - 1;
      --  Make sure the function works for inputs with 'First <> 1
      Result : Boolean := True;
   begin
      for I in Mask'Range loop
         Result := I + Offset in HTTP_Date'Range;

         exit when not Result;

         case Mask (I) is
            when 'A' =>
               Result := HTTP_Date (I + Offset) in 'A' .. 'Z';

            when 'a' =>
               Result := HTTP_Date (I + Offset) in 'a' .. 'z';

            when '9' =>
               Result := HTTP_Date (I + Offset) in '0' .. '9';

            when others =>
               Result := Mask (I) = HTTP_Date (I + Offset);
         end case;
      end loop;

      return Result;
   end Is_Valid_HTTP_Date;

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
         declare
            Pos : constant Natural := Fixed.Index (Command, ";");
         begin
            if Pos = 0 then
               Status.Set_Content_Type
                 (C_Stat,
                  Command
                  (Messages.Content_Type_Token'Length + 1 .. Command'Last));
            else
               Status.Set_Content_Type
                 (C_Stat,
                  Command
                  (Messages.Content_Type_Token'Length + 1 .. Pos - 1));
               Status.Set_Multipart_Boundary
                 (C_Stat,
                  Command (Pos + 11 .. Command'Last));
            end if;
         end;

      elsif Messages.Is_Match
        (Command, Messages.If_Modified_Since_Token)
      then
         Status.Set_If_Modified_Since
           (C_Stat,
            Command (Messages.If_Modified_Since_Token'Length + 1
                     .. Command'Last));

      elsif Messages.Is_Match
        (Command, Messages.Authorization_Token)
      then
         Status.Set_Authorization
           (C_Stat,
            Command (Messages.Authorization_Token'Length + 1 .. Command'Last));

      elsif Messages.Is_Match (Command, Messages.Cookie_Token)
        and then Command (Messages.Cookie_Token'Length + 1
                          .. Messages.Cookie_Token'Length + 3) = "AWS"
      then
         --  the expected Cookie line is:
         --  Cookie: AWS=<cookieID>

         Status.Set_Session
           (C_Stat,
            Command (Messages.Cookie_Token'Length + 4 .. Command'Last));
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

         use type Ada.Streams.Stream_Element_Offset;

         Buffer : Streams.Stream_Element_Array (1 .. 4_096);

      begin
         --  terminate header

         Send_File_Size (Sock, Filename);
         Sockets.New_Line (Sock);

         --  send file content

         loop
            Streams.Stream_IO.Read (File, Buffer, Last);
            exit when Last <= 0;
            Sockets.Send (Sock, Buffer (1 .. Last));
         end loop;

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
      Streams.Stream_IO.Open (File, Streams.Stream_IO.In_File,
                              Filename, "shared=no");

      Send_File_Time (Sock, Filename);

      if HTTP_Version = HTTP_10 then
         Send_File;
      else
         --  Always use chunked transfer encoding method for HTTP/1.1 even if
         --  it also support standard method.
         --  ??? it could be better to use the standard method for small file
         --  (could be faster).

         Send_File_Chunked;
      end if;

      Streams.Stream_IO.Close (File);

   exception
      when others =>
         Streams.Stream_IO.Close (File);
         raise;
   end Send_File;

   --------------------
   -- Send_File_Time --
   --------------------

   procedure Send_File_Time (Sock     : in Sockets.Socket_FD'Class;
                             Filename : in String) is
   begin
      Sockets.Put_Line
        (Sock,
         "Last-Modified: " &
         Messages.To_HTTP_Date (OS_Lib.File_Timestamp (Filename)));
   end Send_File_Time;

   --------------------
   -- Send_File_Size --
   --------------------

   procedure Send_File_Size (Sock     : in Sockets.Socket_FD'Class;
                             Filename : in String) is
   begin
      Sockets.Put_Line
        (Sock, "Content-Length:"
         & Streams.Stream_Element_Offset'Image (OS_Lib.File_Size (Filename)));
   end Send_File_Size;

begin
   C_Stat := Status.No_Data;

   --  this new connection has been initialized because some data are
   --  beeing sent. We are by default using HTTP/1.1 persistent
   --  connection. We will exit this loop only if the client request
   --  so or if we time-out on waiting for a request.

   For_Every_Request : loop

      Get_Message_Header;

      Get_Message_Data;

      Answer_To_Client;

      --  exit if connection has not the Keep-Alive status or we are working
      --  on HTTP/1.0 protocol or we have a single slot.

      exit when Status.Connection (C_Stat) /= "Keep-Alive"
        or else Status.HTTP_Version (C_Stat) = HTTP_10
        or else HTTP_Server.Slots.N = 1;

      Status.Reset (C_Stat);

   end loop For_Every_Request;

end Protocol_Handler;
