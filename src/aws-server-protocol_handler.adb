------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2003                          --
--                                ACT-Europe                                --
--                                                                          --
--  Authors: Dmitriy Anisimkov - Pascal Obry                                --
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

--  $RCSfile$
--  $Revision$
--  $Date$
--  $Author$

--  This procedure is responsible of handling the HTTP protocol. Every
--  responses and coming requests are parsed/formated here.

with Ada.Characters.Handling;
with Ada.Streams.Stream_IO;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Strings.Unbounded;

with AWS.Config;
with AWS.Headers.Values;
with AWS.Log;
with AWS.Messages;
with AWS.MIME;
with AWS.Net.Buffered;
with AWS.OS_Lib;
with AWS.Parameters.Set;
with AWS.Resources;
with AWS.Session;
with AWS.Server.Get_Status;
with AWS.Status.Set;
with AWS.Templates;
with AWS.Translator;
with AWS.Utils;
with AWS.URL;

separate (AWS.Server)

procedure Protocol_Handler
  (HTTP_Server : in out HTTP;
   Index       : in     Positive)
is

   use Ada;
   use Ada.Strings;
   use Ada.Strings.Unbounded;

   Case_Sensitive_Parameters : constant Boolean
     := CNF.Case_Sensitive_Parameters (HTTP_Server.Properties);

   Admin_URI      : constant String
     := CNF.Admin_URI (HTTP_Server.Properties);

   HTTP_10        : constant String := "HTTP/1.0";

   C_Stat         : AWS.Status.Data;     -- Connection status

   P_List         : AWS.Parameters.List; -- Form data

   Sock_Ptr       : constant Socket_Access
     := HTTP_Server.Slots.Get (Index => Index).Sock;

   Sock           : Net.Socket_Type'Class renames Sock_Ptr.all;

   Socket_Taken   : Boolean := False;
   --  Set to True if a socket has been reserved for a push session.

   Will_Close     : Boolean;
   --  Will_Close is set to true when the connection will be closed by the
   --  server. It means that the server is about to send the lastest message
   --  to the client using this sockets.

   --  Duplication of some status fields for faster access

   Status_Connection         : Unbounded_String;
   Status_Multipart_Boundary : Unbounded_String;
   Status_Content_Type       : Unbounded_String;

   procedure Send_Resource
     (Method : in     Status.Request_Method;
      File   : in out Resources.File_Type;
      Length : in out Response.Content_Length_Type);
   --  Send the last header line Transfer-Encoding and Content_Length if
   --  necessary and send the file content. Length is the size of the
   --  resource/file as known before the call, Length returned value is the
   --  actual number of bytes sent.

   procedure Answer_To_Client;
   --  This procedure use the C_Stat status data to build the correct answer
   --  to the client. If Force_Answer is not Empty it will be sent back to the
   --  client's browser, otherwise the answer will be retreived from user's
   --  callback.

   procedure Send (Answer : in Response.Data);
   --  Send Answer to the client's browser

   procedure Get_Message_Header;
   --  Parse HTTP message header. This procedure fill in the C_Stat status
   --  data.

   procedure Get_Message_Data;
   --  If the client sent us some data read them. Right now only the
   --  POST method is handled. This procedure fill in the C_Stat status
   --  data.

   procedure Parse_Request_Line (Command : in String);
   --  Parse the request line:
   --  Request-Line = Method SP Request-URI SP HTTP-Version CRLF

   function Is_Valid_HTTP_Date (HTTP_Date : in String) return Boolean;
   --  Check the date format as some Web brower seems to return invalid date
   --  field.

   ----------------------
   -- Answer_To_Client --
   ----------------------

   procedure Answer_To_Client is
      use type Messages.Status_Code;

      Answer : Response.Data;

      procedure Build_Answer;
      --  Build the Answer that should be sent to the client's browser

      procedure Create_Session;
      --  Create a session if needed

      procedure Answer_File (File_Name : in String);
      --  Assign File to Answer response data

      -----------------
      -- Answer_File --
      -----------------

      procedure Answer_File (File_Name : in String) is
      begin
         Answer := Response.File
           (Content_Type => MIME.Content_Type (File_Name),
            Filename     => File_Name);
      end Answer_File;

      ------------------
      -- Build_Answer --
      ------------------

      procedure Build_Answer is
         URL : constant AWS.URL.Object := AWS.Status.URI (C_Stat);
         URI : constant String         := AWS.URL.URL (URL);
      begin
         --  Check if the status page, status page logo or status page images
         --  are requested. These are AWS internal data that should not be
         --  handled by AWS users.

         --  AWS Internal status page handling.

         if Admin_URI'Length > 0
              and then
           URI'Length >= Admin_URI'Length
              and then
           URI (URI'First .. URI'First + Admin_URI'Length - 1) = Admin_URI
         then

            if URI = Admin_URI then

               --  Status page
               begin
                  Answer := Response.Build
                    (Content_Type => MIME.Text_HTML,
                     Message_Body => Get_Status (HTTP_Server));
               exception
                  when Templates.Template_Error =>
                     Answer := Response.Build
                       (Content_Type => MIME.Text_HTML,
                        Message_Body =>
                          "Status template error. Please check "
                          & "that '" & CNF.Status_Page (HTTP_Server.Properties)
                          & "' file is valid.");
               end;

            elsif URI = Admin_URI & "-logo" then
               --  Status page logo
               Answer_File (CNF.Logo_Image (HTTP_Server.Properties));

            elsif URI = Admin_URI & "-uparr" then
               --  Status page hotplug up-arrow
               Answer_File (CNF.Up_Image (HTTP_Server.Properties));

            elsif URI = Admin_URI & "-downarr" then
               --  Status page hotplug down-arrow
               Answer_File (CNF.Down_Image (HTTP_Server.Properties));

            elsif URI = Admin_URI & "-HPup" then
               --  Status page hotplug up message
               Hotplug.Move_Up
                 (HTTP_Server.Filters,
                  Positive'Value (AWS.Parameters.Get (P_List, "N")));
               Answer := Response.URL (Admin_URI);

            elsif URI = Admin_URI & "-HPdown" then
               --  Status page hotplug down message
               Hotplug.Move_Down
                 (HTTP_Server.Filters,
                  Positive'Value (AWS.Parameters.Get (P_List, "N")));
               Answer := Response.URL (Admin_URI);

            else
               Answer := Response.Build
                 (Content_Type => MIME.Text_HTML,
                  Message_Body =>
                    "Invalid use of reserved status URI prefix: " & Admin_URI);
            end if;

            --  End of Internal status page handling.

            --  Check if the URL is trying to reference resource above Web root
            --  directory.

         elsif CNF.Check_URL_Validity (HTTP_Server.Properties)
           and then not AWS.URL.Is_Valid (URL)
         then
            --  403 status code "Forbidden".

            Answer := Response.Build
              (Status_Code   => Messages.S403,
               Content_Type  => "text/plain",
               Message_Body  => "Request " & URI & ASCII.LF
                 & " trying to reach resource above the Web root directory.");

         else
            --  Otherwise, check if a session needs to be created

            Create_Session;

            --  and get answer from client callback

            declare
               Found : Boolean;
            begin
               HTTP_Server.Slots.Mark_Phase (Index, Server_Processing);

               --  Check the hotplug filters

               Hotplug.Apply (HTTP_Server.Filters, C_Stat, Found, Answer);

               --  If no one applied, run the user callback

               if not Found then
                  AWS.Status.Set.Socket (C_Stat, Sock_Ptr);

                  HTTP_Server.Dispatcher_Sem.Read;

                  --  Be sure to always release the read semaphore

                  begin
                     Answer := Dispatchers.Dispatch
                       (HTTP_Server.Dispatcher.all, C_Stat);

                     HTTP_Server.Dispatcher_Sem.Release_Read;
                  exception
                     when others =>
                        HTTP_Server.Dispatcher_Sem.Release_Read;
                        raise;
                  end;

               end if;

               HTTP_Server.Slots.Mark_Phase (Index, Server_Response);
            end;
         end if;
      end Build_Answer;

      --------------------
      -- Create_Session --
      --------------------

      procedure Create_Session is
         use type Session.ID;
      begin
         if CNF.Session (HTTP_Server.Properties)
           and then (AWS.Status.Session (C_Stat) = Session.No_Session
                     or else not Session.Exist (AWS.Status.Session (C_Stat)))
         then
            --  Generate the session ID
            AWS.Status.Set.Session (C_Stat);
         end if;
      end Create_Session;

   begin
      --  Set status peername

      AWS.Status.Set.Peername
        (C_Stat, HTTP_Server.Slots.Get_Peername (Index));

      Build_Answer;

      Send (Answer);
   end Answer_To_Client;

   ----------------------
   -- Get_Message_Data --
   ----------------------

   procedure Get_Message_Data is

      use type Status.Request_Method;

      Multipart_Boundary : constant String
        := To_String (Status_Multipart_Boundary);

      procedure File_Upload
        (Start_Boundary, End_Boundary : in String;
         Parse_Boundary               : in Boolean);
      --  Handle file upload data coming from the client browser.

      function Value_For (Name : in String; Into : in String) return String;
      --  Returns the value for the variable named "Name" into the string
      --  "Into". The data format is: name1="value2"; name2="value2"...

      -----------------
      -- File_Upload --
      -----------------

      procedure File_Upload
        (Start_Boundary, End_Boundary : in String;
         Parse_Boundary               : in Boolean)
      is
         Name            : Unbounded_String;
         Filename        : Unbounded_String;
         Server_Filename : Unbounded_String;
         Content_Type    : Unbounded_String;
         File            : Streams.Stream_IO.File_Type;
         Is_File_Upload  : Boolean;

         End_Found       : Boolean := False;
         --  Set to true when the end-boundary has been found.

         type Error_State is (No_Error, Name_Error, Device_Error);
         --  This state is to monitor the file upload process. If we receice
         --  Name_Error or Device_Error while writing data on disk we need to
         --  continue reading all data from the socket if we want to be able
         --  to send back an error message.

         Error : Error_State := No_Error;

         procedure Get_File_Data;
         --  Read file data from the stream, set End_Found if the end-boundary
         --  signature has been read.

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
            --  Returns True if we have reach the end of file data

            procedure Write (Buffer : in Streams.Stream_Element_Array);
            pragma Inline (Write);
            --  Write buffer to the file, handle the Device_Error exception

            Buffer : Streams.Stream_Element_Array (1 .. 4096);
            Index  : Streams.Stream_Element_Offset := Buffer'First;

            Data   : Streams.Stream_Element_Array (1 .. 1);

            ---------------
            -- Check_EOF --
            ---------------

            function Check_EOF return Boolean is

               Signature : constant Streams.Stream_Element_Array
                 := (1 => 13, 2 => 10)
                    & Translator.To_Stream_Element_Array (Start_Boundary);

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
                  if Error /= No_Error then
                     return;
                  end if;

                  if Get_File_Data.Buffer'Last
                    < Get_File_Data.Index + Index - 1
                  then
                     Write (Get_File_Data.Buffer
                              (Get_File_Data.Buffer'First
                                 .. Get_File_Data.Index - 1));
                     Get_File_Data.Index := Get_File_Data.Buffer'First;
                  end if;

                  Get_File_Data.Buffer
                    (Get_File_Data.Index .. Get_File_Data.Index + Index - 2)
                    := Buffer (Buffer'First .. Index - 1);

                  Get_File_Data.Index := Get_File_Data.Index + Index - 1;
               end Write_Data;

            begin
               Buffer (Index) := 13;
               Index := Index + 1;

               loop
                  Net.Buffered.Read (Sock, Data);

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

            -----------
            -- Write --
            -----------

            procedure Write (Buffer : in Streams.Stream_Element_Array) is
            begin
               if Error = No_Error then
                  Streams.Stream_IO.Write (File, Buffer);
               end if;
            exception
               when Text_IO.Device_Error =>
                  Error := Device_Error;
            end Write;

         begin
            begin
               Streams.Stream_IO.Create
                 (File,
                  Streams.Stream_IO.Out_File,
                  To_String (Server_Filename));
            exception
               when Text_IO.Name_Error =>
                  Error := Name_Error;
            end;

            Read_File : loop
               Net.Buffered.Read (Sock, Data);

               while Data (1) = 13 loop
                  exit Read_File when Check_EOF;
               end loop;

               Buffer (Index) := Data (1);
               Index := Index + 1;

               if Index > Buffer'Last then
                  Write (Buffer);
                  Index := Buffer'First;

                  HTTP_Server.Slots.Mark_Data_Time_Stamp
                    (Protocol_Handler.Index);
               end if;
            end loop Read_File;

            if Index /= Buffer'First then
               Write (Buffer (Buffer'First .. Index - 1));
            end if;

            if Error = No_Error then
               Streams.Stream_IO.Close (File);
            end if;

            --  Check for end-boundary, at this point we have at least two
            --  chars. Either the terminating "--" or CR+LF.

            Net.Buffered.Read (Sock, Data);
            Net.Buffered.Read (Sock, Data);

            if Data (1) = 10 then
               --  We have CR+LF, it is a start-boundary
               End_Found := False;

            else
               --  We have read the "--", read line terminator. This is the
               --  end-boundary.

               End_Found := True;
               Net.Buffered.Read (Sock, Data);
               Net.Buffered.Read (Sock, Data);
            end if;

            if Error = Name_Error then
               --  We can't create the file, add a clear exception message
               Exceptions.Raise_Exception
                 (Text_IO.Name_Error'Identity,
                  "Cannot create file " & To_String (Server_Filename));

            elsif Error = Device_Error then
               --  We can't write to the file, there is probably no space left
               --  on devide.
               Exceptions.Raise_Exception
                 (Text_IO.Device_Error'Identity,
                  "No space left on device while writting "
                    & To_String (Server_Filename));
            end if;
         end Get_File_Data;

         ---------------------
         -- Target_Filename --
         ---------------------

         function Target_Filename (Filename : in String) return String is
            I           : constant Natural
              := Fixed.Index (Filename, Maps.To_Set ("/\"),
                              Going => Strings.Backward);
            Upload_Path : constant String
              := CNF.Upload_Directory (HTTP_Server.Properties);

            UID         : Natural;
         begin
            File_Upload_UID.Get (UID);

            if I = 0 then
               return Upload_Path
                 & Utils.Image (UID) & '.'
                 & Filename;
            else
               return Upload_Path
                 & Utils.Image (UID) & '.'
                 & Filename (I + 1 .. Filename'Last);
            end if;
         end Target_Filename;

      begin
         --  Reach the boundary

         if Parse_Boundary then
            loop
               declare
                  Data : constant String := Net.Buffered.Get_Line (Sock);
               begin
                  exit when Data = Start_Boundary;

                  if Data = End_Boundary then
                     --  This is the end of the multipart data
                     return;
                  end if;
               end;
            end loop;
         end if;

         --  Read file upload parameters

         declare
            Data : constant String := Net.Buffered.Get_Line (Sock);
         begin
            Is_File_Upload := Fixed.Index (Data, "filename=") /= 0;

            Name     := To_Unbounded_String (Value_For ("name", Data));
            Filename := To_Unbounded_String (Value_For ("filename", Data));
         end;

         --  Reach the data

         loop
            declare
               Data : constant String := Net.Buffered.Get_Line (Sock);
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

         --  Read file/field data

         if Is_File_Upload then
            --  This part of the multipart message contains file data.

            --  Set Server_Filename, the name of the file in the local file
            --  sytstem.

            Server_Filename := To_Unbounded_String
              (Target_Filename (To_String (Filename)));

            if To_String (Filename) /= "" then
               --  First value is the uniq name on the server side

               AWS.Parameters.Set.Add
                 (P_List, To_String (Name), To_String (Server_Filename));

               --  Second value is the original name as found on the client
               --  side.

               AWS.Parameters.Set.Add
                 (P_List, To_String (Name), To_String (Filename));

               --  Read file data, set End_Found if the end-boundary signature
               --  has been read.

               Get_File_Data;

               if not End_Found then
                  File_Upload (Start_Boundary, End_Boundary, False);
               end if;

            else
               --  There is no file for this multipart, user did not enter
               --  something in the field.

               File_Upload (Start_Boundary, End_Boundary, True);
            end if;

         else
            --  This part of the multipart message contains field value

            declare
               Value : Unbounded_String;
            begin
               loop
                  declare
                     L : constant String := Net.Buffered.Get_Line (Sock);
                  begin
                     End_Found := (L = End_Boundary);

                     exit when End_Found or else L = Start_Boundary;

                     --  Append this line to the value

                     if Value /= Null_Unbounded_String then
                        Append (Value, ASCII.CR & ASCII.LF);
                     end if;
                     Append (Value, L);
                  end;
               end loop;

               AWS.Parameters.Set.Add
                 (P_List, To_String (Name), To_String (Value));
            end;

            if not End_Found then
               File_Upload (Start_Boundary, End_Boundary, False);
            end if;
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
            return Into
              (Start .. Fixed.Index (Into (Start .. Into'Last), """") - 1);
         end if;
      end Value_For;

   begin
      --  Is there something to read ?

      if Status.Content_Length (C_Stat) /= 0 then

         if Status.Method (C_Stat) = Status.POST
           and then Status_Content_Type = MIME.Appl_Form_Data
         then
            --  Read data from the stream and convert it to a string as
            --  these are a POST form parameters.
            --  The body has the format: name1=value1;name2=value2...

            declare
               use Streams;

               Data : Stream_Element_Array
                 (1 .. Stream_Element_Offset (Status.Content_Length (C_Stat)));
            begin
               Net.Buffered.Read (Sock, Data);

               AWS.Status.Set.Binary (C_Stat, Data);
               --  We record the message body as-is to be able to send it back
               --  to an hotplug module if needed.

               --  We then decode it and add the parameters read in the
               --  message body.

               AWS.Parameters.Set.Add (P_List, Translator.To_String (Data));
            end;

         elsif Status.Method (C_Stat) = Status.POST
           and then Status_Content_Type = MIME.Multipart_Form_Data
         then
            --  This is a file upload

            File_Upload ("--" & Multipart_Boundary,
                         "--" & Multipart_Boundary & "--",
                         True);

         elsif Status.Method (C_Stat) = Status.POST
           and then Status.Is_SOAP (C_Stat)
         then
            --  This is a SOAP request, read and set the Payload XML message

            declare
               use Streams;

               Data : Stream_Element_Array
                 (1 .. Stream_Element_Offset (Status.Content_Length (C_Stat)));
            begin
               Net.Buffered.Read (Sock, Data);

               AWS.Status.Set.Payload (C_Stat, Translator.To_String (Data));
            end;

         else
            --  Let's suppose for now that all others content type data are
            --  binary data.

            declare
               use Streams;

               Data : Stream_Element_Array
                 (1 .. Stream_Element_Offset (Status.Content_Length (C_Stat)));
            begin
               Net.Buffered.Read (Sock, Data);
               AWS.Status.Set.Binary (C_Stat, Data);
            end;

         end if;
      end if;
   end Get_Message_Data;

   ------------------------
   -- Get_Message_Header --
   ------------------------

   procedure Get_Message_Header is
   begin
      --  Get and parse request line

      declare
         Data : constant String := Net.Buffered.Get_Line (Sock);
      begin
         HTTP_Server.Slots.Mark_Phase (Index, Client_Header);
         Parse_Request_Line (Data);
      end;

      Status.Set.Read_Header (Socket => Sock, D => C_Stat);

      Status_Connection := To_Unbounded_String (Status.Connection (C_Stat));

      --  Get necessary data from header for the reading HTTP body.

      declare

         procedure Named_Value
           (Name, Value : in String;
            Quit        : in out Boolean);
         --  Looking for the Boundary value in the
         --  Content-Type header line.

         procedure Value (Item : in String; Quit : in out Boolean);
         --  Reading the first unnamed value into the Status_Content_Type
         --  variable from the Content-Type header line.

         -----------------
         -- Named_Value --
         -----------------

         procedure Named_Value
           (Name, Value : in String;
            Quit        : in out Boolean) is
         begin
            if Ada.Characters.Handling.To_Lower (Name) = "boundary" then
               Status_Multipart_Boundary := To_Unbounded_String (Value);
               Quit := True;
            end if;
         end Named_Value;

         -----------
         -- Value --
         -----------

         procedure Value (Item : in String; Quit : in out Boolean) is
         begin
            if Status_Content_Type /= Null_Unbounded_String then
               --  Only first unnamed value is the Content_Type.

               Quit := True;

            elsif Item'Length > 0 then
               Status_Content_Type := To_Unbounded_String (Item);
            end if;
         end Value;

         procedure Parse is new Headers.Values.Parse (Value, Named_Value);

      begin
         --  Clear Content-Type status as this could have already been set in
         --  previous request.
         Status_Content_Type := Null_Unbounded_String;

         Parse (Status.Content_Type (C_Stat));
      end;
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

   ------------------------
   -- Parse_Request_Line --
   ------------------------

   procedure Parse_Request_Line (Command : in String) is

      I1, I2 : Natural;
      --  Index of first space and second space

      I3 : Natural;
      --  Index of ? if present in the URI (means that there is some
      --  parameters)

      procedure Cut_Command;
      --  Parse Command and set I1, I2 and I3

      function Resource return String;
      pragma Inline (Resource);
      --  Returns first parameter. parameters are separated by spaces.

      function Parameters return String;
      --  Returns parameters if some where specified in the URI.

      function HTTP_Version return String;
      pragma Inline (HTTP_Version);
      --  Returns second parameter. parameters are separated by spaces.

      -----------------
      -- Cut_Command --
      -----------------

      procedure Cut_Command is
      begin
         I1  := Fixed.Index (Command, " ");
         I2  := Fixed.Index (Command (I1 + 1 .. Command'Last), " ", Backward);

         I3  := Fixed.Index (Command (I1 + 1 .. I2 - 1), "?");

         if I3 = 0 then
            --  Could be encoded ?

            I3  := Fixed.Index (Command (I1 + 1 .. I2 - 1), "%3f");

            if I3 = 0 then
               I3  := Fixed.Index (Command (I1 + 1 .. I2 - 1), "%3F");
            end if;
         end if;
      end Cut_Command;

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
            if Command (I3) = '%' then
               return Command (I3 + 3 .. I2 - 1);
            else
               return Command (I3 + 1 .. I2 - 1);
            end if;
         end if;
      end Parameters;

      ---------
      -- URI --
      ---------

      function Resource return String is
      begin
         if I3 = 0 then
            return URL.Decode (Command (I1 + 1 .. I2 - 1));
         else
            return URL.Decode (Command (I1 + 1 .. I3 - 1));
         end if;
      end Resource;

   begin
      Cut_Command;

      --  GET and HEAD can have a set of parameters (query) attached. This is
      --  not really standard see [RFC 2616 - 13.9] but is widely used now.
      --
      --  POST parameters are passed into the message body, we do not allow
      --  parameters here even is this could be possible but as of today this
      --  feature is not used and it is not clear if it is permitted or
      --  prohibited by reading RFC 2616.

      if Messages.Match (Command, Messages.Get_Token) then
         Status.Set.Request (C_Stat, Status.GET, Resource, HTTP_Version);
         AWS.Parameters.Set.Add (P_List, Parameters);

      elsif Messages.Match (Command, Messages.Head_Token) then
         Status.Set.Request (C_Stat, Status.HEAD, Resource, HTTP_Version);
         AWS.Parameters.Set.Add (P_List, Parameters);

      elsif Messages.Match (Command, Messages.Post_Token) then
         Status.Set.Request (C_Stat, Status.POST, Resource, HTTP_Version);

      end if;
   end Parse_Request_Line;

   ----------
   -- Send --
   ----------

   procedure Send (Answer : in Response.Data) is

      use type Response.Data_Mode;

      Status : Messages.Status_Code;

      Length : Response.Content_Length_Type := 0;

      procedure Send_General_Header;
      --  Send the "Date:", "Server:", "Set-Cookie:" and "Connection:" header

      procedure Send_Header_Only;
      --  Send HTTP message header only. This is used to implement the HEAD
      --  request.

      procedure Send_Data;
      --  Send a text/binary data to the client

      ---------------
      -- Send_Data --
      ---------------

      procedure Send_Data is
         use type Calendar.Time;
         use type AWS.Status.Request_Method;

         Filename      : constant String
           := Response.Filename (Answer);

         File_Size     : constant Response.Content_Length_Type
           := Response.Content_Length (Answer);

         Is_Up_To_Date : Boolean;

         File_Mode     : constant Boolean
           := Response.Mode (Answer) = Response.File;

         File          : Resources.File_Type;
      begin
         Is_Up_To_Date := File_Mode
              and then
           Is_Valid_HTTP_Date (AWS.Status.If_Modified_Since (C_Stat))
              and then
           Resources.File_Timestamp (Filename)
             = Messages.To_Time (AWS.Status.If_Modified_Since (C_Stat));
         --  Equal used here see [RFC 2616 - 14.25]

         if Is_Up_To_Date then
            --  [RFC 2616 - 10.3.5]
            Net.Buffered.Put_Line
              (Sock,
               Messages.Status_Line (Messages.S304));

            Send_General_Header;
            Net.Buffered.New_Line (Sock);
            return;
         else
            Net.Buffered.Put_Line (Sock, Messages.Status_Line (Status));
         end if;

         --  Checking if we have to close connection because of undefined
         --  message length comming from a user's stream.

         if Response.Content_Length (Answer) = Response.Undefined_Length
            and then AWS.Status.HTTP_Version (C_Stat) = HTTP_10
            --  We cannot use transfer-encoding chunked in HTTP_10
            and then AWS.Status.Method (C_Stat) /= AWS.Status.HEAD
            --  We have to send message_body
         then
            --  In this case we need to close the connection explicitly at the
            --  end of the transfer.
            Will_Close := True;
         end if;

         Send_General_Header;

         --  Send file last-modified timestamp info in case of a file

         if File_Mode then
            Net.Buffered.Put_Line
              (Sock,
               Messages.Last_Modified (Resources.File_Timestamp (Filename)));
         end if;

         --  Note that we cannot send the Content_Length header at this
         --  point. A server should not send Content_Length if the
         --  transfer-coding used is not identity. This is allowed by the
         --  RFC but it seems that some implementation does not handle this
         --  right. The file can be sent using either identity or chunked
         --  transfer-coding. The proper header will be sent in Send_Resource
         --  see [RFC 2616 - 4.4].

         --  Send message body

         Response.Create_Resource (File, Answer);

         --  Length is the real resource/file size

         Length := File_Size;

         Send_Resource (AWS.Status.Method (C_Stat), File, Length);
      end Send_Data;

      -------------------------
      -- Send_General_Header --
      -------------------------

      procedure Send_General_Header is
         use type Messages.Cache_Option;
      begin
         --  Session

         if CNF.Session (HTTP_Server.Properties)
           and then AWS.Status.Session_Created (C_Stat)
         then
            --  This is an HTTP connection with session but there is no session
            --  ID set yet. So, send cookie to client browser.

            Net.Buffered.Put_Line
              (Sock,
               "Set-Cookie: AWS="
               & Session.Image (AWS.Status.Session (C_Stat)) & "; path=/");
         end if;

         --  Date

         Net.Buffered.Put_Line
           (Sock,
            "Date: " & Messages.To_HTTP_Date (OS_Lib.GMT_Clock));

         --  Server

         Net.Buffered.Put_Line
           (Sock,
            "Server: AWS (Ada Web Server) v" & Version);

         if Will_Close then
            --  We have decided to close connection after answering the client
            Net.Buffered.Put_Line (Sock, Messages.Connection ("close"));
         else
            Net.Buffered.Put_Line (Sock, Messages.Connection ("keep-alive"));
         end if;

         --  Send Content-Type, Cache-Control, Location, WWW-Authenticate
         --  and others user defined header lines.

         Response.Send_Header (Socket => Sock, D => Answer);

      end Send_General_Header;

      ----------------------
      -- Send_Header_Only --
      ----------------------

      procedure Send_Header_Only is
         use type AWS.Status.Request_Method;
      begin
         --  First let's output the status line

         Net.Buffered.Put_Line (Sock, Messages.Status_Line (Status));

         Send_General_Header;

         --  There is no content

         Net.Buffered.Put_Line (Sock, Messages.Content_Length (0));

         --  End of header

         Net.Buffered.New_Line (Sock);
      end Send_Header_Only;

      use type Response.Data;

   begin
      Status := Response.Status_Code (Answer);

      case Response.Mode (Answer) is

         when Response.File | Response.Stream | Response.Message =>
            Send_Data;

         when Response.Header =>
            Send_Header_Only;

         when Response.Socket_Taken =>
            HTTP_Server.Slots.Socket_Taken (Index);
            Socket_Taken := True;

         when Response.No_Data =>
            null;

      end case;

      Net.Buffered.Flush (Sock);

      AWS.Log.Write (HTTP_Server.Log, C_Stat, Status, Length);
   end Send;

   -------------------
   -- Send_Resource --
   -------------------

   procedure Send_Resource
     (Method : in     Status.Request_Method;
      File   : in out Resources.File_Type;
      Length : in out Response.Content_Length_Type)
   is
      use type Status.Request_Method;
      use type Streams.Stream_Element_Offset;

      Buffer_Size : constant := 4 * 1_024;
      --  Size of the buffer used to send the file.

      Chunk_Size  : constant := 1_024;
      --  Size of the buffer used to send the file with the chunked encoding.
      --  This is the maximum size of each chunk.

      procedure Send_File;
      --  Send file in one part

      procedure Send_File_Chunked;
      --  Send file in chunks, used in HTTP/1.1 and when the message length
      --  is not known)

      Last : Streams.Stream_Element_Offset;

      ---------------
      -- Send_File --
      ---------------

      procedure Send_File is

         use type Ada.Streams.Stream_Element_Offset;

         Buffer : Streams.Stream_Element_Array (1 .. Buffer_Size);

      begin
         loop
            Resources.Read (File, Buffer, Last);

            exit when Last < Buffer'First;

            Net.Buffered.Write (Sock, Buffer (1 .. Last));

            Length := Length + Positive (Last);

            HTTP_Server.Slots.Mark_Data_Time_Stamp (Index);
         end loop;
      end Send_File;

      ---------------------
      -- Send_File_Chunk --
      ---------------------

      procedure Send_File_Chunked is
         use type Streams.Stream_Element_Array;
         --  Note that we do not use a buffered socket here. Opera on SSL
         --  sockets does not like chunk that are not sent in a whole.

         Buffer : Streams.Stream_Element_Array (1 .. Chunk_Size);
         --  Each chunk will have a maximum length of Buffer'Length

         CRLF : constant Streams.Stream_Element_Array
           := (1 => Character'Pos (ASCII.CR), 2 => Character'Pos (ASCII.LF));

         Last_Chunk : constant Streams.Stream_Element_Array
           := Character'Pos ('0') & CRLF & CRLF;
         --  Last chunk for a chunked encoding stream. See [RFC 2616 - 3.6.1]

      begin
         Send_Chunks : loop
            Resources.Read (File, Buffer, Last);

            if Last = 0 then
               --  There is not more data to read, the previous chunk was the
               --  last one, just terminate the chunk message here.
               Net.Send (Sock, Last_Chunk);
               exit Send_Chunks;
            end if;

            Length := Length + Positive (Last);

            HTTP_Server.Slots.Mark_Data_Time_Stamp (Index);

            declare
               H_Last : constant String := Utils.Hex (Positive (Last));

               Chunk  : constant Streams.Stream_Element_Array
                 := Translator.To_Stream_Element_Array (H_Last)
                 & CRLF
                 & Buffer (1 .. Last)
                 & CRLF;
               --  A chunk is composed of:
               --     the Size of the chunk in hexadecimal
               --     a line feed
               --     the chunk
               --     a line feed

            begin
               --  Check if the last data portion.

               if Last < Buffer'Last then
                  --  No more data, add the terminating chunk
                  Net.Send (Sock, Chunk & Last_Chunk);
                  exit Send_Chunks;
               else
                  Net.Send (Sock, Chunk);
               end if;
            end;
         end loop Send_Chunks;
      end Send_File_Chunked;

   begin
      if Status.HTTP_Version (C_Stat) = HTTP_10
        or else Length /= Response.Undefined_Length
      then
         --  If content length is undefined and we handle an HTTP/1.0 protocol
         --  then the end of the stream will be determined by closing the
         --  connection. [RFC 1945 - 7.2.2] See the Will_Close local variable.

         if Length /= Response.Undefined_Length then
            Net.Buffered.Put_Line (Sock, Messages.Content_Length (Length));
         end if;

         --  Terminate header

         Net.Buffered.New_Line (Sock);

         if Method /= Status.HEAD and then Length /= 0 then
            Length := 0;
            Send_File;
         end if;

      else
         --  HTTP/1.1 case and we do not know the message lenght.
         --
         --  Terminate header, do not send the Content_Length see
         --  [RFC 2616 - 4.4]. It could be possible to send the Content_Length
         --  as this is cleary a permission but it does not work in some
         --  obsucre cases.

         Net.Buffered.Put_Line (Sock, Messages.Transfer_Encoding ("chunked"));
         Net.Buffered.New_Line (Sock);
         Net.Buffered.Flush (Sock);

         --  Past this point we will not use the buffered mode. Opera on SSL
         --  sockets does not like chunk that are not sent in a whole.

         if Method /= Status.HEAD then
            Length := 0;
            Send_File_Chunked;
         end if;
      end if;

      Resources.Close (File);

   exception
      when Text_IO.Name_Error =>
         raise;

      when others =>
         Resources.Close (File);
         raise;
   end Send_Resource;

begin
   --  This new connection has been initialized because some data are
   --  beeing sent. We are by default using HTTP/1.1 persistent
   --  connection. We will exit this loop only if the client request
   --  so or if we time-out on waiting for a request.

   For_Every_Request : loop

      HTTP_Server.Slots.Mark_Phase (Index, Wait_For_Client);

      Status.Set.Reset (C_Stat);

      P_List := Status.Parameters (C_Stat);

      Parameters.Set.Case_Sensitive
        (P_List, Case_Sensitive_Parameters);

      HTTP_Server.Slots.Increment_Slot_Activity_Counter (Index);

      Get_Message_Header;

      HTTP_Server.Slots.Mark_Phase (Index, Client_Data);

      Get_Message_Data;

      declare
         Connection : constant String := To_String (Status_Connection);
      begin
         --  Connection, check connection string with Match to skip connection
         --  options [RFC 2616 - 14.10].
         Will_Close := AWS.Messages.Match (Connection, "close")
           or else HTTP_Server.Slots.N = 1
           or else (Status.HTTP_Version (C_Stat) = HTTP_10
                      and then
                      AWS.Messages.Does_Not_Match (Connection, "keep-alive"));
      end;

      Status.Set.Keep_Alive (C_Stat, not Will_Close);

      Status.Set.Parameters (C_Stat, P_List);

      HTTP_Server.Slots.Mark_Phase (Index, Server_Response);

      Answer_To_Client;

      --  Exit if connection has not the Keep-Alive status or we are working
      --  on HTTP/1.0 protocol or we have a single slot.

      exit For_Every_Request when Will_Close or else Socket_Taken;

   end loop For_Every_Request;

   --  Release memory for local objects

   Status.Set.Free (C_Stat);

exception
   --  We must never exit from the outer loop as a Line task is
   --  supposed to live forever.
   --  We have here a pool of Line and each line is recycled when
   --  needed.

   when Net.Socket_Error =>
      null;

   when E : others =>

      declare
         use type Response.Data_Mode;

         Answer : Response.Data := Response.Empty;
      begin
         HTTP_Server.Exception_Handler (E, False, Answer);

         if Response.Mode (Answer) /= Response.No_Data then
            HTTP_Server.Slots.Mark_Phase (Index, Server_Response);
            Send (Answer);
         end if;
      exception
         when others =>
            null;
      end;

      Status.Set.Free (C_Stat);
end Protocol_Handler;
