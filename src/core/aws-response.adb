------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2015, AdaCore                     --
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

pragma Ada_2012;

with Ada.Directories;
with Ada.Strings.Fixed;

with AWS.Headers.Values;
with AWS.Resources.Streams.Disk.Once;
with AWS.Resources.Streams.Memory;
with AWS.Response.Set;
with AWS.Translator;

package body AWS.Response is

   package RSM renames AWS.Resources.Streams.Memory;

   -----------------
   -- Acknowledge --
   -----------------

   function Acknowledge
     (Status_Code  : Messages.Status_Code;
      Message_Body : String := "";
      Content_Type : String := MIME.Text_HTML) return Data
   is
      Result : Data;
   begin
      Set.Status_Code (Result, Status_Code);

      if Message_Body = "" then
         Set.Mode (Result, Header);
      else
         Set.Message_Body (Result, Message_Body);
         Set.Content_Type (Result, Content_Type);
      end if;

      return Result;
   end Acknowledge;

   ------------
   -- Adjust --
   ------------

   overriding procedure Adjust (Object : in out Data) is
   begin
      Object.Ref_Counter.Counter := Object.Ref_Counter.Counter + 1;
   end Adjust;

   ------------------
   -- Authenticate --
   ------------------

   function Authenticate
     (Realm   : String;
      Mode    : Authentication_Mode := Basic;
      Stale   : Boolean             := False;
      Message : String              := Default_Authenticate_Message)
      return Data
   is
      Result : Data;
   begin
      Set.Authentication (Result, Realm, Mode, Stale);
      Set.Content_Type   (Result, AWS.MIME.Text_HTML);
      Set.Message_Body   (Result, Message);
      return Result;
   end Authenticate;

   --------------------
   -- Authentication --
   --------------------

   function Authentication (D : Data) return Authentication_Mode is
      use AWS.Headers;
      Auth_Values : constant VString_Array :=
                      Get_Values (D.Header, Messages.WWW_Authenticate_Token);
   begin
      if Auth_Values'Length = 1 then
         return Authentication_Mode'Value
            (Values.Get_Unnamed_Value (To_String (Auth_Values (1)), 1));
      else
         return Any;
      end if;
   end Authentication;

   --------------------------
   -- Authentication_Stale --
   --------------------------

   function Authentication_Stale (D : Data) return Boolean is
      use AWS.Headers;
      Auth_Values : constant VString_Array :=
                      Get_Values (D.Header, Messages.WWW_Authenticate_Token);
   begin
      for J in Auth_Values'Range loop
         declare
            Stale_Image : constant String :=
                            Values.Search
                              (To_String (Auth_Values (J)), "stale", False);
         begin
            if Stale_Image /= "" then
               return Boolean'Value (Stale_Image);
            end if;
         end;
      end loop;
      return False;
   end Authentication_Stale;

   -----------
   -- Build --
   -----------

   function Build
     (Content_Type  : String;
      Message_Body  : String;
      Status_Code   : Messages.Status_Code      := Messages.S200;
      Cache_Control : Messages.Cache_Option     := Messages.Unspecified;
      Encoding      : Messages.Content_Encoding := Messages.Identity)
      return Data
   is
      Result : Data;
   begin
      Set.Status_Code   (Result, Status_Code);
      Set.Content_Type  (Result, Content_Type);
      Set.Data_Encoding (Result, Encoding);
      Set.Message_Body  (Result, Message_Body);
      Set.Cache_Control (Result, Cache_Control);
      return Result;
   end Build;

   function Build
     (Content_Type    : String;
      UString_Message : Strings.Unbounded.Unbounded_String;
      Status_Code     : Messages.Status_Code      := Messages.S200;
      Cache_Control   : Messages.Cache_Option     := Messages.Unspecified;
      Encoding        : Messages.Content_Encoding := Messages.Identity)
      return Data
   is
      Result : Data;
   begin
      Set.Status_Code   (Result, Status_Code);
      Set.Content_Type  (Result, Content_Type);
      Set.Data_Encoding (Result, Encoding);
      Set.Message_Body  (Result, UString_Message);
      Set.Cache_Control (Result, Cache_Control);
      return Result;
   end Build;

   function Build
     (Content_Type  : String;
      Message_Body  : Streams.Stream_Element_Array;
      Status_Code   : Messages.Status_Code      := Messages.S200;
      Cache_Control : Messages.Cache_Option     := Messages.Unspecified;
      Encoding      : Messages.Content_Encoding := Messages.Identity)
      return Data
   is
      Result : Data;
   begin
      Set.Status_Code   (Result, Status_Code);
      Set.Content_Type  (Result, Content_Type);
      Set.Data_Encoding (Result, Encoding);
      Set.Message_Body  (Result, Message_Body);
      Set.Cache_Control (Result, Cache_Control);
      return Result;
   end Build;

   -------------------
   -- Cache_Control --
   -------------------

   function Cache_Control (D : Data) return Messages.Cache_Option is
   begin
      return Messages.Cache_Option
        (Headers.Get (D.Header, Messages.Cache_Control_Token));
   end Cache_Control;

   function Cache_Control (D : Data) return Messages.Cache_Data is
   begin
      return Messages.To_Cache_Data (Messages.Response, Cache_Control (D));
   end Cache_Control;

   --------------------
   -- Close_Resource --
   --------------------

   function Close_Resource (D : Data) return Boolean is
   begin
      return D.Close_Stream;
   end Close_Resource;

   --------------------
   -- Content_Length --
   --------------------

   function Content_Length (D : Data) return Content_Length_Type is
      CL_Image : constant String :=
                   Headers.Get (D.Header, Messages.Content_Length_Token);
   begin
      if CL_Image = "" then
         return Undefined_Length;
      else
         return Content_Length_Type'Value (CL_Image);
      end if;
   end Content_Length;

   ------------------
   -- Content_Type --
   ------------------

   function Content_Type (D : Data) return String is
   begin
      return To_String (D.Content_Type);
   end Content_Type;

   ----------------------
   -- Create_Resource --
   ----------------------

   procedure Create_Resource
     (D    : in out Data;
      File : out AWS.Resources.File_Type;
      GZip : Boolean)
   is
      use AWS.Resources;

      GZip_Out : Boolean := GZip;
   begin
      case D.Mode is
         when Response.File =>
            Open (File, Filename (D), "shared=no", GZip_Out);

            if GZip_Out then
               Set.Update_Header (D, Messages.Content_Encoding_Token, "gzip");
            end if;

         when Response.File_Once =>
            declare
               Stream : AWS.Resources.Streams.Stream_Access;
            begin
               Stream := new AWS.Resources.Streams.Disk.Once.Stream_Type;
               AWS.Resources.Streams.Disk.Once.Open
                 (AWS.Resources.Streams.Disk.Once.Stream_Type (Stream.all),
                  Filename (D), "shared=no");
               AWS.Resources.Streams.Create (File, Stream);
            end;

         when Response.Stream | Response.Message =>
            Resources.Streams.Create (File, D.Stream);

            D.Ref_Counter.Stream_Taken := True;
         when others =>
            --  Should not be called for others response modes
            raise Constraint_Error;
      end case;
   end Create_Resource;

   -----------
   -- Empty --
   -----------

   function Empty return Data is
      Result : Data;
   begin
      Set.Status_Code (Result, Messages.S204);
      return Result;
   end Empty;

   -------------
   -- Expires --
   -------------

   function Expires (D : Data) return Calendar.Time is
   begin
      return Messages.To_Time (Headers.Get (D.Header, Messages.Expires_Token));
   end Expires;

   ----------
   -- File --
   ----------

   function File
     (Content_Type  : String;
      Filename      : String;
      Status_Code   : Messages.Status_Code      := Messages.S200;
      Cache_Control : Messages.Cache_Option     := Messages.Unspecified;
      Encoding      : Messages.Content_Encoding := Messages.Identity;
      Once          : Boolean                   := False;
      Disposition   : Disposition_Mode          := None;
      User_Filename : String                    := "") return Data
   is
      function CD_Filename return String is
        (if User_Filename = ""
         then Directories.Simple_Name (Filename)
         else User_Filename) with Inline;
      --  Returns the Content-Disposition filename

      Result : Data;
   begin
      Set.Status_Code   (Result, Status_Code);
      Set.Content_Type  (Result, Content_Type);
      Set.Filename      (Result, Filename);
      Set.Cache_Control (Result, Cache_Control);

      if Resources.Is_Regular_File (Filename) then
         --  Set the Content_Disposition to properly pass the filename to
         --  the browser.

         case Disposition is
            when Attachment =>
               Set.Add_Header
                 (Result,
                  Messages.Content_Disposition_Token,
                  "attachment; filename=""" & CD_Filename & '"');

            when Inline =>
               Set.Add_Header
                 (Result,
                  Messages.Content_Disposition_Token,
                  "inline; filename=""" & CD_Filename & '"');

            when None =>
               null;
         end case;

         if Once then
            Set.Mode (Result, File_Once);
         else
            Set.Mode (Result, File);
         end if;

         case Encoding is
            when Messages.GZip     =>
               Response.Set.Add_Header
                 (Result, Messages.Content_Encoding_Token, "gzip");

            when Messages.Deflate  =>
               Response.Set.Add_Header
                 (Result, Messages.Content_Encoding_Token, "deflate");

            when Messages.Identity =>
               null;
         end case;
      end if;

      return Result;
   end File;

   --------------
   -- Filename --
   --------------

   function Filename (D : Data) return String is
   begin
      return To_String (D.Filename);
   end Filename;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Object : in out Data) is

      use Resources.Streams;

      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Release_Controller, Release_Controller_Access);

      Ref_Counter : Release_Controller_Access := Object.Ref_Counter;

   begin
      --  Ensure call is idempotent

      Object.Ref_Counter := null;

      if Ref_Counter /= null then
         Ref_Counter.Counter := Ref_Counter.Counter - 1;

         if Ref_Counter.Counter = 0 then
            --  No more reference to this object

            if not Ref_Counter.Stream_Taken
              and then Object.Stream /= null
            then
               --  We can finalize it as the socket has not been recorded
               Close (Object.Stream.all);
               Unchecked_Free (Object.Stream);
            end if;

            Unchecked_Free (Ref_Counter);
         end if;
      end if;
   end Finalize;

   ----------------
   -- Has_Header --
   ----------------

   function Has_Header (D : Data; Name : String) return Boolean is
   begin
      return D.Header.Exist (Name);
   end Has_Header;

   ------------
   -- Header --
   ------------

   function Header (D : Data) return AWS.Headers.List is
   begin
      return D.Header;
   end Header;

   function Header (D : Data; Name : String; N : Positive) return String is
   begin
      return Headers.Get (D.Header, Name, N);
   end Header;

   function Header (D : Data; Name : String) return String is
   begin
      return Headers.Get_Values (D.Header, Name);
   end Header;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (Object : in out Data) is
   begin
      Object.Ref_Counter := new Release_Controller;
      Object.Header.Reset;
   end Initialize;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (D : Data) return Boolean is
   begin
      return D.Mode = No_Data;
   end Is_Empty;

   ----------------
   -- Keep_Alive --
   ----------------

   function Keep_Alive (D : Data) return Boolean is
   begin
      return D.Keep_Alive;
   end Keep_Alive;

   --------------
   -- Location --
   --------------

   function Location (D : Data) return String is
   begin
      return Headers.Get (D.Header, Messages.Location_Token);
   end Location;

   ------------------
   -- Message_Body --
   ------------------

   function Message_Body (D : Data) return String is
   begin
      return Translator.To_String (Message_Body (D));
   end Message_Body;

   function Message_Body (D : Data) return Unbounded_String is
      use type Resources.Streams.Stream_Access;

      Result : Unbounded_String;
      Buffer : Stream_Element_Array (1 .. 4_096);
      Last   : Stream_Element_Offset;
   begin
      if D.Stream = null then
         return Null_Unbounded_String;
      end if;

      loop
         Resources.Streams.Read (D.Stream.all, Buffer, Last);

         Append (Result, Translator.To_String (Buffer (1 .. Last)));

         exit when Last < Buffer'Last;
      end loop;

      RSM.Reset (RSM.Stream_Type (D.Stream.all));

      return Result;
   end Message_Body;

   function Message_Body (D : Data) return Streams.Stream_Element_Array is
      use type Resources.Streams.Stream_Access;

      No_Data : constant Streams.Stream_Element_Array := (1 .. 0 => 0);
      Size    : Stream_Element_Offset;
   begin
      if D.Stream = null then
         return No_Data;
      end if;

      Size := Resources.Streams.Size (D.Stream.all);

      if Size = Resources.Undefined_Length then
         --  Undefined_Length could have only user defined streams.
         --  We do not have memory stream here.

         raise Constraint_Error;
      else
         declare
            Result : Stream_Element_Array (1 .. Size);
            Last   : Stream_Element_Offset;
         begin
            Resources.Streams.Read (D.Stream.all, Result, Last);

            --  Raise Contraint_Error on try to get Message_Body
            --  not from memory stream.

            RSM.Reset (RSM.Stream_Type (D.Stream.all));

            return Result;
         end;
      end if;
   end Message_Body;

   procedure Message_Body
     (D    : Data;
      File : out AWS.Resources.File_Type) is
   begin
      Resources.Streams.Create (File, D.Stream);

      D.Ref_Counter.Stream_Taken := True;
   end Message_Body;

   ----------
   -- Mode --
   ----------

   function Mode (D : Data) return Data_Mode is
   begin
      return D.Mode;
   end Mode;

   -----------
   -- Moved --
   -----------

   function Moved
     (Location      : String;
      Message       : String                := Default_Moved_Message;
      Content_Type  : String                := AWS.MIME.Text_HTML;
      Cache_Control : Messages.Cache_Option := Messages.Unspecified)
      return Data
   is
      use Ada.Strings;

      function Build_Message_Body return String;
      --  Returns proper message body using Message template. It replaces _@_
      --  in Message by Location.

      ------------------------
      -- Build_Message_Body --
      ------------------------

      function Build_Message_Body return String is
         Start : constant Natural := Fixed.Index (Message, "_@_");
      begin
         if Start = 0 then
            return Message;
         else
            return Fixed.Replace_Slice (Message, Start, Start + 2, Location);
         end if;
      end Build_Message_Body;

      Message_Body : constant String := Build_Message_Body;
      Result       : Data;

   begin
      Set.Location      (Result, Location);
      Set.Status_Code   (Result, Messages.S301);
      Set.Message_Body  (Result, Message_Body);
      Set.Content_Type  (Result, Content_Type);
      Set.Cache_Control (Result, Cache_Control);
      return Result;
   end Moved;

   -----------
   -- Realm --
   -----------

   function Realm (D : Data) return String is
      use Headers;
   begin
      return Values.Search
        (Header_Value   => Get (D.Header, Messages.WWW_Authenticate_Token),
         Name           => "realm",
         Case_Sensitive => False);
   end Realm;

   -----------------
   -- Send_Header --
   -----------------

   procedure Send_Header (Socket : Net.Socket_Type'Class; D : Data) is
   begin
      Headers.Send_Header (Socket, D.Header);
   end Send_Header;

   ------------------
   -- Socket_Taken --
   ------------------

   function Socket_Taken return Data is
      Result : Data;
   begin
      Set.Mode (Result, Socket_Taken);
      return Result;
   end Socket_Taken;

   -----------------
   -- Status_Code --
   -----------------

   function Status_Code (D : Data) return Messages.Status_Code is
   begin
      return D.Status_Code;
   end Status_Code;

   ------------
   -- Stream --
   ------------

   function Stream
     (Content_Type  : String;
      Handle        : not null access Resources.Streams.Stream_Type'Class;
      Status_Code   : Messages.Status_Code      := Messages.S200;
      Cache_Control : Messages.Cache_Option     := Messages.No_Cache;
      Encoding      : Messages.Content_Encoding := Messages.Identity;
      Server_Close  : Boolean                   := True;
      Disposition   : Disposition_Mode          := None;
      User_Filename : String                    := "") return Data
   is
      function CD_Filename return String with Inline;
      --  Returns the Content-Disposition filename

      -----------------
      -- CD_Filename --
      -----------------

      function CD_Filename return String is
      begin
         if User_Filename = "" then
            return Directories.Simple_Name
              (Resources.Streams.Name (Handle.all));
         else
            return User_Filename;
         end if;
      end CD_Filename;

      Result : Data;
   begin
      if Handle.Name /= "" then
         Set.Filename (Result, Handle.Name);
      end if;

      Set.Stream         (Result, Handle, Encoding);
      Set.Status_Code    (Result, Status_Code);
      Set.Content_Type   (Result, Content_Type);
      Set.Cache_Control  (Result, Cache_Control);
      Set.Close_Resource (Result, Server_Close);

      --  Set the Content_Disposition to properly pass the filename to
      --  the browser.

      if Disposition = None or else CD_Filename = "" then
         null;

      elsif Disposition = Attachment then
         Set.Add_Header
           (Result,
            Messages.Content_Disposition_Token,
            "attachment; filename=""" & CD_Filename & '"');

      elsif Disposition = Inline then
         Set.Add_Header
           (Result,
            Messages.Content_Disposition_Token,
            "inline; filename=""" & CD_Filename & '"');
      end if;

      return Result;
   end Stream;

   ---------
   -- URL --
   ---------

   function URL
     (Location      : String;
      Cache_Control : Messages.Cache_Option := Messages.Unspecified)
      return Data
   is
      Result : Data;
   begin
      Set.Status_Code   (Result, Messages.S302);
      Set.Location      (Result, Location);
      Set.Mode          (Result, Header);
      Set.Cache_Control (Result, Cache_Control);
      return Result;
   end URL;

   ---------------
   -- WebSocket --
   ---------------

   function WebSocket return Data is
      Result : Data;
   begin
      Set.Status_Code (Result, Messages.S101);
      Set.Mode        (Result, WebSocket);
      Set.Add_Header
        (Result, Messages.Connection_Token, Messages.Upgrade_Token);
      Set.Add_Header
        (Result, Messages.Upgrade_Token, Messages.Websocket_Token);
      return Result;
   end WebSocket;

end AWS.Response;
