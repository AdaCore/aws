------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2010, AdaCore                     --
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

with Ada.Characters.Handling;
with Ada.Strings.Fixed;
with Ada.Unchecked_Deallocation;

with AWS.Headers.Set;
with AWS.Headers.Values;
with AWS.Messages;
with AWS.Net.Buffered;
with AWS.Parameters.Set;
with AWS.Server;
with AWS.Translator;
with AWS.URL.Set;

package body AWS.Status.Set is

   use Ada.Strings;

   procedure Authorization (D : in out Data);
   --  Parse the Authorization parameters from the Authorization header value

   procedure Update_Data_From_Header (D : in out Data);
   --  Update some Data fields from the internal Data header container.
   --  The Update_Data_From_Header should be called after the complete
   --  header parsing.

   -------------------
   -- Add_Parameter --
   -------------------

   procedure Add_Parameter
     (D           : in out Data;
      Name, Value : String;
      Decode      : Boolean := True;
      Replace     : Boolean := False) is
   begin
      if Replace then
         AWS.Parameters.Set.Update
           (AWS.URL.Set.Parameters (D.URI'Access).all, Name, Value, Decode);
      else
         AWS.Parameters.Set.Add
           (AWS.URL.Set.Parameters (D.URI'Access).all, Name, Value, Decode);
      end if;
   end Add_Parameter;

   --------------------
   -- Add_Parameters --
   --------------------

   procedure Add_Parameters (D : in out Data; Parameters : String) is
   begin
      AWS.Parameters.Set.Add
        (AWS.URL.Set.Parameters (D.URI'Access).all, Parameters);
   end Add_Parameters;

   -----------------
   -- Append_Body --
   -----------------

   procedure Append_Body
     (D      : in out Data;
      Buffer : Stream_Element_Array;
      Trim   : Boolean := False) is
   begin
      if D.Binary_Data = null then
         D.Binary_Data := new Containers.Memory_Streams.Stream_Type;
      end if;

      Containers.Memory_Streams.Append (D.Binary_Data.all, Buffer, Trim);
   end Append_Body;

   -----------------
   -- Attachments --
   -----------------

   procedure Attachments
     (D : in out Data; Attachments : AWS.Attachments.List) is
   begin
      D.Attachments := Attachments;
   end Attachments;

   ------------------
   -- Authenticate --
   ------------------

   procedure Authenticate
     (D                      : in out Data;
      Authorization_Mode     : Authorization_Type;
      Authorization_Name     : String;
      Authorization_Password : String) is
   begin
      D.Auth_Mode     := Authorization_Mode;
      D.Auth_Name     := To_Unbounded_String (Authorization_Name);
      D.Auth_Password := To_Unbounded_String (Authorization_Password);
   end Authenticate;

   -------------------
   -- Authorization --
   -------------------

   procedure Authorization (D : in out Data) is

      Header_Value : constant String
        := AWS.Headers.Get (D.Header, Messages.Authorization_Token);

      procedure Named_Value (Name, Value : String; Quit : in out Boolean);

      procedure Value (Item : String; Quit : in out Boolean);

      -----------------
      -- Named_Value --
      -----------------

      procedure Named_Value (Name, Value : String; Quit : in out Boolean) is

         type Digest_Attribute
            is (Username, Realm, Nonce, NC, CNonce,
                QOP, URI, Response, Algorithm);
         --  The enumeration type is using to be able to
         --  use the name in the case statement.
         --  The case statement has usially faster implementation.

         Attribute : Digest_Attribute;

         function "+" (Item : String) return Unbounded_String
           renames To_Unbounded_String;

      begin
         begin
            Attribute := Digest_Attribute'Value (Name);
         exception
            when Constraint_Error =>
               --  Ignoring unrecognized attribute
               return;
         end;

         --  Check if the attributes is for the Digest authenticatio schema.
         --  AWS does not support other authentication schemas with attributes
         --  now.

         if D.Auth_Mode /= Digest then
            Quit := True;
         end if;

         case Attribute is
            when Username  => D.Auth_Name     := +Value;
            when Realm     => D.Auth_Realm    := +Value;
            when NC        => D.Auth_NC       := +Value;
            when CNonce    => D.Auth_CNonce   := +Value;
            when QOP       => D.Auth_QOP      := +Value;
            when Nonce     => D.Auth_Nonce    := +Value;
            when Response  => D.Auth_Response := +Value;
            when URI       => D.Auth_URI      := +Value;
            when Algorithm =>
               if Value /= "MD5" then
                  raise Constraint_Error
                    with "Only MD5 algorithm is supported.";
               end if;
         end case;
      end Named_Value;

      -----------
      -- Value --
      -----------

      procedure Value (Item : String; Quit : in out Boolean) is
         Upper_Item : constant String
           := Ada.Characters.Handling.To_Upper (Item);
      begin
         if Upper_Item = "BASIC" then

            D.Auth_Mode := Basic;

            Quit := True;

            --  We could not continue to parse Basic authentication
            --  by the regular way, because next value is Base64 encoded
            --  "username:password", it is possibe to have symbol '=' there,
            --  our parser could think that it is name/value delimiter.

            declare
               use Ada.Streams;

               Auth_Str : constant String
                 := Translator.To_String (Translator.Base64_Decode
                   (Header_Value (Item'Length + 2 .. Header_Value'Last)));

               Delimit  : constant Natural := Fixed.Index (Auth_Str, ":");
            begin
               if Delimit = 0 then
                  D.Auth_Name := To_Unbounded_String (Auth_Str);

               else
                  D.Auth_Name
                    := To_Unbounded_String (Auth_Str (1 .. Delimit - 1));
                  D.Auth_Password
                    := To_Unbounded_String
                         (Auth_Str (Delimit + 1 .. Auth_Str'Last));
               end if;
            end;

         elsif Upper_Item = "DIGEST" then
            D.Auth_Mode := Digest;

         end if;
      end Value;

      procedure Parse is new Headers.Values.Parse (Value, Named_Value);

   begin
      Parse (Header_Value);
   end Authorization;

   ------------
   -- Binary --
   ------------

   procedure Binary (D : in out Data; Parameter : Stream_Element_Array) is
   begin
      if D.Binary_Data = null then
         D.Binary_Data := new Containers.Memory_Streams.Stream_Type;
      end if;

      Containers.Memory_Streams.Append (D.Binary_Data.all, Parameter);
   end Binary;

   -------------------------------
   -- Case_Sensitive_Parameters --
   -------------------------------

   procedure Case_Sensitive_Parameters (D : in out Data; Mode : Boolean) is
   begin
      AWS.Parameters.Set.Case_Sensitive
        (AWS.URL.Set.Parameters (D.URI'Access).all, Mode);
   end Case_Sensitive_Parameters;

   ---------------------
   -- Connection_Data --
   ---------------------

   procedure Connection_Data
     (D        : in out Data;
      Host     : String;
      Port     : Positive;
      Security : Boolean) is
   begin
      AWS.URL.Set.Connection_Data (D.URI, Host, Port, Security);
   end Connection_Data;

   -------------------------
   -- Delete_Idle_Session --
   -------------------------

   procedure Delete_Idle_Session (D : in out Data) is
   begin
      if D.Session_Created
        and then AWS.Session.Delete_If_Empty (D.Session_Id)
      then
         D.Session_Created := False;
      end if;
   end Delete_Idle_Session;

   ----------
   -- Free --
   ----------

   procedure Free (D : in out Data) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Containers.Memory_Streams.Stream_Type, Memory_Stream_Access);
   begin
      if D.Binary_Data /= null then
         Containers.Memory_Streams.Close (D.Binary_Data.all);
         Free (D.Binary_Data);
      end if;

      AWS.Attachments.Reset (D.Attachments, Delete_Files => True);
   end Free;

   ----------------
   -- Keep_Alive --
   ----------------

   procedure Keep_Alive (D : in out Data; Flag : Boolean) is
   begin
      D.Keep_Alive := Flag;
   end Keep_Alive;

   ----------------
   -- Parameters --
   ----------------

   procedure Parameters (D : in out Data; Set : AWS.Parameters.List) is
   begin
      AWS.URL.Set.Parameters (D.URI, Set);
   end Parameters;

   --------------------------
   -- Parameters_From_Body --
   --------------------------

   procedure Parameters_From_Body (D : in out Data) is
   begin
      AWS.Parameters.Set.Add
        (AWS.URL.Set.Parameters (D.URI'Access).all, D.Binary_Data.all);
   end Parameters_From_Body;

   ---------------
   -- Read_Body --
   ---------------

   procedure Read_Body
     (Socket   : Net.Socket_Type'Class;
      D        : in out Data;
      Boundary : String := "")
   is
      use Containers.Memory_Streams;
      use type Stream_Element_Offset;

      procedure Read_Whole_Body;
      --  Read the whole body (Content_Length octets)

      ---------------------
      -- Read_Whole_Body --
      ---------------------

      procedure Read_Whole_Body is
         use Ada.Streams;
         Buffer : Stream_Element_Array (1 .. 4096);
         Rest   : Stream_Element_Offset :=
                    Stream_Element_Offset (D.Content_Length);
      begin
         while Rest > Buffer'Length loop
            Rest := Rest - Buffer'Length;
            Net.Buffered.Read (Socket, Buffer);
            Append (D.Binary_Data.all, Buffer);
         end loop;

         Net.Buffered.Read (Socket, Buffer (1 .. Rest));
         Append (D.Binary_Data.all, Buffer (1 .. Rest), Trim => True);
      end Read_Whole_Body;

   begin
      if D.Binary_Data = null then
         D.Binary_Data := new Stream_Type;
      end if;

      if Boundary = "" then
         Read_Whole_Body;

      else
         declare
            Content : constant Stream_Element_Array :=
                        Net.Buffered.Read_Until
                          (Socket,
                           Translator.To_Stream_Element_Array (Boundary));
         begin
            if Content'Length > Boundary'Length + 2 then
               Append
                 (D.Binary_Data.all,
                  Content (Content'First
                    .. Content'Last - Boundary'Length - 2),
                  Trim => True);
               --  Boundary'Length - 2 to remove the boundary and also the CRLF
               --  (before the boundary) which is not part of the body.
            end if;
         end;
      end if;
   end Read_Body;

   -----------------
   -- Read_Header --
   -----------------

   procedure Read_Header (Socket : Net.Socket_Type'Class; D : in out Data) is
   begin
      Headers.Set.Read (Socket, D.Header);
      Update_Data_From_Header (D);
   end Read_Header;

   -------------
   -- Request --
   -------------

   procedure Request
     (D            : in out Data;
      Method       : String;
      URI          : String;
      HTTP_Version : String) is
   begin
      D.Calendar_Time  := Ada.Calendar.Clock;
      D.Monotonic_Time := Ada.Real_Time.Clock;

      --  Method is case sensitive

      if Method = Messages.Options_Token then
         D.Method := Status.OPTIONS;

      elsif Method = Messages.Get_Token then
         D.Method := Status.GET;

      elsif Method = Messages.Head_Token then
         D.Method := Status.HEAD;

      elsif Method = Messages.Post_Token then
         D.Method := Status.POST;

      elsif Method = Messages.Put_Token then
         D.Method := Status.PUT;

      elsif Method = Messages.Delete_Token then
         D.Method := Status.DELETE;

      elsif Method = Messages.Trace_Token then
         D.Method := Status.TRACE;

      elsif Method = Messages.Connect_Token then
         D.Method := Status.CONNECT;

      else
         D.Method := Status.EXTENSION_METHOD;
      end if;

      D.Method_String := To_Unbounded_String (Method);
      D.HTTP_Version  := To_Unbounded_String (HTTP_Version);

      --  Parse URI and keep parameters case sensitivity flag

      URL.Set.Parse (D.URI, URI, False, False);
   end Request;

   -----------
   -- Reset --
   -----------

   procedure Reset (D : in out Data) is
   begin
      Free (D);

      D.Method            := GET;
      D.HTTP_Version      := Null_Unbounded_String;
      D.Content_Length    := 0;
      D.Auth_Mode         := None;
      D.Auth_Name         := Null_Unbounded_String;
      D.Auth_Password     := Null_Unbounded_String;
      D.Auth_Realm        := Null_Unbounded_String;
      D.Auth_Nonce        := Null_Unbounded_String;
      D.Auth_NC           := Null_Unbounded_String;
      D.Auth_CNonce       := Null_Unbounded_String;
      D.Auth_QOP          := Null_Unbounded_String;
      D.Auth_URI          := Null_Unbounded_String;
      D.Auth_Response     := Null_Unbounded_String;
      D.Session_Id        := AWS.Session.No_Session;
      D.Session_Created   := False;
      D.Session_Timed_Out := False;
      D.Uploaded          := False;
      D.Monotonic_Time    := Ada.Real_Time.Time_First;

      AWS.Headers.Set.Reset (D.Header);
      AWS.Parameters.Set.Reset (AWS.URL.Set.Parameters (D.URI'Access).all);
   end Reset;

   -------------
   -- Session --
   -------------

   procedure Session (D : in out Data) is
   begin
      D.Session_Id      := AWS.Session.Create;
      D.Session_Created := True;
   end Session;

   ------------
   -- Socket --
   ------------

   procedure Socket (D : in out Data; Sock : Net.Socket_Access) is
   begin
      D.Socket   := Sock;
      D.Peername := To_Unbounded_String (Net.Peer_Addr (Sock.all));
   end Socket;

   -----------------------------
   -- Update_Data_From_Header --
   -----------------------------

   procedure Update_Data_From_Header (D : in out Data) is
      AWS_Session_Name : constant String := Server.Session_Name;
   begin
      Authorization (D);

      declare
         Content_Length : constant String := AWS.Headers.Get
           (D.Header, Messages.Content_Length_Token);
      begin
         if Content_Length /= "" then
            D.Content_Length := Integer'Value (Content_Length);
         end if;
      end;

      D.SOAP_Action := AWS.Headers.Exist (D.Header, Messages.SOAPAction_Token);

      declare
         use AWS.Headers;

         Cookies_Set : constant VString_Array :=
                         Get_Values (D.Header, Messages.Cookie_Token);
      begin
         for Idx in Cookies_Set'Range loop

            declare
               --  The expected Cookie line is:
               --  Cookie: ... AWS=<cookieID>[,;] ...

               use type AWS.Session.Id;

               procedure Value
                 (Item : String;
                  Quit : in out Boolean) is null;
               --  Called for every un-named value read from the header value

               procedure Named_Value
                 (Name  : String;
                  Value : String;
                  Quit  : in out Boolean);
               --  Called for every named value read from the header value

               -----------------
               -- Named_Value --
               -----------------

               procedure Named_Value
                 (Name  : String;
                  Value : String;
                  Quit  : in out Boolean) is
               begin
                  --  Check if it is current process Cookie

                  if Name /= AWS_Session_Name then
                     return;
                  end if;

                  D.Session_Id := AWS.Session.Value (Value);

                  --  Check if the cookie value was correct

                  if D.Session_Id = AWS.Session.No_Session then
                     return;
                  end if;

                  --  Check if cookie exists in the server

                  if not AWS.Session.Exist (D.Session_Id) then
                     --  Reset to empty cookie if session does not exists.
                     --  This is a case where a session has timed out.

                     D.Session_Id        := AWS.Session.No_Session;
                     D.Session_Timed_Out := True;

                     return;
                  end if;

                  --  Check if the session has expired, even though it hasn't
                  --  been deleted yet by the cleaner task.

                  if AWS.Session.Has_Expired (D.Session_Id) then
                     AWS.Session.Delete (D.Session_Id);
                     D.Session_Id        := AWS.Session.No_Session;
                     D.Session_Timed_Out := True;
                     return;
                  end if;

                  --  If all filters done, we found our cookie !
                  Quit := True;
               end Named_Value;

               -----------
               -- Parse --
               -----------

               procedure Parse is
                  new AWS.Headers.Values.Parse (Value, Named_Value);

            begin
               Parse (To_String (Cookies_Set (Idx)));

               --  Exit when we have found an existing cookie
               exit when D.Session_Id /= AWS.Session.No_Session;
            end;
         end loop;
      end;
   end Update_Data_From_Header;

   --------------
   -- Uploaded --
   --------------

   procedure Uploaded (D : in out Data) is
   begin
      D.Uploaded := True;
   end Uploaded;

end AWS.Status.Set;
