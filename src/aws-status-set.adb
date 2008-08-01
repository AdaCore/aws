------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2008, AdaCore                     --
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

with AWS.Headers.Set;
with AWS.Headers.Values;
with AWS.Messages;
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
      Name, Value : in     String;
      Decode      : in Boolean := True) is
   begin
      AWS.Parameters.Set.Add
        (AWS.URL.Set.Parameters (D.URI'Access).all, Name, Value, Decode);
   end Add_Parameter;

   --------------------
   -- Add_Parameters --
   --------------------

   procedure Add_Parameters (D : in out Data; Parameters : in String) is
   begin
      AWS.Parameters.Set.Add
        (AWS.URL.Set.Parameters (D.URI'Access).all, Parameters);
   end Add_Parameters;

   -----------------
   -- Attachments --
   -----------------

   procedure Attachments
     (D           : in out Data;
      Attachments : in     AWS.Attachments.List) is
   begin
      D.Attachments := Attachments;
   end Attachments;

   ------------------
   -- Authenticate --
   ------------------

   procedure Authenticate
     (D                      : in out Data;
      Authorization_Mode     : in     Authorization_Type;
      Authorization_Name     : in     String;
      Authorization_Password : in     String) is
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

      procedure Named_Value (Name, Value : in String; Quit : in out Boolean);

      procedure Value (Item : in String; Quit : in out Boolean);

      -----------------
      -- Named_Value --
      -----------------

      procedure Named_Value
        (Name, Value : in String;
         Quit        : in out Boolean)
      is

         type Digest_Attribute
            is (Username, Realm, Nonce, NC, CNonce,
                QOP, URI, Response, Algorithm);
         --  The enumeration type is using to be able to
         --  use the name in the case statement.
         --  The case statement has usially faster implementation.

         Attribute : Digest_Attribute;

         function "+"
           (Item : in String)
            return Unbounded_String
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

      procedure Value (Item : in String; Quit : in out Boolean) is
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

   procedure Binary
     (D         : in out Data;
      Parameter : in     Stream_Element_Array) is
   begin
      D.Binary_Data := new Stream_Element_Array'(Parameter);
   end Binary;

   -------------------------------
   -- Case_Sensitive_Parameters --
   -------------------------------

   procedure Case_Sensitive_Parameters (D : in out Data; Mode : in Boolean) is
   begin
      AWS.Parameters.Set.Case_Sensitive
        (AWS.URL.Set.Parameters (D.URI'Access).all, Mode);
   end Case_Sensitive_Parameters;

   ---------------------
   -- Connection_Data --
   ---------------------

   procedure Connection_Data
     (D        : in out Data;
      Host     : in     String;
      Port     : in     Positive;
      Security : in     Boolean) is
   begin
      AWS.URL.Set.Connection_Data (D.URI, Host, Port, Security);
   end Connection_Data;

   ----------
   -- Free --
   ----------

   procedure Free (D : in out Data) is
   begin
      Utils.Free (D.Binary_Data);
      AWS.Attachments.Reset (D.Attachments, Delete_Files => True);
   end Free;

   ----------------
   -- Keep_Alive --
   ----------------

   procedure Keep_Alive
     (D    : in out Data;
      Flag : in     Boolean) is
   begin
      D.Keep_Alive := Flag;
   end Keep_Alive;

   ----------------
   -- Parameters --
   ----------------

   procedure Parameters (D : in out Data; Set : in AWS.Parameters.List) is
   begin
      AWS.URL.Set.Parameters (D.URI, Set);
   end Parameters;

   -----------------
   -- Read_Header --
   -----------------

   procedure Read_Header
     (Socket : in     Net.Socket_Type'Class;
      D      : in out Data) is
   begin
      Headers.Set.Read (Socket, D.Header);
      Update_Data_From_Header (D);
   end Read_Header;

   -------------
   -- Request --
   -------------

   procedure Request
     (D            : in out Data;
      Method       : in     String;
      URI          : in     String;
      HTTP_Version : in     String) is
   begin
      D.Request_Time  := Ada.Calendar.Clock;

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
      Utils.Free (D.Binary_Data);

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

      AWS.Headers.Set.Reset (D.Header);
      AWS.Parameters.Set.Reset (AWS.URL.Set.Parameters (D.URI'Access).all);
      AWS.Attachments.Reset (D.Attachments, Delete_Files => True);
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

   procedure Socket
     (D    : in out Data;
      Sock : in     Net.Socket_Access) is
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
                 (Item : in     String;
                  Quit : in out Boolean) is null;
               --  Called for every un-named value read from the header value

               procedure Named_Value
                 (Name  : in     String;
                  Value : in     String;
                  Quit  : in out Boolean);
               --  Called for every named value read from the header value

               -----------------
               -- Named_Value --
               -----------------

               procedure Named_Value
                 (Name  : in     String;
                  Value : in     String;
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

end AWS.Status.Set;
