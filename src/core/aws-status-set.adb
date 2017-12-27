------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2017, AdaCore                     --
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

with Ada.Characters.Handling;
with Ada.Strings.Fixed;
with Ada.Unchecked_Deallocation;

with GNAT.SHA256;

with AWS.Headers;
with AWS.Headers.Values;
with AWS.Messages;
with AWS.Net.Buffered;
with AWS.Parameters;
with AWS.Resources.Streams.Memory.ZLib;
with AWS.Server;
with AWS.Translator;
with AWS.URL.Set;
with ZLib;

package body AWS.Status.Set is

   use Ada.Strings;

   procedure Authorization (D : in out Data);
   --  Parse the Authorization parameters from the Authorization header value

   procedure Update_Data_From_Header (D : in out Data);
   --  Update some Data fields from the internal Data header container.
   --  The Update_Data_From_Header should be called after the complete
   --  header parsing.

   procedure Create_Stream (D : in out Data)
     with Pre  => D.Binary_Data = null,
          Post => D.Binary_Data /= null;
   --  Create the in-memory stream used to store the message data. This stream
   --  is either standard or compressed to support GZip content-encoding.

   function Create_Private_Hash
     (SID : AWS.Session.Id) return GNAT.SHA256.Message_Digest with Inline;
   --  Returns the binary private digest for the SID

   -------------------
   -- Add_Parameter --
   -------------------

   procedure Add_Parameter
     (D           : in out Data;
      Name, Value : String;
      Decode      : Boolean := True;
      Replace     : Boolean := False) is
   begin
      Add_Parameter
        (D       => D,
         Name    => To_Unbounded_String (Name),
         Value   => To_Unbounded_String (Value),
         Decode  => Decode,
         Replace => Replace);
   end Add_Parameter;

   procedure Add_Parameter
     (D           : in out Data;
      Name, Value : Unbounded_String;
      Decode      : Boolean := True;
      Replace     : Boolean := False) is
   begin
      if Replace then
         AWS.URL.Set.Parameters (D.URI'Access).all.Update
           (Name, Value, Decode);
      else
         AWS.URL.Set.Parameters (D.URI'Access).all.Add (Name, Value, Decode);
      end if;
   end Add_Parameter;

   --------------------
   -- Add_Parameters --
   --------------------

   procedure Add_Parameters (D : in out Data; Parameters : String) is
   begin
      AWS.URL.Set.Parameters (D.URI'Access).all.Add (Parameters);
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
         Create_Stream (D);
      end if;

      D.Binary_Data.Append (Buffer, Trim);
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

      Header_Value : constant String :=
                       Headers.Get (D.Header, Messages.Authorization_Token);

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
         Upper_Item : constant String :=
                        Characters.Handling.To_Upper (Item);
      begin
         if Upper_Item = "BASIC" then

            D.Auth_Mode := Basic;

            Quit := True;

            --  We could not continue to parse Basic authentication
            --  by the regular way, because next value is Base64 encoded
            --  "username:password", it is possibe to have symbol '=' there,
            --  our parser could think that it is name/value delimiter.

            declare
               Auth_Str : constant String :=
                            Translator.To_String
                              (Translator.Base64_Decode
                                 (Header_Value
                                    (Item'Length + 2 .. Header_Value'Last)));

               Delimit  : constant Natural := Fixed.Index (Auth_Str, ":");
            begin
               if Delimit = 0 then
                  D.Auth_Name := To_Unbounded_String (Auth_Str);

               else
                  D.Auth_Name :=
                    To_Unbounded_String (Auth_Str (1 .. Delimit - 1));
                  D.Auth_Password :=
                    To_Unbounded_String
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
         Create_Stream (D);

      else
         --  Clear previous data if exists

         D.Binary_Data.Clear;
      end if;

      --  "Trim => True" mean don't remain allocated space at the end of
      --  internal buffer for next data because this routine designed to put
      --  all data at once.

      D.Binary_Data.Append (Parameter, Trim => True);
   end Binary;

   -------------------------------
   -- Case_Sensitive_Parameters --
   -------------------------------

   procedure Case_Sensitive_Parameters (D : in out Data; Mode : Boolean) is
   begin
      AWS.URL.Set.Parameters (D.URI'Access).all.Case_Sensitive (Mode);
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
   -- Create_Private_Hash --
   -------------------------

   function Create_Private_Hash
     (SID : AWS.Session.Id) return GNAT.SHA256.Message_Digest
   is
      P_Key   : constant String  := AWS.Session.Private_Key (SID);
      L_SID   : constant String  := AWS.Session.Image (SID);
      Context : SHA256.Context   := SHA256.HMAC_Initial_Context (P_Key);
   begin
      SHA256.Update (Context, L_SID);
      return SHA256.Digest (Context);
   end Create_Private_Hash;

   -------------------
   -- Create_Stream --
   -------------------

   procedure Create_Stream (D : in out Data) is
   begin
      if Headers.Get (D.Header, Messages.Content_Encoding_Token) = "gzip" then
         D.Binary_Data := new Resources.Streams.Memory.ZLib.Stream_Type;
         Resources.Streams.Memory.ZLib.Stream_Type
           (D.Binary_Data.all).Inflate_Initialize (Header => ZLib.GZip);

      else
         D.Binary_Data := new Resources.Streams.Memory.Stream_Type;
      end if;
   end Create_Stream;

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
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Resources.Streams.Memory.Stream_Type'Class, Memory_Stream_Access);
   begin
      if D.Binary_Data /= null then
         D.Binary_Data.Close;
         Unchecked_Free (D.Binary_Data);
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
      AWS.URL.Set.Parameters (D.URI'Access).all.Add (D.Binary_Data.all);
   end Parameters_From_Body;

   -----------
   -- Query --
   -----------

   procedure Query (D : in out Data; Parameters : String) is
      P : constant not null access AWS.Parameters.List :=
            AWS.URL.Set.Parameters (D.URI'Access);
   begin
      if P.Count > 0 then
         raise Program_Error with "Could not set HTTP Query twice";
      end if;

      D.Query := To_Unbounded_String (Parameters);

      P.Add (Parameters);
   end Query;

   ---------------
   -- Read_Body --
   ---------------

   procedure Read_Body
     (Socket   : Net.Socket_Type'Class;
      D        : in out Data;
      Boundary : String := "")
   is
      procedure Read_Whole_Body;
      --  Read the whole body (Content_Length octets)

      ---------------------
      -- Read_Whole_Body --
      ---------------------

      procedure Read_Whole_Body is

         procedure Read_Chunk (Size : Stream_Element_Offset);
         --  Read a chunk of data of the given Size, the corresponding data is
         --  added into the Binary_Data.

         ----------------
         -- Read_Chunk --
         ----------------

         procedure Read_Chunk (Size : Stream_Element_Offset) is
            Buffer : Stream_Element_Array (1 .. 4096);
            Rest   : Stream_Element_Offset := Size;
         begin
            while Rest > Buffer'Length loop
               Rest := Rest - Buffer'Length;
               Net.Buffered.Read (Socket, Buffer);
               D.Binary_Data.Append (Buffer);
            end loop;

            Net.Buffered.Read (Socket, Buffer (1 .. Rest));
            D.Binary_Data.Append (Buffer (1 .. Rest), Trim => True);
         end Read_Chunk;

         TE : constant String  :=
                Headers.Get (D.Header, Messages.Transfer_Encoding_Token);
      begin
         if TE = "chunked" then
            --  A chuncked message is written on the stream as list of data
            --  chunk. Each chunk has the following format:
            --
            --  <N : the chunk size in hexadecimal> CRLF
            --  <N * BYTES : the data> CRLF
            --
            --  The termination chunk is:
            --
            --  0 CRLF
            --  CRLF
            --

            Read_Chunks : loop
               declare
                  C_Size : constant Stream_Element_Offset :=
                             Stream_Element_Offset'Value
                               ("16#" & Net.Buffered.Get_Line (Socket) & '#');
                  CRLF   : Stream_Element_Array (1 .. 2);
               begin
                  if C_Size = 0 then
                     --  We reached the end of the chunked message, read
                     --  terminating CRLF.
                     Net.Buffered.Read (Socket, CRLF);
                     exit Read_Chunks;
                  end if;

                  Read_Chunk (Size => C_Size);
                  Net.Buffered.Read (Socket, CRLF);
               end;
            end loop Read_Chunks;

         else
            Read_Chunk (Size => Stream_Element_Offset (D.Content_Length));
         end if;
      end Read_Whole_Body;

   begin
      if D.Binary_Data = null then
         Create_Stream (D);
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
               D.Binary_Data.Append
                 (Content (Content'First
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
      D.Header.Read (Socket);
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
      D.Calendar_Time  := Calendar.Clock;
      D.Monotonic_Time := Real_Time.Clock;

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

      AWS.URL.Set.Parse (D.URI, URI, False, False);
   end Request;

   -----------
   -- Reset --
   -----------

   procedure Reset (D : in out Data) is
   begin
      Free (D);

      D.Socket            := null;
      D.Peername          := Null_Unbounded_String;
      D.Method            := GET;
      D.Method_String     := Null_Unbounded_String;
      D.Query             := Null_Unbounded_String;
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
      D.Session_Private   := No_Session_Private;
      D.Session_Created   := False;
      D.Session_Timed_Out := False;
      D.SOAP_Action       := False;
      D.Uploaded          := False;
      D.Monotonic_Time    := Ada.Real_Time.Time_First;

      D.Header.Reset;
      AWS.URL.Set.Parameters (D.URI'Access).all.Reset;
   end Reset;

   -------------
   -- Session --
   -------------

   procedure Session (D : in out Data) is
   begin
      D.Session_Id      := AWS.Session.Create;

      --  Create the session's private key

      D.Session_Private := Create_Private_Hash (D.Session_Id);

      D.Session_Created := True;
   end Session;

   -----------------------
   -- Session_Timed_Out --
   -----------------------

   procedure Session_Timed_Out (D : in out Data; Timed_Out : Boolean) is
   begin
      D.Session_Timed_Out := Timed_Out;
   end Session_Timed_Out;

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
      AWS_Session_Name      : constant String := Server.Session_Name;
      AWS_Session_Priv_Name : constant String := Server.Session_Private_Name;
   begin
      Authorization (D);

      declare
         Content_Length : constant String :=
                            AWS.Headers.Get
                              (D.Header, Messages.Content_Length_Token);
      begin
         if Content_Length /= "" then
            D.Content_Length := Stream_Element_Count'Value (Content_Length);

         --  Special case for the websockets draft76: even though there is no
         --  Content-Length header, the message contains 8 bytes that are part
         --  of the challenge and must be available to AWS.

         elsif AWS.Headers.Exist
           (D.Header, Messages.Sec_WebSocket_Key1_Token)
         then
            D.Content_Length := 8;
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
               --  Cookie: ... AWS=<cookieID>[,;] AWS_Private=<key>[,;] ...

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

               SID_Found   : Boolean := False;
               P_SID_Found : Boolean := False;

               -----------------
               -- Named_Value --
               -----------------

               procedure Named_Value
                 (Name  : String;
                  Value : String;
                  Quit  : in out Boolean) is
               begin
                  --  Check if it is current process Cookie

                  if Name = AWS_Session_Name then
                     D.Session_Id := AWS.Session.Value (Value);

                     --  Check if the cookie value was correct

                     if D.Session_Id = AWS.Session.No_Session then
                        return;
                     end if;

                     --  Check if cookie exists in the server

                     if not AWS.Session.Exist (D.Session_Id) then
                        --  Reset to empty cookie if session does not exist.
                        --  This is a case where a session has timed out.

                        D.Session_Id        := AWS.Session.No_Session;
                        D.Session_Private   := No_Session_Private;
                        D.Session_Timed_Out := True;

                        return;
                     end if;

                     --  Check if the session has expired, even though it
                     --  hasn't been deleted yet by the cleaner task.

                     if AWS.Session.Has_Expired (D.Session_Id) then
                        AWS.Session.Delete (D.Session_Id);
                        D.Session_Id        := AWS.Session.No_Session;
                        D.Session_Private   := No_Session_Private;
                        D.Session_Timed_Out := True;
                        return;
                     end if;

                     SID_Found := True;

                  elsif Name = AWS_Session_Priv_Name then
                     if Value'Length = D.Session_Private'Length then
                        D.Session_Private := Value;
                        P_SID_Found := True;
                     end if;
                  end if;

                  Quit := SID_Found and then P_SID_Found;
               end Named_Value;

               -----------
               -- Parse --
               -----------

               procedure Parse is
                 new Headers.Values.Parse (Value, Named_Value);

            begin
               Parse (To_String (Cookies_Set (Idx)));

               --  Exit when we have found the session cookies

               exit when D.Session_Id /= AWS.Session.No_Session
                 and then D.Session_Private /= Null_Unbounded_String;
            end;
         end loop;

         --  Now double check that the session id is valid and has not been
         --  compromised.

         if D.Session_Private /= Create_Private_Hash (SID => D.Session_Id) then
            --  There is mismatch, could be a corrupted session id
            D.Session_Id := AWS.Session.No_Session;
         end if;
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
