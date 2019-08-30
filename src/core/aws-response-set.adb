------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2002-2019, AdaCore                     --
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

with Ada.Characters.Handling;
with Ada.Strings.Fixed;

with AWS.Digest;
with AWS.Headers.Values;
with AWS.Resources.Streams.Memory.ZLib;
with AWS.Resources.Streams.ZLib;
with AWS.Server;
with AWS.Translator;

with ZLib;

package body AWS.Response.Set is

   package RSM renames AWS.Resources.Streams.Memory;

   procedure Check_Memory_Stream (D : in out Data) with Inline;
   --  Test if the memory stream allocated and create it if necessary

   procedure Clear_Memory_Stream (D : in out Data) with Inline;
   --  Test if the memory stream allocated and create it if necessary,
   --  if the stream already created, clear it.

   ----------------
   -- Add_Header --
   ----------------

   procedure Add_Header
     (D     : in out Data;
      Name  : String;
      Value : String) is
   begin
      D.Header.Add (Name, Value);
   end Add_Header;

   -----------------
   -- Append_Body --
   -----------------

   procedure Append_Body
     (D    : in out Data;
      Item : Streams.Stream_Element_Array)  is
   begin
      Check_Memory_Stream (D);

      RSM.Append (RSM.Stream_Type'Class (D.Stream.all), Item);
   end Append_Body;

   procedure Append_Body (D : in out Data; Item : String) is
   begin
      Append_Body (D, Translator.To_Stream_Element_Array (Item));
   end Append_Body;

   --------------------
   -- Authentication --
   --------------------

   procedure Authentication
     (D     : in out Data;
      Realm : String;
      Mode  : Authentication_Mode := Basic;
      Stale : Boolean             := False)
   is
      N : Positive := 1;
      --  The index for the update of WWW-Authenticate header values.
      --  We are not using AWS.Headers.Set.Add routine for add WWW-Authenticate
      --  header lines, because user could call this routine more than once.
   begin
      --  In case of Authenticate = Any
      --  We should create both header lines
      --  WWW-Authenticate: Basic
      --  and
      --  WWW-Authenticate: Digest

      if Mode = Digest or else Mode = Any then
         D.Header.Update
           (Name  => Messages.WWW_Authenticate_Token,
            Value => "Digest qop=""auth"", realm=""" & Realm
                     & """, stale=""" & Boolean'Image (Stale)
                     & """, nonce="""
                     & String (AWS.Digest.Create_Nonce) & """",
            N => N);
         N := N + 1;
      end if;

      if Mode = Basic or else Mode = Any then
         D.Header.Update
           (Name  => Messages.WWW_Authenticate_Token,
            Value => "Basic realm=""" & Realm & """",
            N     => N);
      end if;

      D.Status_Code := Messages.S401;
   end Authentication;

   -------------------
   -- Cache_Control --
   -------------------

   procedure Cache_Control
     (D     : in out Data;
      Value : Messages.Cache_Option)
   is
      use type Messages.Cache_Option;
   begin
      if Value /= Messages.Unspecified then
         D.Header.Update
           (Name  => Messages.Cache_Control_Token,
            Value => String (Value));

         if Strings.Fixed.Index (String (Value), "no-cache") /= 0 then
            --  There is a no-cache option specified for the Cache-Control
            --  header. Add "Pragma: no-cache" for compatibility with HTTP/1.0
            --  protocol.

            D.Header.Update
              (Name => Messages.Pragma_Token, Value => "no-cache");
         end if;
      end if;
   end Cache_Control;

   -------------------------
   -- Check_Memory_Stream --
   -------------------------

   procedure Check_Memory_Stream (D : in out Data) is
      use type Resources.Streams.Stream_Access;
   begin
      if D.Stream = null then
         D.Stream := new RSM.Stream_Type;
         D.Mode   := Message;
      end if;
   end Check_Memory_Stream;

   -----------
   -- Clear --
   -----------

   procedure Clear (D : in out Data) is
      use type Resources.Streams.Stream_Access;
   begin
      if D.Stream /= null then
         if not D.Ref_Counter.Stream_Taken then
            Resources.Streams.Close (D.Stream.all);
         end if;
         Unchecked_Free (D.Stream);
      end if;

      D.Header.Reset;

      D.Mode        := No_Data;
      D.Status_Code := Messages.S200;
      D.Filename    := Null_Unbounded_String;
   end Clear;

   -------------------------
   -- Clear_Memory_Stream --
   -------------------------

   procedure Clear_Memory_Stream (D : in out Data) is
      use type Resources.Streams.Stream_Access;
   begin
      if D.Stream = null then
         D.Stream := new RSM.Stream_Type;
         D.Mode   := Message;
      else
         RSM.Clear (RSM.Stream_Type (D.Stream.all));
      end if;
   end Clear_Memory_Stream;

   -------------------
   -- Clear_Session --
   -------------------

   procedure Clear_Session (D : in out Data) is
   begin
      Add_Header (D, "Set-Cookie", AWS.Server.Session_Name & "=");
   end Clear_Session;

   --------------------
   -- Close_Resource --
   --------------------

   procedure Close_Resource
     (D     : in out Data;
      State : Boolean) is
   begin
      D.Close_Stream := State;
   end Close_Resource;

   ------------------
   -- Content_Type --
   ------------------

   procedure Content_Type
     (D     : in out Data;
      Value : String) is
   begin
      D.Content_Type := To_Unbounded_String (Value);
   end Content_Type;

   -------------------
   -- Data_Encoding --
   -------------------

   procedure Data_Encoding
     (D         : in out Data;
      Encoding  : Messages.Content_Encoding;
      Direction : Encoding_Direction := Encode)
   is
      use type Resources.Streams.Stream_Access;
      Header : RSM.ZLib.Header_Type := ZLib.None;
   begin
      if D.Stream /= null then
         --  The stream is already active, there is nothing to do, we can't
         --  change the encoding when data has alredy been added into the
         --  stream.
         return;
      end if;

      D.Mode := Message;

      case Encoding is
         when Messages.Identity => D.Stream := new RSM.Stream_Type;
         when Messages.GZip     => Header   := ZLib.GZip;
         when Messages.Deflate  => Header   := ZLib.Default;
      end case;

      if D.Stream = null then
         --  ZLib encoding/decoding is necessary

         D.Stream := new RSM.ZLib.Stream_Type;
         --  Create the encoding stream, now initialize it

         if Direction = Encode then
            RSM.ZLib.Deflate_Initialize
              (RSM.ZLib.Stream_Type (D.Stream.all), Header => Header);

            --  Set the Content-Encoding header value for server's response
            --
            --  Looks like IE in the Windows Mobile 5.0 do not understand
            --  content coding in the upper case. Bug found in the
            --  User-Agent:
            --  Mozilla/4.0 (compatible; MSIE 4.01; Windows CE; PPC; 240x320)

            Update_Header
              (D,
               Messages.Content_Encoding_Token,
               Ada.Characters.Handling.To_Lower
                 (Messages.Content_Encoding'Image (Encoding)));

         else
            RSM.ZLib.Inflate_Initialize
              (RSM.ZLib.Stream_Type (D.Stream.all), Header => Header);
         end if;
      end if;
   end Data_Encoding;

   -------------
   -- Expires --
   -------------

   procedure Expires
     (D     : in out Data;
      Value : Calendar.Time) is
   begin
      Expires (D, Messages.To_HTTP_Date (Value));
   end Expires;

   procedure Expires
     (D     : in out Data;
      Value : String) is
   begin
      D.Header.Update (Name => Messages.Expires_Token, Value => Value);
   end Expires;

   --------------
   -- Filename --
   --------------

   procedure Filename
     (D     : in out Data;
      Value : String) is
   begin
      D.Filename := To_Unbounded_String (Value);
      D.Mode     := File;
   end Filename;

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid (D : Data) return Boolean is
      Redirection_Code : Boolean;
   begin
      case D.Status_Code is
         when
           Messages.S300 | -- Section 10.3.1: Multiple Choices
           Messages.S301 | -- Section 10.3.2: Moved Permanently
           Messages.S302 | -- Section 10.3.3: Found
           Messages.S303 | -- Section 10.3.4: See Other
           Messages.S305 | -- Section 10.3.6: Use Proxy
           Messages.S307   -- Section 10.3.8: Temporary Redirect
           =>
            Redirection_Code := True;

         when others =>
            Redirection_Code := False;
      end case;

      return (Redirection_Code
                xor not Headers.Exist
                          (D.Header,
                           Messages.Location_Token))
        and then (D.Status_Code = Messages.S401
                    xor not Headers.Exist
                              (D.Header,
                               Messages.WWW_Authenticate_Token));
   end Is_Valid;

   ----------------
   -- Keep_Alive --
   ----------------

   procedure Keep_Alive (D : in out Data; State : Boolean) is
   begin
      D.Keep_Alive := State;
   end Keep_Alive;

   --------------
   -- Location --
   --------------

   procedure Location
     (D     : in out Data;
      Value : String) is
   begin
      D.Header.Update (Name  => Messages.Location_Token, Value => Value);
   end Location;

   ------------------
   -- Message_Body --
   ------------------

   procedure Message_Body
     (D     : in out Data;
      Value : Streams.Stream_Element_Array) is
   begin
      Clear_Memory_Stream (D);

      RSM.Append (RSM.Stream_Type'Class (D.Stream.all), Value);
   end Message_Body;

   procedure Message_Body
     (D     : in out Data;
      Value : String) is
   begin
      Message_Body (D, To_Unbounded_String (Value));
   end Message_Body;

   procedure Message_Body
     (D     : in out Data;
      Value : Unbounded_String)
   is
      Chunk_Size  : constant := 8 * 1_024;
      Len         : constant Natural := Length (Value);

      First, Last : Natural;
   begin
      --  Prepare the memory stream

      Clear_Memory_Stream (D);

      First := 1;

      loop
         Last := Integer'Min (First + Chunk_Size - 1, Len);

         RSM.Append
           (RSM.Stream_Type'Class (D.Stream.all),
            Stream_Element_Array'
              (Translator.To_Stream_Element_Array
                 (Slice (Value, First, Last))));

         First := Last + 1;

         exit when First > Len;
      end loop;
   end Message_Body;

   ----------
   -- Mode --
   ----------

   procedure Mode
     (D     : in out Data;
      Value : Data_Mode) is
   begin
      D.Mode := Value;
   end Mode;

   -----------------
   -- Read_Header --
   -----------------

   procedure Read_Header
     (Socket : Net.Socket_Type'Class;
      D      : in out Data) is
   begin
      D.Header.Read (Socket);

      --  Set D.Content_Type with the value read from the socket

      D.Content_Type := To_Unbounded_String
        (Headers.Get (D.Header, Messages.Content_Type_Token));

      --  Set the Filename if any

      if AWS.Headers.Exist (D.Header, Messages.Content_Disposition_Token) then
         D.Filename := To_Unbounded_String
           (AWS.Headers.Values.Search
             (AWS.Headers.Get
               (D.Header, Messages.Content_Disposition_Token), "filename"));
      end if;
   end Read_Header;

   -----------------
   -- Status_Code --
   -----------------

   procedure Status_Code
     (D     : in out Data;
      Value : Messages.Status_Code) is
   begin
      D.Status_Code := Value;
   end Status_Code;

   ------------
   -- Stream --
   ------------

   procedure Stream
     (D        : in out Data;
      Handle   : not null access Resources.Streams.Stream_Type'Class;
      Encoding : Messages.Content_Encoding := Messages.Identity)
   is
      use type Messages.Content_Encoding;
   begin
      case Encoding is
         when Messages.GZip     =>
            D.Stream := Resources.Streams.ZLib.Deflate_Create
                          (Resources.Streams.Stream_Access (Handle),
                           Header => ZLib.GZip);
         when Messages.Deflate  =>
            D.Stream := Resources.Streams.ZLib.Deflate_Create
                          (Resources.Streams.Stream_Access (Handle),
                           Header => ZLib.Default);
         when Messages.Identity =>
            D.Stream := Resources.Streams.Stream_Access (Handle);
      end case;

      if Encoding /= Messages.Identity then
         --  We should not send "Content-Encoding: Identity" header line
         --
         --  RFC 2616
         --  3.5 Content Codings
         --  ...
         --  identity
         --  The default (identity) encoding; the use of no transformation
         --  whatsoever. This content-coding is used only in the Accept-
         --  Encoding header, and SHOULD NOT be used in the Content-Encoding
         --  header.

         Update_Header
           (D,
            Messages.Content_Encoding_Token,
            Ada.Characters.Handling.To_Lower
              (Messages.Content_Encoding'Image (Encoding)));

      elsif D.Header.Get (Messages.Content_Encoding_Token) /= "" then
         raise Constraint_Error
           with "Response content encoding is not supported.";
      end if;

      D.Mode := Stream;
   end Stream;

   -------------------
   -- Update_Header --
   -------------------

   procedure Update_Header
     (D     : in out Data;
      Name  : String;
      Value : String;
      N     : Positive := 1) is
   begin
      D.Header.Update (Name, Value, N);
   end Update_Header;

end AWS.Response.Set;
