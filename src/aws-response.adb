------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2001                          --
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

--  $Id$

with Ada.Strings.Fixed;
with Ada.Unchecked_Deallocation;

with AWS.Translator;
with AWS.Resources.Embedded;

package body AWS.Response is

   use Streams;

   -----------------
   -- Acknowledge --
   -----------------

   function Acknowledge
     (Status_Code  : in Messages.Status_Code;
      Message_Body : in String := "";
      Content_Type : in String := MIME.Text_HTML)
      return Data is
   begin
      if Message_Body = "" then
         return Data'(Finalization.Controlled with
                      new Natural'(1),
                      Header,
                      Status_Code,
                      0,
                      Content_Type   => Null_Unbounded_String,
                      Filename       => Null_Unbounded_String,
                      Location       => Null_Unbounded_String,
                      Realm          => Null_Unbounded_String,
                      Authentication => Any,
                      Auth_Stale     => False,
                      Stream         => null,
                      Message_Body   => null);
      else
         return Data'(Finalization.Controlled with
                      new Natural'(1),
                      Message,
                      Status_Code,
                      Message_Body'Length,
                      Content_Type   => To_Unbounded_String (Content_Type),
                      Filename       => Null_Unbounded_String,
                      Location       => Null_Unbounded_String,
                      Realm          => Null_Unbounded_String,
                      Authentication => Any,
                      Auth_Stale     => False,
                      Stream         => null,
                      Message_Body   => new Stream_Element_Array'
                        (Translator.To_Stream_Element_Array (Message_Body)));
      end if;
   end Acknowledge;

   ------------
   -- Adjust --
   ------------

   procedure Adjust (Object : in out Data) is
   begin
      Object.Ref_Counter.all := Object.Ref_Counter.all + 1;
   end Adjust;

   ------------------
   -- Authenticate --
   ------------------

   function Authenticate
     (Realm : in String;
      Mode  : in Authentication_Mode := Basic;
      Stale : in Boolean             := False)
      return Data
   is
      CRLF : constant String := ASCII.CR & ASCII.LF;

      Auth_Mess : constant String
        := "<HTML><HEAD>" & CRLF
        & "<TITLE>401 Authorization Required</TITLE>" & CRLF
        & "</HEAD><BODY>" & CRLF
        & "<H1>Authorization Required</H1>" & CRLF
        & "This server could not verify that you" & CRLF
        & "are authorized to access the document you" & CRLF
        & "requested.  Either you supplied the wrong" & CRLF
        & "credentials (e.g., bad password), or your" & CRLF
        & "browser doesn't understand how to supply" & CRLF
        & "the credentials required.<P>" & CRLF
        & "</BODY></HTML>" & CRLF;
   begin
      return Data'(Finalization.Controlled with
                   new Natural'(1),
                   Message,
                   Messages.S401,
                   Auth_Mess'Length,
                   Content_Type   => To_Unbounded_String (AWS.MIME.Text_HTML),
                   Filename       => Null_Unbounded_String,
                   Location       => Null_Unbounded_String,
                   Realm          => To_Unbounded_String (Realm),
                   Authentication => Mode,
                   Auth_Stale     => Stale,
                   Stream         => null,
                   Message_Body   => new Stream_Element_Array'
                     (Translator.To_Stream_Element_Array (Auth_Mess)));
   end Authenticate;

   --------------------
   -- Authentication --
   --------------------

   function Authentication (D : in Data) return Authentication_Mode is
   begin
      return D.Authentication;
   end Authentication;

   --------------------------
   -- Authentication_Stale --
   --------------------------

   function Authentication_Stale  (D : in Data) return Boolean is
   begin
      return D.Auth_Stale;
   end Authentication_Stale;

   -----------
   -- Build --
   -----------

   function Build
     (Content_Type : in String;
      Message_Body : in String;
      Status_Code  : in Messages.Status_Code := Messages.S200)
      return Data is
   begin
      return Data'(Finalization.Controlled with
                   new Natural'(1),
                   Message,
                   Status_Code,
                   Message_Body'Length,
                   Content_Type   => To_Unbounded_String (Content_Type),
                   Filename       => Null_Unbounded_String,
                   Location       => Null_Unbounded_String,
                   Realm          => Null_Unbounded_String,
                   Authentication => Any,
                   Auth_Stale     => False,
                   Stream         => null,
                   Message_Body   => new Stream_Element_Array'
                     (Translator.To_Stream_Element_Array (Message_Body)));
   end Build;

   function Build
     (Content_Type    : in String;
      UString_Message : in Strings.Unbounded.Unbounded_String;
      Status_Code     : in Messages.Status_Code := Messages.S200)
      return Data
   is
      Message_Body : constant String := To_String (UString_Message);
   begin
      return Data'(Finalization.Controlled with
                   new Natural'(1),
                   Message,
                   Status_Code,
                   Length (UString_Message),
                   Content_Type   => To_Unbounded_String (Content_Type),
                   Filename       => Null_Unbounded_String,
                   Location       => Null_Unbounded_String,
                   Realm          => Null_Unbounded_String,
                   Authentication => Any,
                   Auth_Stale     => False,
                   Stream         => null,
                   Message_Body   => new Stream_Element_Array'
                     (Translator.To_Stream_Element_Array (Message_Body)));
   end Build;

   function Build
     (Content_Type : in String;
      Message_Body : in Streams.Stream_Element_Array;
      Status_Code  : in Messages.Status_Code := Messages.S200)
      return Data is
   begin
      return Data'(Finalization.Controlled with
                   new Natural'(1),
                   Message,
                   Status_Code,
                   Message_Body'Length,
                   Content_Type   => To_Unbounded_String (Content_Type),
                   Filename       => Null_Unbounded_String,
                   Location       => Null_Unbounded_String,
                   Realm          => Null_Unbounded_String,
                   Authentication => Any,
                   Auth_Stale     => False,
                   Stream         => null,
                   Message_Body   =>
                     new Streams.Stream_Element_Array'(Message_Body));
   end Build;

   --------------------
   -- Content_Length --
   --------------------

   function Content_Length (D : in Data) return Content_Length_Type is
   begin
      return D.Content_Length;
   end Content_Length;

   ------------------
   -- Content_Type --
   ------------------

   function Content_Type (D : in Data) return String is
   begin
      return To_String (D.Content_Type);
   end Content_Type;

   ----------------------
   -- Create_Resource --
   ----------------------

   procedure Create_Resource
     (File :    out AWS.Resources.File_Type;
      D    : in     Data)
   is
      use AWS.Resources;
   begin
      case D.Mode is
         when Response.File =>
            Open (File, Filename (D), "shared=no");

         when Response.Stream =>
            Resources.Streams.Create (File, D.Stream);

         when Response.Message =>
            Embedded.Create (File, Embedded.Buffer_Access (D.Message_Body));

         when others =>
            --  Should not be called for others response modes.
            raise Constraint_Error;
      end case;
   end Create_Resource;

   -----------
   -- Empty --
   -----------

   function Empty return Data is
   begin
      return Data'(Finalization.Controlled with
                   new Natural'(1),
                   Response.No_Data,
                   Messages.S204,
                   0,
                   Null_Unbounded_String,
                   Null_Unbounded_String,
                   Null_Unbounded_String,
                   Null_Unbounded_String,
                   Any,
                   False,
                   null,
                   null);
   end Empty;

   ----------
   -- File --
   ----------

   function File
     (Content_Type : in String;
      Filename     : in String;
      Status_Code  : in Messages.Status_Code := Messages.S200)
      return Data is
   begin
      return Data'(Finalization.Controlled with
                   new Natural'(1),
                   File,
                   Status_Code,
                   Integer (Resources.File_Size (Filename)),
                   Content_Type   => To_Unbounded_String (Content_Type),
                   Filename       => To_Unbounded_String (Filename),
                   Location       => Null_Unbounded_String,
                   Realm          => Null_Unbounded_String,
                   Authentication => Any,
                   Auth_Stale     => False,
                   Stream         => null,
                   Message_Body   => null);
   exception
      when Resources.Resource_Error =>
         return Acknowledge (Messages.S404, "<p> " & Filename & " not found");
   end File;

   --------------
   -- Filename --
   --------------

   function Filename (D : in Data) return String is
   begin
      return To_String (D.Filename);
   end Filename;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Object : in out Data) is

      procedure Free is new Ada.Unchecked_Deallocation
        (Streams.Stream_Element_Array, Stream_Element_Array_Access);

      procedure Free is new Ada.Unchecked_Deallocation
        (Natural, Natural_Access);

   begin
      Object.Ref_Counter.all := Object.Ref_Counter.all - 1;

      if Object.Ref_Counter.all = 0 then
         Free (Object.Ref_Counter);
         Free (Object.Message_Body);
      end if;
   end Finalize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Object : in out Data) is
   begin
      Object.Ref_Counter := new Natural'(1);
   end Initialize;

   --------------
   -- Location --
   --------------

   function Location (D : in Data) return String is
   begin
      return To_String (D.Location);
   end Location;

   ------------------
   -- Message_Body --
   ------------------

   function Message_Body (D : in Data) return String is
   begin
      if D.Message_Body = null then
         return "";
      else
         return Translator.To_String (D.Message_Body.all);
      end if;
   end Message_Body;

   function Message_Body (D : in Data) return Unbounded_String is
   begin
      if D.Message_Body = null then
         return Null_Unbounded_String;
      else
         return To_Unbounded_String
           (Translator.To_String (D.Message_Body.all));
      end if;
   end Message_Body;

   function Message_Body (D : in Data) return Streams.Stream_Element_Array is
      No_Data : constant Streams.Stream_Element_Array := (1 .. 0 => 0);
   begin
      if D.Message_Body = null then
         return No_Data;
      else
         return D.Message_Body.all;
      end if;
   end Message_Body;

   ----------
   -- Mode --
   ----------

   function Mode (D : in Data) return Data_Mode is
   begin
      return D.Mode;
   end Mode;

   -----------
   -- Moved --
   -----------

   function Moved
     (Location : in String;
      Message  : in String := Default_Moved_Message)
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

   begin
      return Data'(Finalization.Controlled with
                   new Natural'(1),
                   Response.Message,
                   Messages.S301,
                   Message_Body'Length,
                   Content_Type   => To_Unbounded_String (AWS.MIME.Text_HTML),
                   Filename       => Null_Unbounded_String,
                   Location       => To_Unbounded_String (Location),
                   Realm          => Null_Unbounded_String,
                   Authentication => Any,
                   Auth_Stale     => False,
                   Stream         => null,
                   Message_Body   => new Stream_Element_Array'
                     (Translator.To_Stream_Element_Array (Message_Body)));
   end Moved;

   -----------
   -- Realm --
   -----------

   function Realm (D : in Data) return String is
   begin
      return To_String (D.Realm);
   end Realm;

   ------------------
   -- Socket_Taken --
   ------------------

   function Socket_Taken return Data is
   begin
      return Data'(Finalization.Controlled with
                   new Natural'(1),
                   Response.Socket_Taken,
                   Messages.S200,
                   0,
                   Null_Unbounded_String,
                   Null_Unbounded_String,
                   Null_Unbounded_String,
                   Null_Unbounded_String,
                   Any,
                   False,
                   null,
                   null);
   end Socket_Taken;

   -----------------
   -- Status_Code --
   -----------------

   function Status_Code (D : in Data) return Messages.Status_Code is
   begin
      return D.Status_Code;
   end Status_Code;

   ------------
   -- Stream --
   ------------

   function Stream
     (Content_Type  : in String;
      Stream_Handle : in Resources.Streams.Stream_Access;
      Stream_Size   : in Content_Length_Type;
      Status_Code   : in Messages.Status_Code := Messages.S200)
      return Data is
   begin
      return Data'(Finalization.Controlled with
                   new Natural'(1),
                   Stream,
                   Status_Code,
                   Stream_Size,
                   Content_Type   => To_Unbounded_String (Content_Type),
                   Filename       => Null_Unbounded_String,
                   Location       => Null_Unbounded_String,
                   Realm          => Null_Unbounded_String,
                   Authentication => Any,
                   Auth_Stale     => False,
                   Stream         => Stream_Handle,
                   Message_Body   => null);
   end Stream;

   ---------
   -- URL --
   ---------

   function URL (Location : in String) return Data is
   begin
      return Data'(Finalization.Controlled with
                   new Natural'(1),
                   Response.Message,
                   Messages.S301,
                   0,
                   Content_Type   => Null_Unbounded_String,
                   Filename       => Null_Unbounded_String,
                   Location       => To_Unbounded_String (Location),
                   Realm          => Null_Unbounded_String,
                   Authentication => Any,
                   Auth_Stale     => False,
                   Stream         => null,
                   Message_Body   => null);
   end URL;

end AWS.Response;
