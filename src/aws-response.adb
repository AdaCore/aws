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

with AWS.OS_Lib;

package body AWS.Response is

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
                      Null_Unbounded_String,
                      Null_Unbounded_String,
                      Null_Unbounded_String,
                      Null_Unbounded_String,
                      null);
      else
         return Data'(Finalization.Controlled with
                      new Natural'(1),
                      Message,
                      Status_Code,
                      Message_Body'Length,
                      To_Unbounded_String (Content_Type),
                      To_Unbounded_String (Message_Body),
                      Null_Unbounded_String,
                      Null_Unbounded_String,
                      null);
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

   function Authenticate (Realm : in String) return Data is

      CRLF : constant String := ASCII.CR & ASCII.LF;

      Auth_Mess : constant String :=
        "<HTML><HEAD>" & CRLF
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
                   To_Unbounded_String (AWS.MIME.Text_HTML),
                   To_Unbounded_String (Auth_Mess),
                   Null_Unbounded_String,
                   To_Unbounded_String (Realm),
                   null);
   end Authenticate;

   ------------
   -- Binary --
   ------------

   function Binary (D : in Data) return Streams.Stream_Element_Array is
      No_Data : constant Streams.Stream_Element_Array := (1 .. 0 => 0);
   begin
      if D.Elements = null then
         return No_Data;
      else
         return D.Elements.all;
      end if;
   end Binary;

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
                   To_Unbounded_String (Content_Type),
                   To_Unbounded_String (Message_Body),
                   Null_Unbounded_String,
                   Null_Unbounded_String,
                   null);
   end Build;

   function Build
     (Content_Type    : in String;
      UString_Message : in Strings.Unbounded.Unbounded_String;
      Status_Code     : in Messages.Status_Code := Messages.S200)
     return Data is
   begin
      return Data'(Finalization.Controlled with
                   new Natural'(1),
                   Message,
                   Status_Code,
                   Length (UString_Message),
                   To_Unbounded_String (Content_Type),
                   UString_Message,
                   Null_Unbounded_String,
                   Null_Unbounded_String,
                   null);
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
                   To_Unbounded_String (Content_Type),
                   Null_Unbounded_String,
                   Null_Unbounded_String,
                   Null_Unbounded_String,
                   new Streams.Stream_Element_Array'(Message_Body));
   end Build;

   --------------------
   -- Content_Length --
   --------------------

   function Content_Length (D : in Data) return Natural is
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

   ----------
   -- File --
   ----------

   function File
     (Content_Type : in String;
      Filename     : in String)
     return Data is
   begin
      return Data'(Finalization.Controlled with
                   new Natural'(1),
                   File,
                   Messages.S200,
                   Integer (OS_Lib.File_Size (Filename)),
                   To_Unbounded_String (Content_Type),
                   To_Unbounded_String (Filename),
                   Null_Unbounded_String,
                   Null_Unbounded_String,
                   null);
   end File;

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
         Free (Object.Elements);
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
      return To_String (D.Message_Body);
   end Message_Body;

   function Message_Body (D : in Data) return Unbounded_String is
   begin
      return D.Message_Body;
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
     (Location     : in String;
      Message      : in String := Default_Moved_Message)
     return Data
   is
      use Ada.Strings;

      function Build_Message_Body return String;
      --  Return proper message body using Message template. It replaces _@_
      --  in Message by Location.

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
                   To_Unbounded_String (AWS.MIME.Text_HTML),
                   To_Unbounded_String (Message_Body),
                   To_Unbounded_String (Location),
                   Null_Unbounded_String,
                   null);
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
                   null);
   end Socket_Taken;

   -----------------
   -- Status_Code --
   -----------------

   function Status_Code (D : in Data) return Messages.Status_Code is
   begin
      return D.Status_Code;
   end Status_Code;

   ---------
   -- URL --
   ---------

   function URL (Location : in String)
     return Data is
   begin
      return Data'(Finalization.Controlled with
                   new Natural'(1),
                   Response.Message,
                   Messages.S301,
                   0,
                   Null_Unbounded_String,
                   Null_Unbounded_String,
                   To_Unbounded_String (Location),
                   Null_Unbounded_String,
                   null);
   end URL;

end AWS.Response;
