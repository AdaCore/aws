------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2002                          --
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

with AWS.Translator;

package body AWS.Response.Set is

   --------------------
   -- Authentication --
   --------------------

   procedure Authentication
     (D     : in out Data;
      Realm : in String;
      Mode  : in Authentication_Mode := Basic;
      Stale : in Boolean             := False)
   is
   begin
      D.Realm          := To_Unbounded_String (Realm);
      D.Authentication := Mode;
      D.Auth_Stale     := Stale;
      D.Status_Code    := Messages.S401;
   end Authentication;

   ------------------
   -- Content_Type --
   ------------------

   procedure Content_Type
     (D     : in out Data;
      Value : in     String)
   is
   begin
      D.Content_Type := To_Unbounded_String (Value);
   end Content_Type;

   --------------
   -- Filename --
   --------------

   procedure Filename
     (D     : in out Data;
      Value : in     String)
   is
   begin
      D.Filename       := To_Unbounded_String (Value);
      D.Mode           := File;
      D.Content_Length := Integer (Resources.File_Size (Value));
   end Filename;

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid (D : in Data) return Boolean is
      use type Messages.Status_Code;
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
                  => Redirection_Code := True;
      when others => Redirection_Code := False;
      end case;
      return (Redirection_Code xor D.Location = Null_Unbounded_String)
         and (D.Status_Code = Messages.S401
                 xor D.Realm = Null_Unbounded_String);
   end Is_Valid;

   --------------
   -- Location --
   --------------

   procedure Location
     (D     : in out Data;
      Value : in     String)
   is
   begin
      D.Location := To_Unbounded_String (Value);
   end Location;

   ------------------
   -- Message_Body --
   ------------------

   procedure Message_Body
     (D     : in out Data;
      Value : in     Streams.Stream_Element_Array)
   is
   begin
      D.Message_Body   := new Streams.Stream_Element_Array'(Value);
      D.Content_Length := Value'Length;
      D.Mode           := Message;
   end Message_Body;

   procedure Message_Body
     (D     : in out Data;
      Value : in     String)
   is
   begin
      Message_Body (D, Translator.To_Stream_Element_Array (Value));
   end Message_Body;

   procedure Message_Body
     (D     : in out Data;
      Value : in     Strings.Unbounded.Unbounded_String)
   is
   begin
      Message_Body (D, To_String (Value));
   end Message_Body;

   ----------
   -- Mode --
   ----------

   procedure Mode
     (D     : in out Data;
      Value : in     Data_Mode)
   is
   begin
      D.Mode := Value;
   end Mode;

   -----------------
   -- Status_Code --
   -----------------

   procedure Status_Code
     (D     : in out Data;
      Value : in     Messages.Status_Code)
   is
   begin
      D.Status_Code := Value;
   end Status_Code;

   ------------
   -- Stream --
   ------------

   procedure Stream
     (D              : in out Data;
      Handle         : in     Resources.Streams.Stream_Access;
      Content_Length : in     Content_Length_Type)
   is
   begin
      D.Stream         := Handle;
      D.Content_Length := Content_Length;
      D.Mode           := Stream;
   end Stream;

end AWS.Response.Set;

