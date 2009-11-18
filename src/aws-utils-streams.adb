------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                       Copyright (C) 2009, AdaCore                        --
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

with AWS.Translator;

package body AWS.Utils.Streams is

   ----------
   -- Open --
   ----------

   procedure Open
     (Stream : in out Strings'Class; Str : String) is
   begin
      Stream.Str        := To_Unbounded_String (Str);
      Stream.Read_Index := 1;
   end Open;

   ----------
   -- Read --
   ----------

   overriding procedure Read
     (Stream : in out Strings;
      Item   : out Stream_Element_Array;
      Last   : out Stream_Element_Offset)
   is
      Str : constant String := Slice
        (Stream.Str, Stream.Read_Index, Stream.Read_Index + Item'Length - 1);
      J   : Stream_Element_Offset := Item'First;
   begin
      for S in Str'Range loop
         Item (J) := Stream_Element (Character'Pos (Str (S)));
         J := J + 1;
      end loop;
      Last := Item'First + Str'Length - 1;
      Stream.Read_Index := Stream.Read_Index + Item'Length;
   end Read;

   overriding procedure Read
     (Stream : in out SHA1;
      Item   : out Stream_Element_Array;
      Last   : out Stream_Element_Offset) is
   begin
      Item := Translator.To_Stream_Element_Array (GNAT.SHA1.Digest (Stream.C));
      Last := Item'Last;
   end Read;

   -----------
   -- Value --
   -----------

   function Value (Stream : access Strings'Class) return String is
   begin
      return To_String (Stream.Str);
   end Value;

   function Value
     (Stream : access SHA1'Class) return GNAT.SHA1.Message_Digest
   is
      Result : GNAT.SHA1.Message_Digest;
   begin
      GNAT.SHA1.Message_Digest'Read (Stream, Result);
      return Result;
   end Value;

   -----------
   -- Write --
   -----------

   overriding procedure Write
     (Stream : in out Strings; Item : Stream_Element_Array)
   is
      Str : String (1 .. Integer (Item'Length));
      S   : Integer := Str'First;
   begin
      for J in Item'Range loop
         Str (S) := Character'Val (Item (J));
         S := S + 1;
      end loop;

      Append (Stream.Str, Str);
   end Write;

   overriding procedure Write
     (Stream : in out SHA1; Item : Stream_Element_Array) is
   begin
      GNAT.SHA1.Update (Stream.C, Item);
   end Write;

end AWS.Utils.Streams;
