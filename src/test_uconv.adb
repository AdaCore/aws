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

--  Detection of the fast unchecked conversion availability between
--  String and Stream_Element_Array. Copy file aws-translator-conversion-f.adb
--  to aws-translator-conversion.adb if the fast unchecked conversion
--  is supported on the target, or otherwise copy
--  aws-translator-conversion-p.adb to aws-translator-conversion.adb
--  if the fast unchecked conversion cannot be used.

with Ada.Unchecked_Conversion;
with Ada.Streams.Stream_IO; use Ada.Streams;

procedure Test_UConv is

   Sample : constant String :=
      "$Author$" & ASCII.LF &
      "$Date$" & ASCII.LF &
      "$Name$" & ASCII.LF &
      "$Locker$" & ASCII.LF &
      "$RCSfile$" & ASCII.LF &
      "$Revision$" & ASCII.LF &
      "$Source$" & ASCII.LF &
      "$State$" & ASCII.LF;

   Prefix : constant String := "aws-translator-conversion";
   Suffix : constant String := ".adb";
   Target : constant String := Prefix & Suffix;

   subtype Fixed_String is String (Sample'First .. Sample'Last);

   subtype Fixed_Array is Stream_Element_Array
     (Stream_Element_Offset (Sample'First)
      .. Stream_Element_Offset (Sample'Last));

   procedure Copy_File (Source : String);

   function To_Stream_Elements is
     new Ada.Unchecked_Conversion (Fixed_String, Fixed_Array);

   function To_String is
     new Ada.Unchecked_Conversion (Fixed_Array, Fixed_String);

   ---------------
   -- Copy_File --
   ---------------

   procedure Copy_File (Source : String) is
      use Ada.Streams.Stream_IO;
      Source_File : File_Type;
      Target_File : File_Type;
      Buffer      : Stream_Element_Array (0 .. 1024);
      Last        : Stream_Element_Offset;
   begin
      Open   (Source_File, In_File,  Source);
      Create (Target_File, Out_File, Target);

      loop
         Read  (Source_File, Buffer, Last);
         Write (Target_File, Buffer (0 .. Last));
         exit when Last < Buffer'Last;
      end loop;

      Close (Source_File);
      Close (Target_File);
   end Copy_File;

begin
   if Fixed_Array'Size = Fixed_String'Size
     and then Fixed_Array'Alignment = Fixed_String'Alignment
     and then Sample = To_String (To_Stream_Elements (Sample))
   then
      Copy_File (Prefix & "-f" & Suffix);
   else
      Copy_File (Prefix & "-p" & Suffix);
   end if;
end Test_UConv;
