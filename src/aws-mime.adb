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

with Ada.Characters.Handling;
with Ada.Strings.Maps;
with Ada.Strings.Fixed;

package body AWS.MIME is

   use Ada;

   type String_Access is access constant String;

   type Mapping is record
      File_Type    : String_Access;
      Content_Type : String_Access;
   end record;

   --  extensions

   Dot_Html : aliased constant String := ".html";
   Dot_Htm  : aliased constant String := ".htm";
   Dot_XML  : aliased constant String := ".xml";
   Dot_Txt  : aliased constant String := ".txt";
   Dot_Ada  : aliased constant String := ".ada";
   Dot_Ads  : aliased constant String := ".ads";
   Dot_Adb  : aliased constant String := ".adb";
   Dot_C    : aliased constant String := ".c";
   Dot_H    : aliased constant String := ".h";
   Dot_Gif  : aliased constant String := ".gif";
   Dot_Jpg  : aliased constant String := ".jpg";
   Dot_Jpeg : aliased constant String := ".jpeg";
   Dot_Png  : aliased constant String := ".png";
   Dot_Ps   : aliased constant String := ".ps";
   Dot_Pdf  : aliased constant String := ".pdf";
   Dot_Zip  : aliased constant String := ".zip";
   Dot_Gz   : aliased constant String := ".gz";
   Dot_Tar  : aliased constant String := ".tar";
   Dot_Exe  : aliased constant String := ".exe";

   Type_Table : constant array (Positive range <>) of Mapping :=
     ((Dot_Html'Access, Text_HTML'Access),
      (Dot_Htm'Access,  Text_HTML'Access),
      (Dot_XML'Access,  Text_XML'Access),

      (Dot_Txt'Access,  Text_Plain'Access),
      (Dot_Ada'Access,  Text_Plain'Access),
      (Dot_Ads'Access,  Text_Plain'Access),
      (Dot_Adb'Access,  Text_Plain'Access),
      (Dot_C'Access,    Text_Plain'Access),
      (Dot_H'Access,    Text_Plain'Access),

      (Dot_Gif'Access,  Image_Gif'Access),
      (Dot_Jpg'Access,  Image_Jpeg'Access),
      (Dot_Jpeg'Access, Image_Jpeg'Access),
      (Dot_Png'Access,  Image_Png'Access),

      (Dot_Ps'Access,   Appl_Postscript'Access),
      (Dot_Pdf'Access,  Appl_Pdf'Access),
      (Dot_Zip'Access,  Appl_Zip'Access),

      (Dot_Gz'Access,   Appl_Octet_Stream'Access),
      (Dot_Tar'Access,  Appl_Octet_Stream'Access),
      (Dot_Exe'Access,  Appl_Octet_Stream'Access));

   --------------
   -- To_Lower --
   --------------

   function To_Lower (Item : in String) return String renames
     Ada.Characters.Handling.To_Lower;
   --  Just a shorter name

   -------------
   -- Content --
   -------------

   function Content_Type (Filename : in String) return String is

      Default_Content_Type : constant String := "application/octet-stream";
      Pos                  : Natural :=
        Strings.Fixed.Index (Filename,
                             Strings.Maps.To_Set ("."),
                             Going => Strings.Backward);
   begin
      if Pos > 0 then
         declare
            File_Type : String := To_Lower (Filename (Pos .. Filename'Last));
         begin
            for I in Type_Table'Range loop
               if File_Type = Type_Table (I).File_Type.all then
                  return Type_Table (I).Content_Type.all;
               end if;
            end loop;
         end;
      end if;
      return Default_Content_Type;
   end Content_Type;

end AWS.MIME;
