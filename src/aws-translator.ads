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

with Ada.Streams;
with Ada.Strings.Unbounded;

with AWS.Resources.Streams.Memory.ZLib;
with AWS.Utils;

package AWS.Translator is

   use Ada.Strings.Unbounded;

   package ZL renames AWS.Resources.Streams.Memory.ZLib;

   ------------
   -- Base64 --
   ------------

   procedure Base64_Encode
     (Data     : in     Unbounded_String;
      B64_Data :    out Unbounded_String);

   function Base64_Encode
     (Data : in Ada.Streams.Stream_Element_Array) return String;
   --  Encode Data using the base64 algorithm

   function Base64_Encode (Data : in String) return String;
   --  Same as above but takes a string as input

   procedure Base64_Decode
     (B64_Data : in     Unbounded_String;
      Data     :    out Unbounded_String);

   function Base64_Decode
     (B64_Data : in String) return Ada.Streams.Stream_Element_Array;
   --  Decode B64_Data using the base64 algorithm

   --------
   -- QP --
   --------

   function QP_Decode (QP_Data : in String) return String;
   --  Decode QP_Data using the Quoted Printable algorithm

   ------------------------------------
   -- String to Stream_Element_Array --
   ------------------------------------

   function To_String
     (Data : in Ada.Streams.Stream_Element_Array) return String;
   pragma Inline (To_String);
   --  Convert a Stream_Element_Array to a string. Note that as this routine
   --  returns a String it should not be used with large array as this could
   --  break the stack size limit. Use the routine below for large array.

   function To_Stream_Element_Array
     (Data : in String) return Ada.Streams.Stream_Element_Array;
   pragma Inline (To_Stream_Element_Array);
   --  Convert a String to a Stream_Element_Array

   function To_Unbounded_String
     (Data : in Ada.Streams.Stream_Element_Array)
      return Ada.Strings.Unbounded.Unbounded_String;
   --  Convert a Stream_Element_Array to an Unbounded_String

   --------------------------
   --  Compress/Decompress --
   --------------------------

   subtype Compression_Level is ZL.Compression_Level;

   Default_Compression : constant Compression_Level := ZL.Default_Compression;

   function Compress
     (Data   : in Ada.Streams.Stream_Element_Array;
      Level  : in Compression_Level                := Default_Compression;
      Header : in ZL.Header_Type                   := ZL.Default_Header)
      return Utils.Stream_Element_Array_Access;
   --  Returns Data compressed with a standard deflate algorithm based on the
   --  zlib library. The result is dynamically allocated and must be
   --  explicitly freed.

   function Decompress
     (Data   : in Ada.Streams.Stream_Element_Array;
      Header : in ZL.Header_Type                   := ZL.Default_Header)
      return Utils.Stream_Element_Array_Access;
   --  Returns Data decompressed based on the zlib library. The results is
   --  dynamically allocated and must be explicitly freed.

end AWS.Translator;
