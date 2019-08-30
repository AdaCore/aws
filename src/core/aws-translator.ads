------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2019, AdaCore                     --
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

with Ada.Streams;
with Ada.Strings.Unbounded;

with AWS.Resources.Streams.Memory.ZLib;
with AWS.Utils;

package AWS.Translator is

   use Ada.Streams;
   use Ada.Strings.Unbounded;

   package ZL renames AWS.Resources.Streams.Memory.ZLib;

   ------------
   -- Base64 --
   ------------

   type Base64_Mode is (MIME, URL);
   --  Base64 encoding variants for encoding routines,
   --  RFC4648
   --  MIME - section 4
   --  URL  - section 5

   subtype Base64_Common is Character with
     Static_Predicate => Base64_Common
       in 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '=';

   subtype Base64_String is String with
     Dynamic_Predicate =>
       (for all C of Base64_String =>
          C in Base64_Common | '+' | '-' | '_' | '/');

   subtype Base64_UString is Unbounded_String with
     Dynamic_Predicate =>
       (for all K in 1 .. Length (Base64_UString) =>
          Element (Base64_UString, K)
            in Base64_Common | '+' | '-' | '_' | '/');

   --
   --  Decoding does not have to have Base64_Mode parameter, because data
   --  coding easy detected automatically.

   procedure Base64_Encode
     (Data     : Unbounded_String;
      B64_Data : out Base64_UString;
      Mode     : Base64_Mode := MIME)
   with
     Post =>
       (Mode = MIME
          and then
        (for all K in 1 .. Length (B64_Data) =>
            Element (B64_Data, K) not in '-' | '_'))
      or else
        (Mode = URL
           and then
         (for all K in 1 .. Length (B64_Data) =>
            Element (B64_Data, K) not in '+' | '/'));

   function Base64_Encode
     (Data : Stream_Element_Array;
      Mode : Base64_Mode := MIME) return Base64_String
   with
     Post =>
       (Mode = MIME
          and then
        (for all C of Base64_Encode'Result => C not in '-' | '_'))
      or else
        (Mode = URL
           and then
         (for all C of Base64_Encode'Result => C not in '+' | '/'));
   --  Encode Data using the base64 algorithm

   function Base64_Encode
     (Data : String; Mode : Base64_Mode := MIME) return Base64_String
   with
     Post =>
       (Mode = MIME
          and then
        (for all C of Base64_Encode'Result => C not in '-' | '_'))
      or else
        (Mode = URL
           and then
         (for all C of Base64_Encode'Result => C not in '+' | '/'));
   --  Same as above but takes a string as input

   procedure Base64_Decode
     (B64_Data : Base64_UString;
      Data     : out Unbounded_String);

   function Base64_Decode
     (B64_Data : Base64_String) return Stream_Element_Array;
   --  Decode B64_Data using the base64 algorithm

   function Base64_Decode (B64_Data : Base64_String) return String;

   --------
   -- QP --
   --------

   function QP_Decode (QP_Data : String) return String;
   --  Decode QP_Data using the Quoted Printable algorithm

   ------------------------------------
   -- String to Stream_Element_Array --
   ------------------------------------

   function To_String
     (Data : Stream_Element_Array) return String with Inline;
   --  Convert a Stream_Element_Array to a string. Note that as this routine
   --  returns a String it should not be used with large array as this could
   --  break the stack size limit. Use the routine below for large array.

   function To_Stream_Element_Array
     (Data : String) return Stream_Element_Array with Inline;
   --  Convert a String to a Stream_Element_Array

   function To_Stream_Element_Array
     (Data : String) return Utils.Stream_Element_Array_Access;
   --  As above but designed to be used for large objects

   function To_Unbounded_String
     (Data : Stream_Element_Array) return Unbounded_String;
   --  Convert a Stream_Element_Array to an Unbounded_String

   --------------------------
   --  Compress/Decompress --
   --------------------------

   subtype Compression_Level is ZL.Compression_Level;

   Default_Compression : constant Compression_Level := ZL.Default_Compression;

   function Compress
     (Data   : Stream_Element_Array;
      Level  : Compression_Level                := Default_Compression;
      Header : ZL.Header_Type                   := ZL.Default_Header)
      return Utils.Stream_Element_Array_Access;
   --  Returns Data compressed with a standard deflate algorithm based on the
   --  zlib library. The result is dynamically allocated and must be
   --  explicitly freed.

   function Decompress
     (Data   : Stream_Element_Array;
      Header : ZL.Header_Type                   := ZL.Default_Header)
      return Utils.Stream_Element_Array_Access;
   --  Returns Data decompressed based on the zlib library. The results is
   --  dynamically allocated and must be explicitly freed.

end AWS.Translator;
