------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2003-2013, AdaCore                     --
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
--  covered by the GNU Public License.                                      --
------------------------------------------------------------------------------

pragma Ada_2012;

--  This API is used as for standard memory stream (see parent package), the
--  only difference is that the stream is compressing/decompressing on append.

with ZLib;

package AWS.Resources.Streams.Memory.ZLib is

   package ZL renames Standard.ZLib;

   type Stream_Type is new Memory.Stream_Type with private;

   subtype Window_Bits_Type   is ZL.Window_Bits_Type;
   subtype Header_Type        is ZL.Header_Type;
   subtype Compression_Level  is ZL.Compression_Level;
   subtype Strategy_Type      is ZL.Strategy_Type;
   subtype Compression_Method is ZL.Compression_Method;
   subtype Memory_Level_Type  is ZL.Memory_Level_Type;

   Default_Compression : constant Compression_Level := ZL.Default_Compression;
   Default_Header      : constant Header_Type       := ZL.Default;

   procedure Deflate_Initialize
     (Resource     : in out Stream_Type;
      Level        : Compression_Level  := ZL.Default_Compression;
      Strategy     : Strategy_Type      := ZL.Default_Strategy;
      Method       : Compression_Method := ZL.Deflated;
      Window_Bits  : Window_Bits_Type   := ZL.Default_Window_Bits;
      Memory_Level : Memory_Level_Type  := ZL.Default_Memory_Level;
      Header       : Header_Type        := ZL.Default)
     with Inline;
   --  Initialize the compression

   procedure Inflate_Initialize
     (Resource    : in out Stream_Type;
      Window_Bits : Window_Bits_Type := ZL.Default_Window_Bits;
      Header      : Header_Type      := ZL.Default)
     with Inline;
   --  Initialize the decompression

   overriding procedure Append
     (Resource : in out Stream_Type;
      Buffer   : Stream_Element_Array;
      Trim     : Boolean := False);
   --  Compress/decompress and Append Buffer into the memory stream

   overriding procedure Read
     (Resource : in out Stream_Type;
      Buffer   : out Stream_Element_Array;
      Last     : out Stream_Element_Offset);
   --  Returns a chunck of data in Buffer, Last point to the last element
   --  returned in Buffer.

   overriding function Size
     (Resource : Stream_Type) return Stream_Element_Offset;
   --  Returns the number of bytes in the memory stream

   overriding procedure Close (Resource : in out Stream_Type);
   --  Close the ZLib stream, release all memory associated with the Resource
   --  object.

private

   type Stream_Access is access all Stream_Type;

   type Stream_Type is new Memory.Stream_Type with record
      Self    : Stream_Access := Stream_Type'Unchecked_Access;
      --  We need it for auto flush in the Size routine call

      Filter  : ZL.Filter_Type;
      Flushed : Boolean;
   end record;

end AWS.Resources.Streams.Memory.ZLib;
