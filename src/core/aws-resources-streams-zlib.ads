------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2003-2012, AdaCore                     --
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

with ZLib;

package AWS.Resources.Streams.ZLib is

   type Stream_Type is new Streams.Stream_Type with private;

   package ZL renames Standard.ZLib;

   subtype Window_Bits_Type   is ZL.Window_Bits_Type;
   subtype Header_Type        is ZL.Header_Type;
   subtype Compression_Level  is ZL.Compression_Level;
   subtype Strategy_Type      is ZL.Strategy_Type;
   subtype Compression_Method is ZL.Compression_Method;
   subtype Memory_Level_Type  is ZL.Memory_Level_Type;

   Default_Compression : constant Compression_Level := ZL.Default_Compression;

   procedure Deflate_Initialize
     (Resource     : in out Stream_Type;
      Source       : Streams.Stream_Access;
      Level        : Compression_Level  := ZL.Default_Compression;
      Strategy     : Strategy_Type      := ZL.Default_Strategy;
      Method       : Compression_Method := ZL.Deflated;
      Window_Bits  : Window_Bits_Type   := ZL.Default_Window_Bits;
      Memory_Level : Memory_Level_Type  := ZL.Default_Memory_Level;
      Header       : Header_Type        := ZL.Default);
   --  Initialize the compression of the user defined stream

   procedure Inflate_Initialize
     (Resource    : in out Stream_Type;
      Source      : Streams.Stream_Access;
      Window_Bits : Window_Bits_Type := ZL.Default_Window_Bits;
      Header      : Header_Type      := ZL.Default);
   --  Initialize the decompression of the user defined stream

   function Deflate_Create
     (Source       : Streams.Stream_Access;
      Level        : Compression_Level     := ZL.Default_Compression;
      Strategy     : Strategy_Type         := ZL.Default_Strategy;
      Method       : Compression_Method    := ZL.Deflated;
      Window_Bits  : Window_Bits_Type      := ZL.Default_Window_Bits;
      Memory_Level : Memory_Level_Type     := ZL.Default_Memory_Level;
      Header       : Header_Type           := ZL.Default)
      return Stream_Access;
   --  Creates dynamically allocated Stream_Type and initialize the compression
   --  of the user defined stream.

   function Inflate_Create
     (Source       : Streams.Stream_Access;
      Window_Bits  : Window_Bits_Type      := ZL.Default_Window_Bits;
      Header       : Header_Type           := ZL.Default)
      return Stream_Access;
   --  Creates dynamically allocated Stream_Type and initialize the
   --  decompression of the user defined stream.

   overriding procedure Read
     (Resource : in out Stream_Type;
      Buffer   : out Stream_Element_Array;
      Last     : out Stream_Element_Offset);
   --  Get the source data from the Source stream, encode it, and return
   --  encoded data in the Buffer.

   overriding procedure Close (Resource : in out Stream_Type);

   overriding procedure Reset (Resource : in out Stream_Type);

   overriding procedure Set_Index
     (Resource : in out Stream_Type;
      To       : Stream_Element_Offset);

   overriding function End_Of_File (Resource : Stream_Type) return Boolean;
   --  Returns true if there is no more data to read

private

   type Stream_Type is new Streams.Stream_Type with record
      Source      : Resources.File_Type;
      End_Of_File : Boolean;
      Filter      : ZL.Filter_Type;
      Buffer      : Stream_Element_Array (1 .. 4096);
      Rest_First  : Stream_Element_Offset;
      Rest_Last   : Stream_Element_Offset;
   end record;

end AWS.Resources.Streams.ZLib;
