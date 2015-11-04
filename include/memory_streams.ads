------------------------------------------------------------------------------
--                       Generic memory stream                              --
--                                                                          --
--                Copyright (C) 2003-2015, Dmitriy Anisimkov                --
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

generic
   type Element is private;
   type Element_Index  is range <>;
   type Element_Array is array (Element_Index range <>) of Element;
   type Element_Access is access Element_Array;
   type Constant_Access is access constant Element_Array;

   First_Block_Length : in Element_Index :=   256;
   Next_Block_Length  : in Element_Index := 1_024;

package Memory_Streams is

   type Stream_Type is limited private;

   subtype Element_Offset is Element_Index'Base range 0 .. Element_Index'Last;

   procedure Append
     (Stream : in out Stream_Type;
      Value  : in     Element_Array;
      Trim   : in     Boolean := False);
   --  Append the data to the resource.
   --  Set Trim to true disable remaining free spaces at the end of stream,
   --  Set Trim to true for every call to stream would decrease performance.

   procedure Append
     (Stream : in out Stream_Type;
      Data   : in     Element_Access);
   --  Append dynamically allocated data or access to the static data
   --  to the stream. Application must not use Data after send it to the
   --  Stream. Stream would care about it, and free when necessary.

   procedure Append
     (Stream : in out Stream_Type;
      Data   : in     Constant_Access);
   --  Append dynamically allocated data or access to the static data
   --  to the stream. Application could use Data after send it to the
   --  Stream.

   function Size (Stream : in Stream_Type) return Element_Offset;
   --  Returns the size of the stream in bytes (total number of bytes)

   function Pending (Stream : in Stream_Type) return Element_Offset;
   --  Returns the number of byte from current position to the end

   procedure Reset (Stream : in out Stream_Type);
   --  Set read index at the start of the stream

   procedure Set_Index
     (Stream : in out Stream_Type;
      To     : in     Element_Offset);
   --  Set the position in the stream, next Read will start at the position
   --  whose index is To. If To is outside the content the index is set to
   --  Last + 1 to ensure that next End_Of_File will return True.

   function End_Of_File (Stream : in Stream_Type) return Boolean;
   --  Returns true if there is no more data to read on the stream

   procedure Read
     (Stream : in out Stream_Type;
      Buffer :    out Element_Array;
      Last   :    out Element_Offset);
   --  Read a chunk of data from File and put them into Buffer. Last is the
   --  index of the last item returned in Buffer.

   procedure Close (Stream : in out Stream_Type);
   --  Close File, release all data currently in this stream

   procedure Clear (Stream : in out Stream_Type) renames Close;

private

   type Buffer_Type;

   type Buffer_Access is access all Buffer_Type;

   type Buffer_Type (Steady : Boolean) is record
      Next : Buffer_Access;
      case Steady is
         when True  => Const : Constant_Access;
         when False => Data  : Element_Access;
      end case;
   end record;

   type Stream_Type is limited record
      First          : Buffer_Access;
      Current        : Buffer_Access;
      Last           : Buffer_Access;
      Last_Length    : Element_Offset := 0;
      Length         : Element_Offset := 0;
      Current_Offset : Element_Index  := 1;
   end record;

end Memory_Streams;
