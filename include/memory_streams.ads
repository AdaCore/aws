------------------------------------------------------------------------------
--                       Generic memory stream                              --
--                                                                          --
--                        Copyright (C) 2003                                --
--                        Dmitriy Anisimkov                                 --
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

generic
   type Element is private;
   type Element_Index  is range <>;
   type Element_Array is array (Element_Index range <>) of Element;
   type Element_Access is access all Element_Array;

   First_Block_Length : in Element_Index :=   256;
   Next_Block_Length  : in Element_Index := 1_024;

package Memory_Streams is

   type Stream_Type is limited private;

   subtype Element_Offset is Element_Index'Base range 0 .. Element_Index'Last;

   procedure Append
     (File  : in out Stream_Type;
      Value : in     Element_Array);
   --  Append the data to the resource. Raises Constraint_Error if ???

   function Size (File : in Stream_Type) return Element_Offset;
   --  Returns the size of the stream in bytes

   procedure Reset (File : in out Stream_Type);
   --  Set read index at the start of the stream, raises Constraint_Error if
   --  ???

   function End_Of_File (File : in Stream_Type) return Boolean;
   --  Returns true if there is no more data to read on the stream

   procedure Read
     (File   : in out Stream_Type;
      Buffer :    out Element_Array;
      Last   :    out Element_Offset);
   --  Read a chunk of data from File and put them into Buffer. Last is the
   --  index of the last item returned in Buffer. Raises Constraint_Error if
   --  ???

   procedure Close (File : in out Stream_Type);
   --  Close File, raises Program_Error if ???

   procedure Clear (File : in out Stream_Type) renames Close;

   ---------------------------------
   --  Fill by pointer interface. --
   ---------------------------------

   procedure Get_Pointer
     (File    : in out Stream_Type;
      Pointer :    out Element_Access);
   --  ???

   procedure Back_Pointer
     (File   : in out Stream_Type;
      Length : in     Element_Offset);
   --  ???

private

   type Buffer_Type;

   type Buffer_Access is access all Buffer_Type;

   type Buffer_Type is record
      Data : Element_Access;
      Next : Buffer_Access;
   end record;

   type Stream_Type is limited record
      First          : Buffer_Access;
      Current        : Buffer_Access;
      Last           : Buffer_Access;
      Last_Length    : Element_Offset;
      Length         : Element_Offset := 0;
      Current_Offset : Element_Index  := 1;
      Pointer        : Element_Access;
   end record;

end Memory_Streams;
