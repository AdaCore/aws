------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                            Copyright (C) 2003                            --
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

--  $RCSfile$
--  $Revision$
--  $Date$
--  $Author$

--  API to handle a memory stream. A memory stream is first created
--  empty. User can add chunk of data using the Append routines. The stream
--  is then read using the Read procedure.

with AWS.Utils;
with Memory_Streams;

package AWS.Resources.Streams.Memory is

   use Ada;

   type Stream_Type is new Streams.Stream_Type with private;

   subtype Stream_Element_Access is Utils.Stream_Element_Array_Access;

   procedure Append
     (Resource : in out Stream_Type;
      Buffer   : in     Stream_Element_Array);
   --  Append Buffer into the memory stream

   procedure Read
     (Resource : in out Stream_Type;
      Buffer   :    out Stream_Element_Array;
      Last     :    out Stream_Element_Offset);
   --  Returns a chunck of data in Buffer, Last point to the last element
   --  returned in Buffer.

   function End_Of_File (Resource : in Stream_Type) return Boolean;
   --  Returns True if the end of the memory stream has been reached

   procedure Clear (Resource : in out Stream_Type);
   pragma Inline (Clear);
   --  Delete all data from memory stream

   procedure Reset (Resource : in out Stream_Type);
   --  Reset the streaming data to the first position

   function Size (Resource : in Stream_Type) return Stream_Element_Offset;
   --  Returns the number of bytes in the memory stream

   procedure Close (Resource : in out Stream_Type);
   --  Close the memory stream. Release all memory associated with the stream

private

   package Containers is
      new Memory_Streams (Element        => Stream_Element,
                          Element_Index  => Stream_Element_Offset,
                          Element_Array  => Stream_Element_Array,
                          Element_Access => Stream_Element_Access);

   type Stream_Type is new Streams.Stream_Type with record
      Data : Containers.Stream_Type;
   end record;

end AWS.Resources.Streams.Memory;
