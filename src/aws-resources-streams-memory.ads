------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2002-2003                          --
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

with Memory_Streams;

package AWS.Resources.Streams.Memory is

   use Ada;

   type Stream_Type is tagged limited private;

   function End_Of_File (Resource : in Stream_Type) return Boolean;

   procedure Append
     (Resource : in out Stream_Type;
      Buffer   : in     Stream_Element_Array);

   procedure Read
     (Resource : in out Stream_Type;
      Buffer   :    out Stream_Element_Array;
      Last     :    out Stream_Element_Offset);

   procedure Close (Resource : in out Stream_Type);

   function Size (Resource : in Stream_Type) return Stream_Element_Offset;

private

   type Stream_Element_Access is access all Stream_Element_Array;

   package Containers is
      new Memory_Streams (Element        => Stream_Element,
                          Element_Index  => Stream_Element_Offset,
                          Element_Array  => Stream_Element_Array,
                          Element_Access => Stream_Element_Access);

   type Stream_Type is new Streams.Stream_Type with record
      Data : Containers.Stream_Type;
   end record;

end AWS.Resources.Streams.Memory;
