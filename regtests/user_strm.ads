------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2003                          --
--                               ACT-Europe                                 --
--                                                                          --
--  Authors: Dmitriy Anisimokv - Pascal Obry                                --
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

--  Test for user defined stream.

with Ada.Streams;

with AWS.Resources.Streams;

package User_Strm is

   use AWS.Resources;
   use Ada.Streams;

   type File_Tagged is new Streams.Stream_Type with private;

   function End_Of_File
     (Resource : in File_Tagged)
      return Boolean;

   procedure Read
     (Resource : in out File_Tagged;
      Buffer   :    out Stream_Element_Array;
      Last     :    out Stream_Element_Offset);

   procedure Close (File : in out File_Tagged);

   procedure Create
     (Resource : in out AWS.Resources.Streams.Stream_Type'Class;
      Size     : in     Stream_Element_Offset);

private

   type File_Tagged is new Streams.Stream_Type with record
      Offset : Stream_Element_Offset;
      Size   : Stream_Element_Offset;
   end record;

end User_Strm;
