------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2002-2017, AdaCore                     --
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

package AWS.Resources.Streams is

   type Stream_Type is abstract tagged limited private;

   type Stream_Access is access all Stream_Type'Class;

   function End_Of_File (Resource : Stream_Type) return Boolean is abstract;

   procedure Read
     (Resource : in out Stream_Type;
      Buffer   : out Stream_Element_Array;
      Last     : out Stream_Element_Offset) is abstract;

   procedure Reset (Resource : in out Stream_Type) is abstract;

   procedure Set_Index
     (Resource : in out Stream_Type;
      To       : Stream_Element_Offset) is abstract;
   --  Set the position in the stream, next Read will start at the position
   --  whose index is To. If To is outside the content the index is set to
   --  Last + 1 to ensure that next End_Of_File will return True.

   procedure Close (Resource : in out Stream_Type) is abstract;

   function Size (Resource : Stream_Type) return Stream_Element_Offset;
   --  This default implementation returns Undefined_Length. If the derived
   --  stream implementation knows about the size (in bytes) of the stream
   --  this routine should be redefined.

   function Name (Resource : Stream_Type) return String;
   --  This default implementation returns the empty string. It is must be
   --  overwritten by file based stream to provide the proper filename
   --  associated with the stream.

   procedure Create
     (Resource : out File_Type;
      Stream   : Stream_Access) with Inline;
   --  Create a resource file from user defined stream

private

   type Stream_Type is abstract new Resources.File_Tagged with null record;

end AWS.Resources.Streams;
