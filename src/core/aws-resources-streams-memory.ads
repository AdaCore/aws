------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2003-2014, AdaCore                     --
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

--  API to handle a memory stream. A memory stream is first created
--  empty. User can add chunk of data using the Append routines. The stream
--  is then read using the Read procedure.

with AWS.Utils;

private with AWS.Containers.Memory_Streams;

package AWS.Resources.Streams.Memory is

   type Stream_Type is new Streams.Stream_Type with private;

   subtype Stream_Element_Access is Utils.Stream_Element_Array_Access;
   subtype Buffer_Access is Utils.Stream_Element_Array_Constant_Access;

   procedure Append
     (Resource : in out Stream_Type;
      Buffer   : Stream_Element_Array;
      Trim     : Boolean := False);
   --  Append Buffer into the memory stream

   procedure Append
     (Resource : in out Stream_Type;
      Buffer   : Stream_Element_Access);
   --  Append static data pointed to Buffer into the memory stream as is.
   --  The stream will free the memory on close.

   procedure Append
     (Resource : in out Stream_Type;
      Buffer   : Buffer_Access);
   --  Append static data pointed to Buffer into the memory stream as is.
   --  The stream would not try to free the memory on close.

   overriding procedure Read
     (Resource : in out Stream_Type;
      Buffer   : out Stream_Element_Array;
      Last     : out Stream_Element_Offset);
   --  Returns a chunck of data in Buffer, Last point to the last element
   --  returned in Buffer.

   overriding function End_Of_File (Resource : Stream_Type) return Boolean;
   --  Returns True if the end of the memory stream has been reached

   procedure Clear (Resource : in out Stream_Type) with Inline;
   --  Delete all data from memory stream

   overriding procedure Reset (Resource : in out Stream_Type);
   --  Reset the streaming data to the first position

   overriding procedure Set_Index
     (Resource : in out Stream_Type;
      To       : Stream_Element_Offset);
   --  Set the position in the stream, next Read will start at the position
   --  whose index is To.

   overriding function Size
     (Resource : Stream_Type) return Stream_Element_Offset;
   --  Returns the number of bytes in the memory stream

   overriding procedure Close (Resource : in out Stream_Type);
   --  Close the memory stream. Release all memory associated with the stream

private

   package Containers renames AWS.Containers.Memory_Streams;

   type Stream_Type is new Streams.Stream_Type with record
      Data : Containers.Stream_Type;
   end record;

end AWS.Resources.Streams.Memory;
