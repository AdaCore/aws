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

--  An ready-to-use implementation of the stream API where the stream content
--  is read from an on-disk file.

private with Ada.Strings.Unbounded;
private with Ada.Streams.Stream_IO;

package AWS.Resources.Streams.Disk is

   type Stream_Type is new Streams.Stream_Type with private;

   procedure Open
     (File : out Stream_Type;
      Name : String;
      Form : String    := "shared=no");

   overriding function End_Of_File (Resource : Stream_Type) return Boolean;

   overriding procedure Read
     (Resource : in out Stream_Type;
      Buffer   : out Stream_Element_Array;
      Last     : out Stream_Element_Offset);

   overriding function Size
     (Resource : Stream_Type) return Stream_Element_Offset;

   overriding function Name (Resource : Stream_Type) return String;

   overriding procedure Reset (Resource : in out Stream_Type);

   overriding procedure Set_Index
     (Resource : in out Stream_Type;
      To       : Stream_Element_Offset);

   overriding procedure Close (Resource : in out Stream_Type);

private

   use Ada.Strings.Unbounded;

   Buffer_Size : constant := 8_192;

   type Stream_Type is new Streams.Stream_Type with record
      File    : Stream_IO.File_Type;
      Stream  : Stream_IO.Stream_Access;
      --  Below are data for buffered access to the file
      Buffer  : Stream_Element_Array (1 .. Buffer_Size);
      Current : Stream_Element_Offset := 1;
      Last    : Stream_Element_Offset := 0;
      Name    : Unbounded_String;
   end record;

end AWS.Resources.Streams.Disk;
