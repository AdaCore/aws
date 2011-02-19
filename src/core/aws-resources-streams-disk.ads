------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2003-2011, AdaCore                     --
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

--  An ready-to-use implementation of the stream API where the stream content
--  is read from an on-disk file.

with Ada.Streams.Stream_IO;

private with Ada.Strings.Unbounded;

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
