------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                            Copyright (C) 2002                            --
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

--  $Id$

with Ada.Streams.Stream_IO;

package AWS.Resources.Files is

   type File_Type is new Resources.File_Type with private;

   procedure Open
     (File :    out File_Access;
      Name : in     String;
      Form : in     String    := "");

   procedure Read
     (Resource : in out File_Type;
      Buffer   :    out Stream_Element_Array;
      Last     :    out Stream_Element_Offset);

   procedure Get_Line
     (Resource  : in out File_Type;
      Buffer    :    out String;
      Last      :    out Natural);

   function End_Of_File (Resource : in File_Type) return Boolean;

   function Is_Regular_File (Name : in String) return Boolean;

   function File_Size
     (Name : in String)
      return Ada.Streams.Stream_Element_Offset;

   function File_Timestamp (Name : in String) return Ada.Calendar.Time;

private

   procedure Close (Resource : in out File_Type);

   Buffer_Size : constant := 8_192;

   type Stream_File_Access is access Stream_IO.File_Type;

   type File_Type is new Resources.File_Type with record
      File    : Stream_File_Access;
      Stream  : Stream_IO.Stream_Access;
      --  below are data for buffered access to the file.
      Buffer  : Stream_Element_Array (1 .. Buffer_Size);
      Current : Stream_Element_Offset := 1;
      Last    : Stream_Element_Offset := 0;
   end record;

end AWS.Resources.Files;
