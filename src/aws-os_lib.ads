------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2001                          --
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

with Ada.Calendar;
with Ada.Streams;

package AWS.OS_Lib is

   No_Such_File : exception;

   function Is_Regular_File (Filename : in String) return Boolean;
   --  Returns True if Filename is a regular file and is readable.

   function Is_Directory (Filename : in String) return Boolean;
   --  Returns True if Filename is a directory.

   function File_Size (Filename : in String)
      return Ada.Streams.Stream_Element_Offset;
   --  Returns Filename's size in bytes.

   function File_Timestamp (Filename : in String) return Ada.Calendar.Time;
   --  Get the time for last modification to a file in UTC/GMT.

   function GMT_Clock return Ada.Calendar.Time;
   --  Returns current UTC/GMT time.

private

   pragma Inline (Is_Regular_File);
   pragma Inline (Is_Directory);
   pragma Inline (File_Size);
   pragma Inline (File_Timestamp);
   pragma Inline (GMT_Clock);

end AWS.OS_Lib;
