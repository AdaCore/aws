------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2002-2004                          --
--                                ACT-Europe                                --
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

with Ada.Streams;

with AWS.Resources.Streams.Memory;

package AWS.Resources.Embedded is

   use Ada;

   subtype Buffer_Access is Streams.Memory.Buffer_Access;

   procedure Open
     (File :    out File_Type;
      Name : in     String;
      Form : in     String    := "";
      GZip : in out Boolean);
   --  Open resource from registered data

   procedure Create
     (File   :    out File_Type;
      Buffer : in     Buffer_Access);
   --  Create the resource directly from memory data

   function Exist (Name : in String) return File_Instance;
   --  Return GZip if only file Name & ".gz" exists.
   --  Return Plain if only file Name exists.
   --  Return Both if both file Name and Name & ".gz" exists.
   --  Return None if files neither Name nor Name & ".gz" exist.

   function Is_Regular_File (Name : in String) return Boolean;
   pragma Inline (Is_Regular_File);
   --  Returns True if file named Name has been registered (i.e. it is an
   --  in-memory file).

   function File_Size
     (Name : in String)
      return Ada.Streams.Stream_Element_Offset;

   function File_Timestamp (Name : in String) return Ada.Calendar.Time;

   procedure Register
     (Name      : in String;
      Content   : in Buffer_Access;
      File_Time : in Calendar.Time);
   --  Register a new file named Name into the embedded resources. The file
   --  content is pointed to by Content, the File_Time must be the last
   --  modification time stamp for the file. If Name ends with ".gz" the
   --  embedded resource registered as compressed. If a file is already
   --  registered for this name, Content replace the previous one.

end AWS.Resources.Embedded;
