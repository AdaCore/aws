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

with Ada.Unchecked_Deallocation;

with AWS.Resources.Files;
with AWS.Resources.Embedded;

package body AWS.Resources is

   use Ada;

   procedure Free is
      new Ada.Unchecked_Deallocation (Resources.File_Type'Class, File_Access);

   -----------
   -- Close --
   -----------

   procedure Close (Resource : in out File_Access) is
   begin
      Close (Resource.all);
      Free (Resource);
   end Close;

   procedure Close (Resource : in File_Type) is
      pragma Unreferenced (Resource);
   begin
      null;
   end Close;

   ---------------
   -- File_Size --
   ---------------

   function File_Size
     (Name : in String)
      return Ada.Streams.Stream_Element_Offset is
   begin
      if Resources.Embedded.Exists (Name) then
         return Resources.Embedded.File_Size (Name);
      else
         return Resources.Files.File_Size (Name);
      end if;
   end File_Size;

   --------------------
   -- File_Timestamp --
   --------------------

   function File_Timestamp
     (Name : in String)
      return Ada.Calendar.Time is
   begin
      if Resources.Embedded.Exists (Name) then
         return Resources.Embedded.File_Timestamp (Name);
      else
         return Resources.Files.File_Timestamp (Name);
      end if;
   end File_Timestamp;

   ---------------------
   -- Is_Regular_File --
   ---------------------

   function Is_Regular_File
     (Name : in String)
      return Boolean is
   begin
      if Resources.Embedded.Exists (Name) then
         return Resources.Embedded.Is_Regular_File (Name);
      else
         return Resources.Files.Is_Regular_File (Name);
      end if;
   end Is_Regular_File;

   ----------
   -- Open --
   ----------

   procedure Open
     (File :    out File_Access;
      Name : in     String;
      Form : in     String    := "") is
   begin
      --  Try to open the file in memory, if not found open the file on disk.
      Resources.Embedded.Open (File, Name, Form);

      if File = null then
         Resources.Files.Open (File, Name, Form);
      end if;
   end Open;

end AWS.Resources;
