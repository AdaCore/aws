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

--  $Id$

with Ada.IO_Exceptions;

with AWS.OS_Lib;
with AWS.Resources.Streams.Disk;

package body AWS.Resources.Files is

   ---------------
   -- File_Size --
   ---------------

   function File_Size
     (Name : in String)
      return Ada.Streams.Stream_Element_Offset is
   begin
      return OS_Lib.File_Size (Name);
   exception
      when others =>
         raise Resource_Error;
   end File_Size;

   --------------------
   -- File_Timestamp --
   --------------------

   function File_Timestamp (Name : in String) return Ada.Calendar.Time is
   begin
      return OS_Lib.File_Timestamp (Name);
   exception
      when others =>
         raise Resource_Error;
   end File_Timestamp;

   ---------------------
   -- Is_Regular_File --
   ---------------------

   function Is_Regular_File (Name : in String) return Boolean is
   begin
      return OS_Lib.Is_Regular_File (Name);
   exception
      when others =>
         raise Resource_Error;
   end Is_Regular_File;

   ----------
   -- Open --
   ----------

   procedure Open
     (File :    out File_Type;
      Name : in     String;
      Form : in     String    := "")
   is
      Stream : AWS.Resources.Streams.Stream_Access;
   begin
      Stream := new AWS.Resources.Streams.Disk.Stream_Type;
      AWS.Resources.Streams.Create (File, Stream);

      AWS.Resources.Streams.Disk.Open
        (AWS.Resources.Streams.Disk.Stream_Type (Stream.all), Name, Form);
   exception
      when Ada.IO_Exceptions.Name_Error =>
         Free (File);
         raise;
   end Open;

end AWS.Resources.Files;
