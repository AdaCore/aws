------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2002-2019 AdaCore                     --
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

with Ada.Directories;
with Ada.IO_Exceptions;

with AWS.Resources.Streams.Disk;
with AWS.Resources.Streams.ZLib;

with ZLib;

package body AWS.Resources.Files is

   use Ada;

   -----------
   -- Exist --
   -----------

   function Exist (Name : String) return File_Instance is
      VP, VG : Boolean := False;
   begin
      if Is_GZip (Name) then
         VG := Utils.Is_Regular_File (Name);
         VP := Utils.Is_Regular_File
           (Name (Name'First .. Name'Last - GZip_Ext'Length));

      else
         VP := Utils.Is_Regular_File (Name);
         VG := Utils.Is_Regular_File (Name & GZip_Ext);
      end if;

      if VG and then VP then
         return Both;
      elsif VG then
         return GZip;
      elsif VP then
         return Plain;
      else
         return None;
      end if;
   end Exist;

   ---------------
   -- File_Size --
   ---------------

   function File_Size (Name : String) return Utils.File_Size_Type is
   begin
      if Utils.Is_Regular_File (Name) then
         return Utils.File_Size (Name);

      else
         if Is_GZip (Name) then
            raise Resource_Error;
         else
            return File_Size (Name & GZip_Ext);
         end if;
      end if;
   end File_Size;

   --------------------
   -- File_Timestamp --
   --------------------

   function File_Timestamp (Name : String) return Ada.Calendar.Time is
   begin
      if Utils.Is_Regular_File (Name) then
         return Directories.Modification_Time (Name);

      else
         if Is_GZip (Name) then
            raise Resource_Error;
         else
            return File_Timestamp (Name & GZip_Ext);
         end if;
      end if;
   end File_Timestamp;

   ---------------------
   -- Is_Regular_File --
   ---------------------

   function Is_Regular_File (Name : String) return Boolean is
   begin
      return Utils.Is_Regular_File (Name)
        or else
          (not Is_GZip (Name)
           and then Utils.Is_Regular_File (Name & GZip_Ext));
   end Is_Regular_File;

   ----------
   -- Open --
   ----------

   procedure Open
     (File : out File_Type;
      Name : String;
      Form : String    := "";
      GZip : in out Boolean)
   is
      Stream    : AWS.Resources.Streams.Stream_Access;
      In_GZip   : constant Boolean := GZip;
      Real_Name : constant String  :=
                    Check_Name (Name, Utils.Is_Regular_File'Access, GZip);
   begin
      if Real_Name = "" then
         raise IO_Exceptions.Name_Error with "File '" & Name & "' not found.";
      end if;

      Stream := new AWS.Resources.Streams.Disk.Stream_Type;

      AWS.Resources.Streams.Disk.Open
        (AWS.Resources.Streams.Disk.Stream_Type (Stream.all), Real_Name, Form);

      if GZip and not In_GZip then
         Stream := Streams.ZLib.Inflate_Create (Stream, Header => ZLib.GZip);
      end if;

      Streams.Create (File, Stream);
   end Open;

   procedure Open
     (File : out File_Type;
      Name : String;
      Form : String    := "")
   is
      GZip : Boolean := False;
   begin
      Open (File, Name, Form, GZip);
   end Open;

end AWS.Resources.Files;
