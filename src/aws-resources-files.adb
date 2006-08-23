------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2002-2006                          --
--                                 AdaCore                                  --
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

with Ada.Directories;
with Ada.IO_Exceptions;
with Ada.Unchecked_Deallocation;

with AWS.Resources.Streams.Disk;
with AWS.Resources.Streams.ZLib;
with AWS.Utils;

with ZLib;

package body AWS.Resources.Files is

   use Ada;
   use type Directories.File_Kind;

   -----------
   -- Exist --
   -----------

   function Exist (Name : in String) return File_Instance is
   begin
      if not Is_GZip (Name)
        and then Utils.Is_Regular_File (Name & GZip_Ext)
      then
         if Utils.Is_Regular_File (Name) then
            return Both;
         else
            return GZip;
         end if;
      elsif Utils.Is_Regular_File (Name) then
         return Plain;
      else
         return None;
      end if;
   end Exist;

   ---------------
   -- File_Size --
   ---------------

   function File_Size
     (Name : in String) return Stream_Element_Offset is
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

   function File_Timestamp (Name : in String) return Ada.Calendar.Time is
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

   function Is_Regular_File (Name : in String) return Boolean is
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
     (File :    out File_Type;
      Name : in     String;
      Form : in     String    := "";
      GZip : in out Boolean)
   is
      use type AWS.Resources.Streams.Stream_Access;

      Stream : AWS.Resources.Streams.Stream_Access;

      procedure Open_File (Name : String; Last : Boolean);

      ---------------
      -- Open_File --
      ---------------

      procedure Open_File (Name : String; Last : Boolean) is
         procedure Free is
           new Ada.Unchecked_Deallocation
             (Streams.Stream_Type'Class, Streams.Stream_Access);
      begin
         Stream := new AWS.Resources.Streams.Disk.Stream_Type;

         AWS.Resources.Streams.Disk.Open
           (AWS.Resources.Streams.Disk.Stream_Type (Stream.all), Name, Form);
      exception
         when Ada.IO_Exceptions.Name_Error =>
            Free (Stream);

            if Last then
               raise;
            end if;
      end Open_File;

   begin
      if Is_GZip (Name) then
         --  Don't try to open file Name & ".gz.gz"

         GZip := False;

         Open_File (Name, True);

      elsif GZip then
         Open_File (Name & ".gz", False);

         if Stream = null then
            Open_File (Name, True);

            GZip := False;
         end if;

      else
         Open_File (Name, False);

         if Stream = null then
            Open_File (Name & ".gz", True);

            Stream := Streams.ZLib.Inflate_Create
                        (Stream, Header => ZLib.GZip);
         end if;
      end if;

      Streams.Create (File, Stream);
   end Open;

   procedure Open
     (File :    out File_Type;
      Name : in     String;
      Form : in     String    := "")
   is
      GZip : Boolean := False;
   begin
      Open (File, Name, Form, GZip);
   end Open;

end AWS.Resources.Files;
