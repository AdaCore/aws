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

with Ada.Text_IO;
with Ada.Strings.Fixed;
with Ada.Command_Line;
with GNAT.OS_Lib;

package body Runme_Info is

   use Ada;
   use GNAT;
   use type GNAT.OS_Lib.String_Access;

   Executable : constant OS_Lib.String_Access
     := OS_Lib.Locate_Exec_On_Path (Client_Name);

   function Get_File_Path (Pathname : in String) return String;
   --  Given a filename (absolute or relative) it will return the
   --  absolute pathname to this file. A trailing directory separator is
   --  returned.

   -------------------
   -- Get_File_Path --
   -------------------

   function Get_File_Path (Pathname : in String) return String is
      File : Text_IO.File_Type;
   begin
      Text_IO.Open (File, Text_IO.In_File, Pathname);

      declare
         Name : constant String := Text_IO.Name (File);
      begin
         Text_IO.Close (File);
         return Name (Name'First ..
                      Strings.Fixed.Index (Name, "\", Strings.Backward));
      end;
   end Get_File_Path;

   -----------------------------------
   -- Get_Client_Name_Full_Pathname --
   -----------------------------------

   function Get_Client_Name_Full_Pathname return String is
   begin
      if Executable = null then
         return Client_Name;
      else
         return Get_File_Path (Executable.all) & Client_Name;
      end if;
   end Get_Client_Name_Full_Pathname;

   -------------------------
   -- Get_Executable_Path --
   -------------------------

   function Get_Executable_Path return String is
      CN : constant String := Command_Line.Command_Name;
   begin
      return CN (CN'First .. Strings.Fixed.Index (CN, "\", Strings.Backward));
   end Get_Executable_Path;

end Runme_Info;
