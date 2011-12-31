------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2012, AdaCore                     --
--                                                                          --
--  This is free software;  you can redistribute it  and/or modify it       --
--  under terms of the  GNU General Public License as published  by the     --
--  Free Software  Foundation;  either version 3,  or (at your option) any  --
--  later version.  This software is distributed in the hope  that it will  --
--  be useful, but WITHOUT ANY WARRANTY;  without even the implied warranty --
--  of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU     --
--  General Public License for  more details.                               --
--                                                                          --
--  You should have  received  a copy of the GNU General  Public  License   --
--  distributed  with  this  software;   see  file COPYING3.  If not, go    --
--  to http://www.gnu.org/licenses for a complete copy of the license.      --
------------------------------------------------------------------------------

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

   function Get_File_Path (Pathname : String) return String;
   --  Given a filename (absolute or relative) it will return the
   --  absolute pathname to this file. A trailing directory separator is
   --  returned.

   -------------------
   -- Get_File_Path --
   -------------------

   function Get_File_Path (Pathname : String) return String is
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
