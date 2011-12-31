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

package Runme_Info is

   Service_Name : constant String := "AWS-Runme";
   Display_Name : constant String := "AWS Runme Web Demo";
   Client_Name  : constant String := "runme_server.exe";

   function Get_Client_Name_Full_Pathname return String;
   --  Returns Client_Name full pathname. This looks for Client_Name into the
   --  path and if found returns the full pathname to it.

   function Get_Executable_Path return String;
   --  Returns full pathname for Client_Name executable. It uses the
   --  information given by Ada.Command_Line.Command_Name.

end Runme_Info;
