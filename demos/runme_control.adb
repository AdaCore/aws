------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                            Copyright (C) 2001                            --
--                               Pascal Obry                                --
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
with GNAT.Command_Line;
with GNAT.Directory_Operations;

with Service;
with Runme_Info;

procedure Runme_Control is

   use Ada;
   use GNAT;

   Syntax_Error : exception;

   procedure Install_Service;
   --  Register service into OS.

   procedure Remove_Service;
   --  Unregister service into OS.

   procedure Parse_Command_Line;
   --  Parse command line and set options.

   type Mode is (Nothing, Install, Uninstall);
   Option : Mode := Nothing;

   ---------------------
   -- Install_Service --
   ---------------------

   procedure Install_Service is
   begin
      if Runme_Info.Get_Client_Name_Full_Pathname = "" then
         Text_IO.Put_Line (Runme_Info.Client_Name
                           & " not found. Check the path.");

      else
         -- Install the service
         Service.Create
           (Service_Name => Runme_Info.Service_Name,
            Display_Name => Runme_Info.Display_Name,
            Binary_Path  => Runme_Info.Get_Client_Name_Full_Pathname,
            Startup      => Service.Automatic);
      end if;
   end Install_Service;

   --------------------
   -- Remove_Service --
   --------------------

   procedure Remove_Service is
   begin
      Service.Delete (Runme_Info.Service_Name);
   end Remove_Service;

   ------------------------
   -- Parse_Command_Line --
   ------------------------

   procedure Parse_Command_Line is
   begin
     loop
        case GNAT.Command_Line.Getopt ("i u") is

           when ASCII.NUL =>
              exit;

           when 'i' =>
              if Option = Nothing then
                 Option := Install;
              else
                 raise Syntax_Error;
              end if;

           when 'u' =>
              if Option = Nothing then
                 Option := Uninstall;
              else
                 raise Syntax_Error;
              end if;

           when others =>
              raise Syntax_Error;

        end case;
     end loop;
   end Parse_Command_Line;

begin
   Parse_Command_Line;

   if Option = Install then
      Install_Service;

   elsif Option = Uninstall then
      Remove_Service;

   else
      raise Syntax_Error;
   end if;

exception
   when Syntax_Error =>
      Text_IO.New_Line;
      Text_IO.Put_Line ("Usage: runme_control [-iu]");
      Text_IO.Put_Line ("       -i  : install service");
      Text_IO.Put_Line ("       -u  : uninstall service");
      Text_IO.New_Line;
end Runme_Control;
