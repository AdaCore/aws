------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2009, AdaCore                     --
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

--  Handle .ini style configuration files. In those files each option is on one
--  line. The first word is the option name and the second one is the option
--  value.

package AWS.Config.Ini is

   function Program_Ini_File (Full_Path : Boolean) return String;
   --  Returns initialization filename for current server (using the
   --  executable name and adding .ini).

   procedure Read
     (Config   : in out Object;
      Filename : String);
   --  Read Filename and update the configuration object with the
   --  options read from it. Raises Ada.Text_IO.Name_Error if Filename does
   --  not exist. Raises Constraint_Error in case of wrong any parameter name
   --  or value.

end AWS.Config.Ini;
