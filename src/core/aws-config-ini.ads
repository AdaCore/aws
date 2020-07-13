------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2012, AdaCore                     --
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
   --  Read Filename and update the configuration object with the options read
   --  from it.
   --  Raises Constraint_Error in case of wrong any parameter name or value.
   --  Raises Ada.Text_IO.Status_Error if the Filename is already open.
   --  Raises Ada.Text_IO.Name_Error if Filename does not exist.
   --  Raises Ada.Text_IO.Use_Error if the external environment does not
   --  support opening for an external file with the given name.

end AWS.Config.Ini;
