------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2006-2012, AdaCore                     --
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

private package AWS.Config.Utils is

   procedure Set_Parameter
     (Param_Set     : in out Parameter_Set;
      Name          : Parameter_Name;
      Value         : String;
      Error_Context : String);
   --  Set the AWS parameters. Raises Constraint_Error in case of wrong
   --  parameter value. Error_Context should contain additional information
   --  about the parameter. This  message will be added to the Constraint_Error
   --  exception. One way to use Error_Context is to set it with information
   --  about where this parameter come form.

   function Value
     (Item : String; Error_Context : String) return Parameter_Name;
   --  Convert string representation of AWS parameter name into Parameter_Name
   --  type.

   procedure Parse_Strings (Vector : in out SV.Vector; Line : String);
   --  Split comma separated values from Line into Vector.
   --  Trim spaces from both sides.

end AWS.Config.Utils;
