------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2020, AdaCore                     --
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

package SOAP is

   --  This is the root package for the SOAP implementation. It supports
   --  SOAP 1.1 specifications.

   SOAP_Error : exception;
   --  Will be raised when an error occurs in the SOAP implementation. The
   --  exception message will described the problem.

   Version : constant String := "3.0.0";
   --  Version number for this implementation

   No_SOAPAction : constant String := (1 => ASCII.NUL);
   --  Value used to specify that there was no SOAPAction specified

private

   function Float_Infinity return Float
     with Import, Convention => Intrinsic, External_Name => "__builtin_inff";

   function Long_Float_Infinity return Long_Float
     with Import, Convention => Intrinsic, External_Name => "__builtin_inf";

end SOAP;
