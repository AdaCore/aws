------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2003-2015, AdaCore                     --
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

with Ada.Strings.Unbounded;

with SOAP.Name_Space;
with SOAP.WSDL.Types;

package SOAP.WSDL.Parameters is

   use Ada.Strings.Unbounded;

   --  Parameter

   type Parameter;
   type P_Set is access Parameter;

   type Parameter (Mode : Types.Kind) is record
      Name      : Unbounded_String;
      Type_Name : Unbounded_String;
      NS        : Name_Space.Object;
      Next      : P_Set;

      case Mode is
         when Types.Compound_Type =>
            Length : Natural;          -- Number of items (0 = unbounded)
            P      : P_Set;

         when others =>
            null;
      end case;
   end record;

   procedure Append (P : in out P_Set; Param : Parameter) with
     Post => Length (P) = Length (P)'Old + 1;
   --  Add Param at the end of P

   function Length (P : access Parameter) return Natural;
   --  Returns the number of items in P

   procedure Output (P : access Parameter);
   --  Output parameter set, this is to be used for debugging purpose

   procedure Release (P : in out P_Set) with
     Post => Length (P) = 0;
   --  Release memory associated the the parameter set

end SOAP.WSDL.Parameters;
