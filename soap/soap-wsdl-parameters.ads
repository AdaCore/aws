------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                            Copyright (C) 2003                            --
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

with Ada.Strings.Unbounded;

package SOAP.WSDL.Parameters is

   use Ada.Strings.Unbounded;

   type Kind is (K_Record, K_Array, K_Derived, K_Simple);

   type Parameter;
   type P_Set is access Parameter;

   type Parameter (Mode : Kind) is record
      Name : Unbounded_String;
      Next : P_Set;

      case Mode is
         when K_Simple =>
            P_Type : Parameter_Type;

         when K_Derived =>
            Parent_Type : Parameter_Type;
            D_Name      : Unbounded_String;

         when K_Array | K_Record =>
            T_Name : Unbounded_String; -- Type name
            E_Type : Unbounded_String; -- Array element's type
            P      : P_Set;
      end case;
   end record;

   procedure Append (P : in out P_Set; Param : in Parameter);
   --  Add Param at the end of P

   function Length (P : in P_Set) return Natural;
   --  Returns the number of items in P

   procedure Output (P : in P_Set);
   --  Output parameter set, this is to be used for debugging purpose

   procedure Release (P : in out P_Set);
   --  Release memory associated the the parameter set

end SOAP.WSDL.Parameters;
