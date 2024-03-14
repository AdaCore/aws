------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2015, AdaCore                     --
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

with AWS.Containers.Key_Value;

with SOAP.Types;

package body SOAP.Message.Payload is

   -----------
   -- Build --
   -----------

   function Build
     (Procedure_Name : String;
      P_Set          : SOAP.Parameters.List;
      Name_Space     : SOAP.Name_Space.Object := SOAP.Name_Space.AWS)
      return Object
   is
      use AWS;
      use type SOAP.Name_Space.Object;

      Gen : Containers.Key_Value.Map;
   begin
      return O : Object do
         O.Name_Space   := Name_Space;
         O.Wrapper_Name := To_Unbounded_String (Procedure_Name);
         O.P            := P_Set;

         --  Add all the user's name-spaces for the given parameters

         for K in 1 .. SOAP.Parameters.Argument_Count (P_Set) loop
            declare
               N : constant SOAP.Name_Space.Object :=
                     SOAP.Types.Name_Space
                       (SOAP.Parameters.Argument (P_Set, K));
            begin
               if N /= SOAP.Name_Space.No_Name_Space
                 and then not Gen.Contains (SOAP.Name_Space.Value (N))
               then
                  O.Index := O.Index + 1;
                  O.Users_NS (O.Index) :=
                    SOAP.Name_Space.Create (SOAP.Name_Space.Name (N),
                                            SOAP.Name_Space.Value (N));
                  Gen.Insert
                    (SOAP.Name_Space.Value (N),
                     SOAP.Name_Space.Name (N));
               end if;
            end;
         end loop;
      end return;
   end Build;

   --------------------
   -- Procedure_Name --
   --------------------

   function Procedure_Name (P : Object'Class) return String is
   begin
      return Wrapper_Name (P);
   end Procedure_Name;

   ------------------------
   -- Set_Procedure_Name --
   ------------------------

   procedure Set_Procedure_Name (P : in out Object'Class; Name : String) is
   begin
      Set_Wrapper_Name (P, Name);
   end Set_Procedure_Name;

end SOAP.Message.Payload;
