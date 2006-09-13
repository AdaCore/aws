------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                          Copyright (C) 2003-2004                         --
--                                ACT-Europe                                --
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

with Ada.Text_IO;
with Ada.Unchecked_Deallocation;

package body SOAP.WSDL.Parameters is

   ------------
   -- Append --
   ------------

   procedure Append (P : in out P_Set; Param : in Parameter) is
      N : P_Set;
   begin
      if P = null then
         P := new Parameter'(Param);
      else

         N := P;

         while N.Next /= null loop
            N := N.Next;
         end loop;

         N.Next := new Parameter'(Param);

      end if;
   end Append;

   ------------
   -- Length --
   ------------

   function Length (P : in P_Set) return Natural is
      N      : P_Set := P;
      Result : Natural := 0;
   begin
      while N /= null loop
         Result := Result + 1;
         N := N.Next;
      end loop;
      return Result;
   end Length;

   ------------
   -- Output --
   ------------

   procedure Output (P : in P_Set) is

      use Ada;
      use type Parameters.Kind;
      use type Parameters.P_Set;

      procedure Output (P : in P_Set; K : in Natural);

      ------------
      -- Output --
      ------------

      procedure Output (P : in P_Set; K : in Natural) is
      begin
         if P /= null then
            Text_IO.Put (String'(1 .. K => ' '));

            if P.Mode = Parameters.K_Simple then
               Text_IO.Put ("[simple] ");
               Text_IO.Put_Line
                 (To_String (P.Name) & " ; " & To_Ada (P.P_Type));

            elsif P.Mode = Parameters.K_Derived then
               Text_IO.Put ("[derived] ");
               Text_IO.Put_Line
                 (To_String (P.Name) & " ; " & To_String (P.D_Name));

            elsif P.Mode = Parameters.K_Enumeration then
               Text_IO.Put ("[enumeration] ");
               Text_IO.Put_Line
                 (To_String (P.Name) & " ; " & To_String (P.E_Name));

            else
               if P.Mode = Parameters.K_Array then
                  Text_IO.Put ("[array] ");
               else
                  Text_IO.Put ("[record] ");
               end if;

               Text_IO.Put_Line
                 (To_String (P.Name) & " ; " & To_String (P.T_Name));

               Output (P.P, K + 3);
            end if;

            Output (P.Next, K);
         end if;
      end Output;

   begin
      Output (P, 6);
   end Output;

   -------------
   -- Release --
   -------------

   procedure Release (P : in out P_Set) is

      procedure Free is new Ada.Unchecked_Deallocation (Parameter, P_Set);

      procedure Free is new Ada.Unchecked_Deallocation (E_Node, E_Node_Access);

   begin
      if P /= null then
         if P.Mode = K_Array or else P.Mode = K_Record then
            Release (P.P);

         elsif P.Mode = K_Enumeration then
            declare
               C, N : E_Node_Access;
            begin
               C := P.E_Def;

               while C /= null loop
                  N := C.Next;
                  Free (C);
                  C := N;
               end loop;
            end;
         end if;

         Release (P.Next);
         Free (P);
      end if;
   end Release;

   ---------------
   -- Type_Name --
   ---------------

   function Type_Name (P : in WSDL.Parameters.P_Set) return String is
   begin
      case P.Mode is
         when K_Simple           => return WSDL.To_Ada (P.P_Type);
         when K_Derived          => return To_String (P.D_Name);
         when K_Array | K_Record => return To_String (P.T_Name);
         when K_Enumeration      => return To_String (P.E_Name);
      end case;
   end Type_Name;

end SOAP.WSDL.Parameters;
