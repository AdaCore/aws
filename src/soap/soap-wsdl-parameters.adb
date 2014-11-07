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

with Ada.Text_IO;
with Ada.Unchecked_Deallocation;

package body SOAP.WSDL.Parameters is

   ------------
   -- Append --
   ------------

   procedure Append (P : in out P_Set; Param : Parameter) is
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

   function Length (P : access Parameter) return Natural is
      N      : access Parameter := P;
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

   procedure Output (P : access Parameter) is

      use Ada;
      use type Parameters.P_Set;
      use type Types.Kind;

      procedure Output (P : access Parameter; K : Natural);

      ------------
      -- Output --
      ------------

      procedure Output (P : access Parameter; K : Natural) is
      begin
         if P /= null then
            Text_IO.Put (String'(1 .. K => ' '));

            if P.Mode = Types.K_Simple then
               Text_IO.Put ("[simple] ");
               Text_IO.Put_Line
                 (To_String (P.Name) & " ; "
                  & To_Ada (To_Type (To_String (P.Type_Name))));

            else
               Text_IO.Put ('[' & Types.Image (P.Mode) & "] ");

               declare
                  Def : constant WSDL.Types.Definition :=
                          WSDL.Types.Find (To_String (P.Type_Name), P.NS);
               begin
                  Text_IO.Put (To_String (P.Name) & " ; ");
                  WSDL.Types.Output (Def);
                  Text_IO.New_Line;

                  if P.Mode in WSDL.Types.Compound_Type then
                     Output (P.P, K + 3);
                  end if;
               end;
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

      procedure Unchecked_Free is
        new Ada.Unchecked_Deallocation (Parameter, P_Set);

   begin
      if P /= null then
         if P.Mode in Types.Compound_Type then
            Release (P.P);
         end if;

         Release (P.Next);
         Unchecked_Free (P);
      end if;
   end Release;

end SOAP.WSDL.Parameters;
