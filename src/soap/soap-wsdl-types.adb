------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                        Copyright (C) 2015, AdaCore                       --
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

with Ada.Containers.Indefinite_Vectors;
with Ada.Text_IO;

package body SOAP.WSDL.Types is

   use Ada;

   package Types_Store is
      new Containers.Indefinite_Vectors (Positive, Definition);
   Store : Types_Store.Vector;

   function Contains
     (Type_Name : String; NS : Name_Space.Object) return Boolean;
   --  Returns True if the type is already registered

   --------------
   -- Contains --
   --------------

   function Contains
     (Type_Name : String; NS : Name_Space.Object) return Boolean
   is
      use type Name_Space.Object;
   begin
      for D of Store loop
         if D.Name = Type_Name and then D.NS = NS then
            return True;
         end if;
      end loop;
      return False;
   end Contains;

   -----------
   -- Count --
   -----------

   function Count return Natural is
   begin
      return Natural (Store.Length);
   end Count;

   ----------
   -- Find --
   ----------

   function Find
     (Type_Name : String; NS : Name_Space.Object) return Definition
   is
      use type Name_Space.Object;
   begin
      for D of Store loop
         if D.Name = Type_Name and then D.NS = NS then
            return D;
         end if;
      end loop;
      return No_Definition;
   end Find;

   -----------
   -- Image --
   -----------

   function Image (K : Kind) return String is
   begin
      return (case K is
                 when K_Record      => "record",
                 when K_Array       => "array",
                 when K_Derived     => "derived",
                 when K_Enumeration => "enumeration",
                 when K_Simple      => "simple");
   end Image;

   ------------
   -- Output --
   ------------

   procedure Output (P : Definition) is
   begin
      Text_IO.Put (To_String (P.Name));
   end Output;

   --------------
   -- Register --
   --------------

   procedure Register (Def : Definition) is
   begin
      if not Contains (To_String (Def.Name), Def.NS) then
         Store.Append (Def);
      end if;
   end Register;

   -------------
   -- Release --
   -------------

   procedure Release is
   begin
      Store.Clear;
   end Release;

end SOAP.WSDL.Types;
