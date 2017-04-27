------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                    Copyright (C) 2015-2017, AdaCore                      --
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
with Ada.Strings.Unbounded;

package body SOAP.WSDL.Schema is

   use Ada;
   use Ada.Strings.Unbounded;

   type Data is record
      URL  : Unbounded_String;
      Node : DOM.Core.Node;
   end record;

   package Schema_Store is new Containers.Indefinite_Vectors (Positive, Data);

   Store : Schema_Store.Vector;

   --------------
   -- Contains --
   --------------

   function Contains (Namespace : URL) return Boolean is
   begin
      for E of Store loop
         if E.URL = Namespace then
            return True;
         end if;
      end loop;

      return False;
   end Contains;

   -------------
   -- For_All --
   -------------

   procedure For_All
     (Namespace : URL;
      Process   : not null access procedure (N : DOM.Core.Node)) is
   begin
      for E of Store loop
         if E.URL = Namespace then
            Process (E.Node);
         end if;
      end loop;
   end For_All;

   -----------------------
   -- Get_Binding_Style --
   -----------------------

   function Get_Binding_Style (Schema : Definition) return Binding_Style is
   begin
      if Schema.Contains ("@binding.style") then
         return Binding_Style'Value (Schema ("@binding.style"));
      else
         return RPC;
      end if;
   end Get_Binding_Style;

   ----------------------------
   -- Get_Call_For_Signature --
   ----------------------------

   function Get_Call_For_Signature
     (Schema    : Definition;
      Signature : String) return String
   is
      Key : constant String := "@" & Signature;
   begin
      if Schema.Contains (Key) then
         return Schema (Key);
      else
         return "";
      end if;
   end Get_Call_For_Signature;

   ------------------------
   -- Get_Encoding_Style --
   ------------------------

   function Get_Encoding_Style
     (Schema    : Definition;
      Operation : String) return Encoding_Style
   is
      Key : constant String := "@" & Operation & ".encoding";
   begin
      if Schema.Contains (Key) then
         return Encoding_Style'Value (Schema (Key));
      else
         return Encoded;
      end if;
   end Get_Encoding_Style;

   --------------
   -- Register --
   --------------

   procedure Register (Namespace : URL; Node : DOM.Core.Node) is
   begin
      Store.Append (Data'(To_Unbounded_String (Namespace), Node));
   end Register;

   -----------------------
   -- Set_Binding_Style --
   -----------------------

   procedure Set_Binding_Style
     (Schema : in out Definition; Style : Binding_Style) is
   begin
      Schema.Include ("@binding.style", Binding_Style'Image (Style));
   end Set_Binding_Style;

   ------------------------
   -- Set_Encoding_Style --
   ------------------------

   procedure Set_Encoding_Style
     (Schema    : in out Definition;
      Operation : String;
      Encoding  : Encoding_Style) is
   begin
      Schema.Include
        ("@" & Operation & ".encoding", Encoding_Style'Image (Encoding));
   end Set_Encoding_Style;

end SOAP.WSDL.Schema;
