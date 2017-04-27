------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2005-2017, AdaCore                     --
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

with Ada.Finalization;

with SOAP.Name_Space;
with SOAP.Utils;

package body SOAP.Types.Untyped is

   -------
   -- S --
   -------

   overriding function S
     (V         : String;
      Name      : String := "item";
      Type_Name : String := XML_String;
      NS        : SOAP.Name_Space.Object := SOAP.Name_Space.No_Name_Space)
      return Untyped
   is
      L_V : constant String := Utils.To_Utf8 (V);
   begin
      return
        (Finalization.Controlled
         with To_Unbounded_String (Name), To_Unbounded_String (Type_Name),
              NS, To_Unbounded_String (L_V));
   end S;

   overriding function S
     (V         : Unbounded_String;
      Name      : String := "item";
      Type_Name : String := XML_String;
      NS        : SOAP.Name_Space.Object := SOAP.Name_Space.No_Name_Space)
      return Untyped is
   begin
      return
        (Finalization.Controlled
         with To_Unbounded_String (Name), To_Unbounded_String (Type_Name),
              NS, Utils.To_Utf8 (V));
   end S;

end SOAP.Types.Untyped;
