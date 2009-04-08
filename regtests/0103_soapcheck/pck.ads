------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2008-2009, AdaCore                     --
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

with SOAP.Utils;

package Pck is

   type Small is new Integer range 1 .. 8;

   type Color is (Red, Green, Blue);

   type Rec is record
      V : Natural;
      S : Small;
      C : Color;
   end record;

   procedure Call (R : Rec);

   procedure Print (X : Integer; Name : String);

   type Tab is array (1 .. 10) of Boolean;
   type Tab_Access is access Tab;
   package Tab_Safe_Pointer is
      new SOAP.Utils.Safe_Pointers (Tab, Tab_Access);

   type R_Rec is record
      V : Natural;
      S : Small;
      C : Color;
      F : Long_Float;
      G : Rec;
      A : Tab_Safe_Pointer.Safe_Pointer;
   end record;

   procedure Big (R : R_Rec);

end Pck;
