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

pragma Ada_2012;

--  Some XML helper functions

with DOM.Core;

package SOAP.XML is

   use type DOM.Core.Node;

   function Get_Attr_Value
     (N    : DOM.Core.Node;
      Name : String;
      NS   : Boolean := True) return String
   with Pre => N /= null;
   --  Returns the value of attribute Name in N, remove namespace if NS is
   --  False. Returns the empty string if attribute Name does not exist.

   function Get_Ref (N : DOM.Core.Node) return DOM.Core.Node with
     Pre => N /= null;
   --  If there is a ref in N returns the multiRef referenced node otherwise
   --  just returns N.

   function First_Child (Parent : DOM.Core.Node) return DOM.Core.Node with
     Pre  => Parent /= null;
   --  Returns the first child, skip #text nodes. Returns null if there is no
   --  children.

   function Next_Sibling (N : DOM.Core.Node) return DOM.Core.Node with
     Pre  => N /= null;
   --  Returns the next sibling, skip #text nodes. Return null if there is no
   --  more sibbling.

end SOAP.XML;
