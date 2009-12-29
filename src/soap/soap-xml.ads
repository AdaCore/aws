------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2003-2009, AdaCore                     --
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

--  Some XML helper functions

with DOM.Core;

package SOAP.XML is

   function Get_Attr_Value
     (N    : DOM.Core.Node;
      Name : String;
      NS   : Boolean := True) return String;
   --  Returns the value of attribute Name in N, remove namespace if NS is
   --  True. Returns the empty string if attribute Name does not exists.

   function Get_Ref (N : DOM.Core.Node) return DOM.Core.Node;
   --  If there is a ref in N returns the multiRef referenced node otherwise
   --  just returns N.

   function First_Child (Parent : DOM.Core.Node) return DOM.Core.Node;
   --  Returns the first child, skip #text nodes

   function Next_Sibling (N : DOM.Core.Node) return DOM.Core.Node;
   --  Returns the next sibling, skip #text nodes

end SOAP.XML;
