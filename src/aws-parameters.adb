------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2001                          --
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

--  Parameters are put into an AVL Tree. Each entry in the tree is composed of
--  a Key and a Value. The parameters must be accessible through their name
--  and also using an index. So given a set of parameters (K1=V1, K2=V2...),
--  one must be able to ask for the value for K1 but also the name of the
--  second key or the value of the third key.
--
--  Each K/V pair is then inserted into the Data tree and two times into the
--  HTTP_Data tree:
--
--  Into Data:
--
--  1) key=K with value=V
--
--  Into HTTP_Data:
--
--  1) key=__AWS_K<n> with value=K     (n beeing an indice representing the
--  2) key=__AWS_V<n> with value=V      entry number in the tree)
--
--  So to get the third key name we ask for the entry indexed under __AWS_K3
--  into HTTP_Data tree.
--
--  Another important point is that a key can have many values. For example
--  with an HTML multiple select entry in a form. In such a case all values
--  associated with the key K are concatenated together with a specific
--  separator.

package body AWS.Parameters is

   ----------------
   -- URI_Format --
   ----------------

   function URI_Format (Parameter_List : in List) return String is
   begin
      return To_String (Parameter_List.Parameters);
   end URI_Format;

end AWS.Parameters;
