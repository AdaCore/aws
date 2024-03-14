------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2003-2017, AdaCore                     --
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

with Ada.Characters.Handling;

with DOM.Core.Nodes;

with SOAP.Utils;

package body SOAP.XML is

   -----------------
   -- First_Child --
   -----------------

   function First_Child (Parent : DOM.Core.Node) return DOM.Core.Node is
      use type DOM.Core.Node_Types;
      N : DOM.Core.Node;
   begin
      N := DOM.Core.Nodes.First_Child (Parent);

      while N /= null
        and then (N.Node_Type = DOM.Core.Text_Node
                  or else N.Node_Type = DOM.Core.Comment_Node)
      loop
         --  DOM.Core.Nodes.Node_Name (N) = "#text" loop
         N := DOM.Core.Nodes.Next_Sibling (N);
      end loop;

      return N;
   end First_Child;

   --------------------
   -- Get_Attr_Value --
   --------------------

   function Get_Attr_Value
     (N    : DOM.Core.Node;
      Name : String;
      NS   : Boolean := True) return String
   is

      A : DOM.Core.Node;
   begin
      A := DOM.Core.Nodes.Get_Named_Item (DOM.Core.Nodes.Attributes (N), Name);

      if A = null then
         return "";

      else
         declare
            V : constant String := DOM.Core.Nodes.Node_Value (A);
         begin
            if NS then
               return V;
            else
               return Utils.No_NS (V);
            end if;
         end;
      end if;
   end Get_Attr_Value;

   -------------
   -- Get_Ref --
   -------------

   function Get_Ref (N : DOM.Core.Node) return DOM.Core.Node is

      use Ada;

      function Body_Node (N : DOM.Core.Node) return DOM.Core.Node;
      --  Returns Payload body node for N being inside this node

      ---------------
      -- Body_Node --
      ---------------

      function Body_Node (N : DOM.Core.Node) return DOM.Core.Node is
         L_N : DOM.Core.Node := N;

      begin
         while Characters.Handling.To_Lower
           (Utils.No_NS (DOM.Core.Nodes.Local_Name (L_N))) /= "body"
         loop
            L_N := DOM.Core.Nodes.Parent_Node (L_N);
         end loop;
         return L_N;
      end Body_Node;

      Atts : constant DOM.Core.Named_Node_Map := DOM.Core.Nodes.Attributes (N);
      Href : constant DOM.Core.Node :=
               DOM.Core.Nodes.Get_Named_Item (Atts, "href");

      HN   : DOM.Core.Node;

   begin
      if Href = null then
         return N;

      else
         HN := XML.First_Child (Body_Node (N));

         declare
            Id : constant String := DOM.Core.Nodes.Node_Value (Href);
            --  This is the Id we are looking for
         begin
            --  We have an href, look for the corresponding node

            while  "#" & XML.Get_Attr_Value (HN, "id") /= Id loop
               HN := DOM.Core.Nodes.Next_Sibling (HN);
            end loop;

            return HN;
         end;
      end if;
   end Get_Ref;

   ------------------
   -- Next_Sibling --
   ------------------

   function Next_Sibling (N : DOM.Core.Node) return DOM.Core.Node is
      use type DOM.Core.Node_Types;
      M : DOM.Core.Node := N;
   begin
      loop
         M := DOM.Core.Nodes.Next_Sibling (M);
         exit when M = null
           or else (M.Node_Type /= DOM.Core.Text_Node
                    and then M.Node_Type /= DOM.Core.Comment_Node);
      end loop;

      return M;
   end Next_Sibling;

end SOAP.XML;
