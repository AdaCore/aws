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

--  This package is based on Tree_Reader from the XMLada package.

with Sax.Attributes;       use Sax.Attributes;
with Unicode;              use Unicode;
with Unicode.CES;          use Unicode.CES;
with DOM.Core.Nodes;       use DOM.Core.Nodes;
with DOM.Core.Documents;   use DOM.Core.Documents;
with DOM.Core.Elements;    use DOM.Core.Elements;

package body SOAP.Message.Reader is

   ----------------
   -- Characters --
   ----------------

   procedure Characters
     (Handler : in out Tree_Reader;
      Ch      : in     Unicode.CES.Byte_Sequence)
   is
      Tmp : Node;
   begin
      Tmp := Append_Child
        (Handler.Current_Node, Create_Text_Node (Handler.Tree, Ch));
   end Characters;

   -----------------
   -- End_Element --
   -----------------

   procedure End_Element
     (Handler       : in out Tree_Reader;
      Namespace_URI : in     Unicode.CES.Byte_Sequence := "";
      Local_Name    : in     Unicode.CES.Byte_Sequence := "";
      Qname         : in     Unicode.CES.Byte_Sequence := "") is
   begin
      Handler.Current_Node := Parent_Node (Handler.Current_Node);
   end End_Element;

   --------------
   -- Get_Tree --
   --------------

   function Get_Tree (Read : in Tree_Reader) return Document is
   begin
      return Read.Tree;
   end Get_Tree;

   --------------------------
   -- Ignorable_Whitespace --
   --------------------------

   procedure Ignorable_Whitespace
     (Handler : in out Tree_Reader;
      Ch      : in     Unicode.CES.Byte_Sequence)
   is
      Tmp : Node;
   begin
      --  Ignore these white spaces at the toplevel
      if Ch'Length = 1
        and then Ch (Ch'First) /= ASCII.LF
        and then Handler.Current_Node /= Handler.Tree
      then
         Tmp := Append_Child
           (Handler.Current_Node, Create_Text_Node (Handler.Tree, Ch));
      end if;
   end Ignorable_Whitespace;

   --------------------
   -- Start_Document --
   --------------------

   procedure Start_Document (Handler : in out Tree_Reader) is
      Implementation : DOM_Implementation;
   begin
      Handler.Tree := Create_Document (Implementation);
      Handler.Current_Node := Handler.Tree;
   end Start_Document;

   -------------------
   -- Start_Element --
   -------------------

   procedure Start_Element
     (Handler       : in out Tree_Reader;
      Namespace_URI : in     Unicode.CES.Byte_Sequence       := "";
      Local_Name    : in     Unicode.CES.Byte_Sequence       := "";
      Qname         : in     Unicode.CES.Byte_Sequence       := "";
      Atts          : in     Sax.Attributes.Attributes'Class) is
   begin
      Handler.Current_Node := Append_Child
        (Handler.Current_Node,
         Create_Element_NS (Handler.Tree,
                            Namespace_URI => Namespace_URI,
                            Qualified_Name => Qname));

      --  Insert the attributes in the right order.
      for J in 0 .. Get_Length (Atts) - 1 loop
         Set_Attribute_NS
           (Handler.Current_Node,
            Get_URI (Atts, J),
            Get_Qname (Atts, J),
            Get_Value (Atts, J));
      end loop;
   end Start_Element;

end SOAP.Message.Reader;
