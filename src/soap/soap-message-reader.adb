------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2015, AdaCore                     --
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

--  This package is based on Tree_Reader from the XMLada package

with DOM.Core.Documents;   use DOM.Core.Documents;
with DOM.Core.Elements;    use DOM.Core.Elements;
with DOM.Core.Nodes;       use DOM.Core.Nodes;
with Sax.Attributes;       use Sax.Attributes;
with Unicode;              use Unicode;
with Unicode.CES;          use Unicode.CES;

with SOAP.Utils;

package body SOAP.Message.Reader is

   ----------------
   -- Characters --
   ----------------

   overriding procedure Characters
     (Handler : in out Tree_Reader;
      Ch      : Unicode.CES.Byte_Sequence)
   is
      Tmp : Node with Unreferenced;
   begin
      declare
         --  Ch comes from the SAX parser and is Utf8 encoded. We convert
         --  it back to Basic_8bit (standard Ada strings). Depending on
         --  Ch's value this could raise an exception. For example if
         --  Ch is Utf32 encoded and contains characters outside the
         --  Basic_8bit encoding.
         S : constant String_Access := Utils.From_Utf8 (Ch);
      begin
         Tmp := Append_Child
           (Handler.Current_Node,
            Create_Text_Node (Handler.Tree, DOM_String_Access (S)));
      end;
   exception
      when Unicode.CES.Invalid_Encoding =>
         --  Here we had a problem decoding the string, just keep Ch as-is
         Tmp := Append_Child
           (Handler.Current_Node, Create_Text_Node (Handler.Tree, Ch));
   end Characters;

   -----------------
   -- End_Element --
   -----------------

   overriding procedure End_Element
     (Handler       : in out Tree_Reader;
      Namespace_URI : Unicode.CES.Byte_Sequence := "";
      Local_Name    : Unicode.CES.Byte_Sequence := "";
      Qname         : Unicode.CES.Byte_Sequence := "")
   is
      pragma Unreferenced (Namespace_URI);
      pragma Unreferenced (Local_Name);
      pragma Unreferenced (Qname);
   begin
      Handler.Current_Node := Parent_Node (Handler.Current_Node);
   end End_Element;

   --------------
   -- Get_Tree --
   --------------

   function Get_Tree (Read : Tree_Reader) return DOM.Core.Document is
   begin
      return Read.Tree;
   end Get_Tree;

   --------------------------
   -- Ignorable_Whitespace --
   --------------------------

   overriding procedure Ignorable_Whitespace
     (Handler : in out Tree_Reader;
      Ch      : Unicode.CES.Byte_Sequence)
   is
      Tmp : Node with Unreferenced;
   begin
      --  Ignore these white spaces at the toplevel
      if Ch'Length >= 1
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

   overriding procedure Start_Document (Handler : in out Tree_Reader) is
      Implementation : DOM_Implementation;
   begin
      Handler.Tree := Create_Document (Implementation);
      Handler.Current_Node := Handler.Tree;
   end Start_Document;

   -------------------
   -- Start_Element --
   -------------------

   overriding procedure Start_Element
     (Handler       : in out Tree_Reader;
      Namespace_URI : Unicode.CES.Byte_Sequence       := "";
      Local_Name    : Unicode.CES.Byte_Sequence       := "";
      Qname         : Unicode.CES.Byte_Sequence       := "";
      Atts          : Sax.Attributes.Attributes'Class)
   is
      pragma Unreferenced (Local_Name);
   begin
      Handler.Current_Node := Append_Child
        (Handler.Current_Node,
         Create_Element_NS (Handler.Tree,
                            Namespace_URI => Namespace_URI,
                            Qualified_Name => Qname));

      --  Insert the attributes in the right order
      for J in 0 .. Get_Length (Atts) - 1 loop
         Set_Attribute_NS
           (Handler.Current_Node,
            Get_URI (Atts, J),
            Get_Qname (Atts, J),
            Get_Value (Atts, J));
      end loop;
   end Start_Element;

begin
   DOM.Core.Set_Node_List_Growth_Factor (1.0);
end SOAP.Message.Reader;
