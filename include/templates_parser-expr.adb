------------------------------------------------------------------------------
--                             Templates Parser                             --
--                                                                          --
--                        Copyright (C) 1999 - 2001                         --
--                               Pascal Obry                                --
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

with Ada.Text_IO;

separate (Templates_Parser)

package body Expr is

   -----------
   -- Image --
   -----------

   function Image (O : in Ops) return String is
   begin
      case O is
         when O_And   => return "and";
         when O_Or    => return "or";
         when O_Xor   => return "xor";
         when O_Sup   => return ">";
         when O_Inf   => return "<";
         when O_Esup  => return ">=";
         when O_Einf  => return "<=";
         when O_Equal => return "=";
      end case;
   end Image;

   -----------
   -- Parse --
   -----------

   function Parse (Expression : in String) return Tree is

      Index : Natural := Expression'First;

      function Get_Token return String;
      --  Returns next token. Set Index to the last analysed position in
      --  Expression.

      function No_Quote (Str : in String) return String;
      --  Removes quotes around Str. If Str (Str'First) and Str (Str'Last)
      --  are quotes return Str (Str'First + 1 ..  Str'Last - 1) otherwise
      --  return Str as-is.

      ---------------
      -- Get_Token --
      ---------------

      function Get_Token return String is
         use Strings;
         K, I  : Natural;
      begin
         if Index > Expression'Last then
            --  No more data to read.
            return "";
         end if;

         Index := Fixed.Index_Non_Blank
           (Expression (Index .. Expression'Last));

         if Index = 0 then
            --  There is only one token, return the whole string.
            Index := Expression'Last + 1;
            return Expression (Index .. Expression'Last);

         elsif Expression (Index) = '(' then
            --  This is a sub-expression, returns it.
            K := 0;

            declare
               L : Natural := 1;
            begin
               Look_For_Sub_Exp : for I in Index + 1 .. Expression'Last loop
                  if Expression (I) = '(' then
                     L := L + 1;
                  elsif Expression (I) = ')' then
                     K := I;
                     L := L - 1;
                  end if;

                  exit Look_For_Sub_Exp when L = 0;
               end loop Look_For_Sub_Exp;
            end;

            if K = 0 then
               --  No matching closing parenthesis.

               Exceptions.Raise_Exception
                 (Internal_Error'Identity,
                  "condition, no matching parenthesis for parent at pos "
                  & Natural'Image (Index));

            else
               I := Index;
               Index := K + 1;
               return Expression (I .. K);
            end if;

         else
            --  We have found the start of a token, look for end of it.
            K := Fixed.Index (Expression (Index .. Expression'Last), Blank);

            if K = 0 then
               --  Token end is the end of Expression.
               I := Index;
               Index := Expression'Last + 1;
               return Expression (I .. Expression'Last);
            else
               I := Index;
               Index := K + 1;
               return Expression (I .. K - 1);
            end if;
         end if;
      end Get_Token;

      --------------
      -- No_Quote --
      --------------

      function No_Quote (Str : in String) return String is
      begin
         if Str (Str'First) = '"' and then Str (Str'Last) = '"' then
            return Str (Str'First + 1 .. Str'Last - 1);
         else
            return Str;
         end if;
      end No_Quote;

      L_Tok : constant String := Get_Token;  -- left operand
      O_Tok : constant String := Get_Token;  -- operator
      R_Tok : constant String := Get_Token;  -- right operand

   begin
      if O_Tok = "" then
         --  No more operator, this is a leaf. It is either a variable or a
         --  value.

         if L_Tok (L_Tok'First) = '(' then
            --  an expression
            return Parse (L_Tok (L_Tok'First + 1 .. L_Tok'Last - 1));

         elsif Strings.Fixed.Index (L_Tok, To_String (Begin_Tag)) = 0 then
            --  a value
            return new Node'(Value, To_Unbounded_String (No_Quote (L_Tok)));

         else
            --  a variable
            return new Node'(Var, Build (No_Quote (L_Tok)));
         end if;


      else
         if Index > Expression'Last then
            --  This is the latest token

            return new Node'(Op, Value (O_Tok),
                             Parse (L_Tok), Parse (R_Tok));

         else
            declare
               NO_Tok : constant String := Get_Token;
            begin
               return new Node'
                 (Op, Value (NO_Tok),
                  Parse (L_Tok & ' ' & O_Tok & ' ' & R_Tok),
                  Parse (Expression (Index .. Expression'Last)));
            end;
         end if;
      end if;
   end Parse;

   ----------------
   -- Print_Tree --
   ----------------

   procedure Print_Tree (E : in Tree) is
   begin
      case E.Kind is
         when Value =>
            Text_IO.Put (To_String (E.V));

         when Var =>
            Text_IO.Put (Image (E.Var));

         when Op =>
            Text_IO.Put ('(');
            Print_Tree (E.Left);
            Text_IO.Put (' ' & Image (E.O) & ' ');
            Print_Tree (E.Right);
            Text_IO.Put (')');
      end case;
   end Print_Tree;

   -------------
   -- Release --
   -------------

   procedure Release (E : in out Tree) is
      procedure Free is new Ada.Unchecked_Deallocation (Node, Tree);
   begin
      case E.Kind is
         when Value =>
            null;

         when Var =>
            Release (E.Var);

         when Op =>
            Release (E.Left);
            Release (E.Right);
      end case;

      Free (E);
   end Release;

   -----------
   -- Value --
   -----------

   function Value (O : in String) return Ops is
   begin
      if O = "and" then
         return O_And;

      elsif O = "or" then
         return O_Or;

      elsif O = "xor" then
         return O_Xor;

      elsif O = ">" then
         return O_Sup;

      elsif O = "<" then
         return O_Inf;

      elsif O = ">=" then
         return O_Esup;

      elsif O = "<=" then
         return O_Einf;

      elsif O = "=" then
         return O_Equal;

      else
         Exceptions.Raise_Exception
           (Internal_Error'Identity, "condition, unknown operator " & O);
      end if;
   end Value;

end Expr;
