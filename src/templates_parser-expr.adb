------------------------------------------------------------------------------
--                             Templates Parser                             --
--                                                                          --
--                        Copyright (C) 1999 - 2004                         --
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

   use Ada.Strings.Maps;

   --  BNF definition of the expression language:
   --
   --  <expr>     ::= <relation> {<Logic_Op> <relation>}
   --  <relation> ::= <term> {<comp_op> <term>}
   --  <term>     ::= ["not"] <primary>
   --  <primary>  ::= <value> | <var> | "(" <expr> ")"
   --  <logic_op> ::= "and" | "or" | "xor"
   --  <comp_op>  ::= "<" | "<=" | "=" | ">=" | ">" | "/="

   subtype Comp_Op  is Ops range O_Sup .. O_Diff;
   subtype Logic_Op is Ops range O_And .. O_Xor;

   Separator : constant Character_Set := Blank or To_Set ("<>=/()");

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
         when O_Diff  => return "/=";
      end case;
   end Image;

   function Image (O : in U_Ops) return String is
   begin
      case O is
         when O_Not => return "not";
      end case;
   end Image;

   -----------
   -- Parse --
   -----------

   function Parse (Expression : in String) return Tree is

      Start_Index : Natural := Expression'First;
      Index       : Natural := Expression'First;

      type Token_Kind
        is (Open_Par, Close_Par, Binary_Op, Unary_Op, Value, Var, End_Expr);

      type Token (Kind : Token_Kind := Var) is record
         case Kind is
            when Open_Par | Close_Par | End_Expr =>
               null;
            when Binary_Op =>
               Bin_Op : Ops;
            when Unary_Op =>
               Un_Op  : U_Ops;
            when Value | Var =>
               Start  : Positive; -- range of the token
               Stop   : Positive; -- in Expression string
         end case;
      end record;

      Current_Token : Token;

      procedure Error (Mess : String);
      pragma No_Return (Error);
      --  Raises Internal_Error with the column of the condition

      function Expr return Tree;
      --  Parse a logical operator

      function Term return Tree;
      --  Parse a term (unary operator)

      function Relation return Tree;
      --  Parse a relational operator

      -----------
      -- Error --
      -----------

      procedure Error (Mess : String) is
      begin
         Exceptions.Raise_Exception
           (Internal_Error'Identity,
            "col" & Integer'Image (Start_Index) & " condition, " & Mess);
      end Error;

      procedure Next_Token;
      --  Moves Current_Token to next token. Set Index after the last analysed
      --  consumed from expression.

      ----------------
      -- Next_Token --
      ----------------

      procedure Next_Token is
         use Ada.Strings, Ada.Characters.Handling;
         I : Natural;
      begin
         --  Skip blanks

         while Index <= Expression'Last
           and then Is_In (Expression (Index), Blank)
         loop
            Index := Index + 1;
         end loop;

         Start_Index := Index;

         if Index > Expression'Last then
            --  No more data to read.
            Current_Token := (Kind => End_Expr);

         --  Check symbolic operators
         elsif Expression (Index) = '(' then
            Current_Token := (Kind => Open_Par);
            Index := Index + 1;

         elsif Expression (Index) = ')' then
            Current_Token := (Kind => Close_Par);
            Index := Index + 1;

         elsif Expression (Index) = '=' then
            Current_Token := (Kind => Binary_Op, Bin_Op => O_Equal);
            Index := Index + 1;

         elsif Expression (Index) = '/' then
            Index := Index + 1;
            if Expression (Index) = '=' then
               Current_Token := (Kind => Binary_Op, Bin_Op => O_Diff);
               Index := Index + 1;
            else
               Error ("illegal comparison operator");
            end if;

         elsif Expression (Index) = '<' then
            Index := Index + 1;
            if Expression (Index) = '=' then
               Current_Token := (Kind => Binary_Op, Bin_Op => O_Einf);
               Index := Index + 1;
            else
               Current_Token := (Kind => Binary_Op, Bin_Op => O_Inf);
            end if;

         elsif Expression (Index) = '>' then
            Index := Index + 1;
            if Expression (Index) = '=' then
               Current_Token := (Kind => Binary_Op, Bin_Op => O_Esup);
               Index := Index + 1;
            else
               Current_Token := (Kind => Binary_Op, Bin_Op => O_Sup);
            end if;

         elsif Expression (Index) = '"' then
            --  This is a string, return it
            Current_Token
              := (Kind => Value, Start => Index + 1, Stop => Index);

            loop
               if Current_Token.Stop = Expression'Last then
                  Error ("condition, no matching closing quote string");
               elsif Expression (Current_Token.Stop + 1) = '"' then
                  exit;
               else
                  Current_Token.Stop := Current_Token.Stop + 1;
               end if;
            end loop;
            Index := Current_Token.Stop + 2;

         else
            --  We have found the start of a string token, look for end of it.
            I := Index;
            Index := Fixed.Index
              (Expression (Index .. Expression'Last), Separator);

            if Index = 0 then
               --  Token end is the end of Expression.
               Index := Expression'Last + 1;
            end if;

            declare
               Token_Image : constant String
                 := To_Lower (Expression (I .. Index - 1));
            begin
               if Token_Image = "not" then
                  Current_Token := (Kind => Unary_Op, Un_Op => O_Not);

               elsif Token_Image = "and" then
                  Current_Token := (Kind => Binary_Op, Bin_Op => O_And);

               elsif Token_Image = "or" then
                  Current_Token := (Kind => Binary_Op, Bin_Op => O_Or);

               elsif Token_Image = "xor" then
                  Current_Token := (Kind => Binary_Op, Bin_Op => O_Xor);

               elsif Token_Image'Length > Length (Begin_Tag)
                 and then
                   Token_Image (Token_Image'First
                                .. Token_Image'First + Length (Begin_Tag) - 1)
                   = Begin_Tag
               then
                  --  This is a variable, we have the start of it, now look
                  --  for the end of the variable.

                  if Index <= Expression'Last
                    and then Expression (Index) = '('
                  then
                     --  This is not the end of the tag variable but the
                     --  start of the tag parameters. Look for tag variable
                     --  end.
                     Index := Fixed.Index
                       (Expression (Index .. Expression'Last),
                        To_String (End_Tag));
                     Index := Index + Length (End_Tag);
                  end if;

                  if Index = 0 then
                     Error ("variable end not found");

                  else
                     Current_Token
                       := (Kind  => Var, Start => I, Stop  => Index - 1);
                  end if;

               else
                  Current_Token
                    := (Kind => Value, Start => I, Stop => Index - 1);
               end if;
            end;
         end if;
      end Next_Token;

      -------------
      -- Primary --
      -------------

      function Primary return Tree is
         Result      : Tree;
         Start, Stop : Natural;
      begin
         case Current_Token.Kind is
            --  Normal cases
            when Open_Par =>
               Next_Token;
               Result := Expr;
               if Current_Token.Kind = Close_Par then
                  Next_Token;
                  return Result;
               else
                  Error ("missing closing parenthesis");
               end if;

            when Value =>
               Start := Current_Token.Start;
               Stop  := Current_Token.Stop;
               Next_Token;
               return new Node'
                 (Value, To_Unbounded_String (Expression (Start .. Stop)));

            when Var =>
               Start := Current_Token.Start;
               Stop  := Current_Token.Stop;
               Next_Token;
               return new Node'(Var, Build (Expression (Start .. Stop)));

            --  Errors

            when Unary_Op =>
               Error ("misplaced operator """
                      & Image (Current_Token.Un_Op) & '"');

            when Binary_Op =>
               Error ("misplaced operator """
                      & Image (Current_Token.Bin_Op) & '"');

            when Close_Par =>
               Error ("unexpected right parenthesis");

            when End_Expr =>
               Error ("missing operand");
         end case;
      end Primary;

      ----------
      -- Term --
      ----------

      function Term return Tree is
         O : U_Ops;
      begin
         if Current_Token.Kind = Unary_Op then
            O := Current_Token.Un_Op;
            Next_Token;
            return new Node'(U_Op, O, Primary);
         else
            return Primary;
         end if;
      end Term;

      --------------
      -- Relation --
      --------------

      function Relation return Tree is
         N : Tree;
         O : Ops;
      begin
         N := Term;

         while Current_Token.Kind = Binary_Op
           and then Current_Token.Bin_Op in Comp_Op
         loop
            O := Current_Token.Bin_Op;
            Next_Token;
            N := new Node'(Op, O, N, Term);
         end loop;

         return N;
      end Relation;

      ----------
      -- Expr --
      ----------

      function Expr return Tree is
         N : Tree;
         O : Ops;
      begin
         N := Relation;

         while Current_Token.Kind = Binary_Op
           and then Current_Token.Bin_Op in Logic_Op
         loop
            O := Current_Token.Bin_Op;
            Next_Token;
            N := new Node'(Op, O, N, Relation);
         end loop;

         return N;
      end Expr;

      Result : Tree;

   begin
      Next_Token;
      Result := Expr;

      case Current_Token.Kind is
         when End_Expr =>
            null;

         when Open_Par | Close_Par | Value | Var =>
            Error ("Missing operator");

         when Binary_Op | Unary_Op =>
            Error ("Missing operand");
      end case;

      return Result;
   end Parse;

   ----------------
   -- Print_Tree --
   ----------------

   procedure Print_Tree (E : in Tree) is
   begin
      case E.Kind is
         when Value =>
            declare
               Val : constant String := To_String (E.V);
               K   : constant Natural := Fixed.Index (Val, " ");
            begin
               if K = 0 then
                  Text_IO.Put (Val);
               else
                  Text_IO.Put ('"' & Val & '"');
               end if;
            end;

         when Var =>
            Text_IO.Put (Image (E.Var));

         when Op =>
            Text_IO.Put ('(');
            Print_Tree (E.Left);
            Text_IO.Put (' ' & Image (E.O) & ' ');
            Print_Tree (E.Right);
            Text_IO.Put (')');

         when U_Op =>
            Text_IO.Put ('(');
            Text_IO.Put (Image (E.U_O) & ' ');
            Print_Tree (E.Next);
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

         when U_Op =>
            Release (E.Next);
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

      elsif O = "/=" then
         return O_Diff;

      else
         Exceptions.Raise_Exception
           (Template_Error'Identity, "unknown operator " & O);
      end if;
   end Value;

   function Value (O : in String) return U_Ops is
   begin
      if O = "not" then
         return O_Not;

      else
         Exceptions.Raise_Exception
           (Template_Error'Identity, "unknown operator " & O);
      end if;
   end Value;

end Expr;
