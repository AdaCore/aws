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

procedure Print_Tree (T : in Tree; Level : in Natural := 0) is

   procedure Print_Indent (L : in Natural) is
      use Ada.Strings.Fixed;
   begin
      Text_IO.Put ((L * 2) * ' ');
   end Print_Indent;

begin
   if T = null then
      return;
   end if;

   Print_Indent (Level);

   case T.Kind is
      when Info =>
         Text_IO.Put_Line ("[INFO] " & To_String (T.Filename));
         declare
            I : Tree := T.I_File;
         begin
            while I /= null loop
               Text_IO.Put (" -> ");
               Text_IO.Put_Line (To_String (I.File.Info.Filename));
               I := I.Next;
            end loop;
         end;

         Print_Tree (T.Next, Level);

      when C_Info =>
         Text_IO.Put_Line ("[C_INFO] "
                           & Natural'Image (T.Used)
                           & ' ' & Boolean'Image (T.Obsolete));

         Print_Tree (T.Next, Level);

      when Text =>
         Text_IO.Put ("[TEXT] ");
         Data.Print_Tree (T.Text);
         Print_Tree (T.Next, Level);

      when Set_Stmt =>
         Text_IO.Put ("[SET_STMT] ");
         Definitions.Print_Tree (T.Def);
         Text_IO.New_Line;
         Print_Tree (T.Next, Level);

      when If_Stmt  =>
         Text_IO.Put ("[IF_STMT] ");
         Expr.Print_Tree (T.Cond);
         Text_IO.New_Line;
         Print_Tree (T.N_True, Level + 1);
         Print_Indent (Level);
         Text_IO.Put_Line ("[ELSE]");
         Print_Tree (T.N_False, Level + 1);
         Print_Indent (Level);
         Text_IO.Put_Line ("[END_IF_STMT]");
         Print_Tree (T.Next, Level);

      when Table_Stmt =>
         Text_IO.Put ("[TABLE_STMT]");

         if T.Terminate_Sections then
            Text_IO.Put (" TERMINATE_SECTIONS");
         end if;

         if T.Reverse_Index then
            Text_IO.Put (" REVERSE");
         end if;

         Text_IO.New_Line;
         Print_Tree (T.Blocks, Level + 1);
         Print_Indent (Level);
         Text_IO.Put_Line ("[END_TABLE_STMT]");
         Print_Tree (T.Next, Level);

      when Section_Block =>
         Text_IO.Put_Line ("[BLOCK]");

         if T.Common /= null then
            Print_Indent (Level + 1);
            Text_IO.Put_Line ("[COMMON]");
            Print_Tree (T.Common, Level + 2);
         end if;

         if T.Sections /= null then
            Print_Tree (T.Sections, Level + 1);
         end if;

         Print_Indent (Level);
         Text_IO.Put_Line ("[END_BLOCK]");
         Print_Tree (T.Next, Level);

      when Section_Stmt =>
         Text_IO.Put_Line ("[SECTION_STMT]");
         Print_Tree (T.Next, Level + 1);
         Print_Tree (T.N_Section, Level);

      when Include_Stmt =>
         Text_IO.Put_Line
           ("[INCLUDE_STMT] " & To_String (T.File.Info.Filename));

         declare
            use type Data.Tree;
         begin
            for K in T.I_Params'Range loop
               if T.I_Params (K) /= null then
                  Print_Indent (Level + 2);
                  Text_IO.Put ("$" & Image (K) & " = ");
                  Data.Print_Tree (T.I_Params (K));
                  Text_IO.New_Line;
               end if;
            end loop;
         end;

         Print_Tree (T.File.Info, Level + 1);
         Print_Tree (T.Next, Level);
   end case;

end Print_Tree;
