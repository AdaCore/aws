------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2015, AdaCore                     --
--                                                                          --
--  This is free software;  you can redistribute it  and/or modify it       --
--  under terms of the  GNU General Public License as published  by the     --
--  Free Software  Foundation;  either version 3,  or (at your option) any  --
--  later version.  This software is distributed in the hope  that it will  --
--  be useful, but WITHOUT ANY WARRANTY;  without even the implied warranty --
--  of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU     --
--  General Public License for  more details.                               --
--                                                                          --
--  You should have  received  a copy of the GNU General  Public  License   --
--  distributed  with  this  software;   see  file COPYING3.  If not, go    --
--  to http://www.gnu.org/licenses for a complete copy of the license.      --
------------------------------------------------------------------------------

with AWS.Containers.Tables;

with Ada.Text_IO;
with Ada.Strings.Unbounded;
with System.Assertions;

procedure CTab is
   use AWS.Containers.Tables;
   use Ada.Text_IO;
   use System.Assertions;

   Table : Table_Type;

   procedure Print_Table;

   procedure Print_Table is
      Names : constant VString_Array := Table.Get_Names;
   begin
      Put_Line ("----------------------------");
      for K in 1 .. Name_Count (Table) loop
         declare
            Name : String := Ada.Strings.Unbounded.To_String (Names (K));
            Last : Natural := Table.Count (Name);
         begin
            Put (Name & ": ");
            for L in 1 .. Last loop
               Put (Get (Table, Name, L));

               if L = Last then
                  Put_Line (";");
               else
                  Put (", ");
               end if;
            end loop;
         end;
      end loop;
   end Print_Table;

begin
   Reset (Table);
   Case_Sensitive (Table, False);
   Table.Add ("name_1", "value_1_1");
   Table.Add ("Name_1", "value_1_2");
   Table.Add ("NAME_1", "value_1_3");
   Table.Add ("name_2", "value_2_1");
   Table.Add ("Name_2", "value_2_2");
   Table.Add ("NAME_2", "value_2_3");
   Table.Add ("name_3", "value_3_1");
   Table.Add ("Name_3", "value_3_2");
   Table.Add ("NAME_3", "value_3_3");
   Table.Add ("name_4", "value_4_1");
   Table.Add ("Name_4", "value_4_2");
   Table.Add ("NAME_4", "value_4_3");
   Print_Table;

   Update (Table, "NAme_1", "Value (1, 4)", 4);
   Update (Table, "NAME_1", "Value (1, 3)", 3);
   Update (Table, "Name_1", "Value (1, 2)", 2);
   Update (Table, "name_1", "Value (1, 1)", 1);

   Update (Table, "NAme_2", "Value (2, 4)", 4);
   Update (Table, "NAME_2", "Value (2, 3)", 3);
   Update (Table, "Name_2", "Value (2, 2)", 2);
   Update (Table, "name_2", "Value (2, 1)", 1);

   Update (Table, "NAme_3", "Value (3, 4)", 4);
   Update (Table, "NAME_3", "Value (3, 3)", 3);
   Update (Table, "Name_3", "Value (3, 2)", 2);
   Update (Table, "name_3", "Value (3, 1)", 1);

   Update (Table, "name_4", "Value (4, 1)", 1);
   Update (Table, "Name_4", "Value (4, 2)", 2);
   Update (Table, "NAME_4", "Value (4, 3)", 3);
   Update (Table, "NAme_4", "Value (4, 4)", 4);

   Update (Table, "name_5", "Value (5, 1)", 1);
   Update (Table, "Name_5", "Value (5, 2)", 2);
   Update (Table, "NAME_5", "Value (5, 3)", 3);
   Update (Table, "NAme_5", "Value (5, 4)", 4);

   Print_Table;

   begin
      Table.Update ("NAme_5", "Value", 6);
      Put_Line ("Error.");
   exception
      when Constraint_Error | Assert_Failure => null;
   end;

   begin
      Table.Update ("NAme_6", "Value", 2);
      Put_Line ("Error.");
   exception
      when Constraint_Error  | Assert_Failure =>
         Put_Line ("Ok.");
   end;
end CTab;
