------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2003-2004                          --
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
--
--  $Id$
--
--  Generator for fast regression test execution.

with Ada.Command_Line;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with AWS.OS_Lib;

procedure Generate is

   use Ada.Text_IO;
   use Ada.Command_Line;

   package U renames Ada.Strings.Unbounded;

   Main   : File_Type;
   Source : U.Unbounded_String
     := U.To_Unbounded_String
          ("procedure Test_All is" & ASCII.LF
         & "   Std_Out : File_Type;" & ASCII.LF
         & "begin"  & ASCII.LF);

   procedure Test (Name : in String);

   ----------
   -- Test --
   ----------

   procedure Test (Name : in String) is
      Spec : File_Type;
      Res_Ext : constant String := ".res";
      Lf      : Character renames ASCII.Lf;
   begin
      Put_Line (Main, "with " & Name & ';');

      U.Append
        (Source,
         "   Put_Line (Standard_Output, ""Running   " & Name & """);" & Lf
           & "   Create (Std_Out, Out_File, """ & Name & Res_Ext & """);" & Lf
           & "   Set_Output (Std_Out);" & Lf
           & "   begin" & Lf
           & "      " & Name & ';' & Lf
           & "   exception" & Lf
           & "      when E : others =>" & Lf
           & "         Put_Line (""Test " & Name & " failed."");" & Lf
           & "         Put_Line (Exception_Information (E));" & Lf
           & "   end;" & Lf
           & "   Close (Std_Out);" & Lf
           & "   Print_Tasks;" & Lf & Lf);
   end Test;

begin
   Create (Main, Out_File, "test_all.adb");
   Put_Line (Main, "with Ada.Text_IO; use Ada.Text_IO;");
   Put_Line (Main, "with Ada.Exceptions; use Ada.Exceptions;");
   Put_Line (Main, "with Print_Tasks;");

   for J in 1 .. Argument_Count loop
      Test (Argument (J));
   end loop;

   New_Line (Main);
   Put (Main, U.To_String (Source));
   Put_Line (Main, "   null;");
   Put_Line (Main, "end Test_All;");
   Close (Main);
end Generate;
