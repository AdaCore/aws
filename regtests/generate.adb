------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                            Copyright (C) 2003                            --
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
--  Generator for the fast regression test executor.

with Ada.Strings.Unbounded;
with Ada.Text_IO;

with AWS.OS_Lib;

procedure Generate is

   use Ada.Text_IO;
   package U renames Ada.Strings.Unbounded;

   Main   : File_Type;
   Diff   : File_Type;
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
      if not AWS.OS_Lib.Is_Regular_File (Name & ".ads") then
         Create (Spec, Out_File, Name & ".ads");
         Put_Line (Spec, "procedure " & Name & ';');
         Close (Spec);
      end if;

      Put_Line (Main, "with " & Name & ';');
      Put_Line (Diff, "diff -c -w " & Name & ".out " & Name & Res_Ext);

      U.Append
        (Source,
         "   Put_Line (Standard_Output, """ & Name & """);" & Lf
       & "   Create (Std_Out, Out_File, """ & Name & Res_Ext & """);" & Lf
       & "   Set_Output (Std_Out);" & Lf
       & "   " & Name & ';' & Lf
       & "   Close (Std_Out);" & Lf & Lf);
   end Test;

begin
   Create (Main, Out_File, "test_all.adb");
   Create (Diff, Out_File, "diff_all.sh");
   Put_Line (Main, "with Ada.Text_IO; use Ada.Text_IO;");

   --  dependent from dispatch.ini Test ("dispatch");
   --  dependent from dispatch_method.ini Test ("dispatch_method");
   --  dependent from dispatch_vh.ini Test ("dispatch_vh");
   --  dependent from program name Test ("tlog");
   --  dependent from upload UID Test ("upload2");
   --  dependent from upload UID Test ("upload3");
   --  dependent from program name Test ("server_config");

   Test ("append");
   Test ("auth");
   Test ("auth2");
   Test ("compress");
   Test ("ctab");
   Test ("dirop");
   Test ("dummy");
   Test ("eres");
   Test ("file");
   Test ("file2");
   Test ("get_post");
   Test ("head");
   Test ("hostip");
   Test ("huge_response");
   Test ("hval");
   Test ("moved");
   Test ("multiple");
   Test ("once");
   Test ("param");
   Test ("redirect");
   Test ("sback");
   Test ("server_info");
   Test ("sessions");
   Test ("sessions2");
   Test ("sessions3");
   Test ("shutdown");
   Test ("simple");
   Test ("sock1");
   Test ("sp");
   Test ("split");
   Test ("strm");
   Test ("strm2");
   Test ("tclientto");
   Test ("tcom");
   Test ("test_templates_if");
   Test ("tgetparam");
   Test ("timer");
   Test ("tmem");
   Test ("tmime");
   Test ("tres2");
   Test ("turl");
   Test ("turl2");
   Test ("turl3");
   Test ("turl4");
   Test ("unexph");
   Test ("unexph2");
   Test ("upload");
   Test ("zapp");
   Test ("zbig");
   Test ("zfile");
   Test ("zopen");
   Test ("zstrm");
   Test ("zstrm2");

   New_Line (Main);
   Put (Main, U.To_String (Source));
   Put_Line (Main, "end Test_All;");
   Close (Main);
   Close (Diff);
end Generate;