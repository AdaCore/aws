------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                            Copyright (C) 2002                            --
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

--  This programs create a parent package Root_Pck and one children for any
--  files passed as argument.

with Ada.Calendar;
with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Streams.Stream_IO;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Strings.Unbounded;

with AWS.OS_Lib;
with GNAT.Command_Line;
with GNAT.Calendar.Time_IO;

procedure AwsRes is

   use Ada;
   use Ada.Strings.Unbounded;

   Syntax_Error : exception;

   Version  : constant String := "0.2";

   Root_Pck : Unbounded_String := To_Unbounded_String ("res");
   Quiet    : Boolean := False;

   RT_File   : Text_IO.File_Type;
   --  Root temp file.

   R_File   : Text_IO.File_Type;
   --  Root spec/body file.

   procedure Create (Filename : in String);
   --  Create resource package for Filename.

   function Package_Name (Filename : in String) return String;
   --  Returns package name for Filename.

   procedure Parse_Command_Line;
   --  Parse command line.

   function Header return String;
   --  Returns file header (AWSRes version, date, time).

   ------------
   -- Create --
   ------------

   procedure Create (Filename : in String) is
      use Streams;

      Max_Data  : constant := 14;
      --  Maximum number of data in a single line.

      Unit_Name : constant String := Package_Name (Filename);
      Pck_Name  : constant String
        := To_String (Root_Pck) & '-' & Unit_Name & ".ads";

      Buffer    : Stream_Element_Array (1 .. 4_096);
      Last      : Stream_Element_Offset;
      I         : Natural;

      File_Time : Calendar.Time;

      O_File    : Text_IO.File_Type;
      I_File    : Stream_IO.File_Type;

      First     : Boolean := True;
   begin
      if not Quiet then
         Text_IO.Put ("creating " & Filename);
      end if;

      File_Time := AWS.OS_Lib.File_Timestamp (Filename);

      Text_IO.Create (O_File, Text_IO.Out_File, Pck_Name);
      Stream_IO.Open (I_File, Stream_IO.In_File, Filename);

      --  Output package declaration

      Text_IO.New_Line (O_File);
      Text_IO.Put_Line (O_File, Header);
      Text_IO.New_Line (O_File);

      Text_IO.Put_Line (O_File, "with Ada.Streams;");
      Text_IO.New_Line (O_File);

      Text_IO.Put_Line (O_File, "package "
                          & To_String (Root_Pck) & '.' & Unit_Name & " is");
      Text_IO.New_Line (O_File);

      Text_IO.Put_Line (O_File, "   use Ada.Streams;");
      Text_IO.New_Line (O_File);

      Text_IO.Put_Line
        (O_File, "   Content : aliased constant Stream_Element_Array :=");

      --  Output file content

      I := 0;

      loop
         Stream_IO.Read (I_File, Buffer, Last);

         exit when Last < Buffer'First;

         for K in Buffer'First .. Last loop
            if I /= 0 then
               Text_IO.Put (O_File, ",");

               if I = Max_Data then
                  Text_IO.New_Line (O_File);
                  Text_IO.Put (O_File, "       ");
                  I := 0;
               end if;
            end if;

            if First then
               --  No space after the open parentesis (style check).
               declare
                  V : constant Integer := Integer (Buffer (K));
               begin
                  if V < 10 then
                     Text_IO.Put
                       (O_File, "         (");
                     Integer_Text_IO.Put (O_File, V, Width => 1);
                  elsif V < 100 then
                     Text_IO.Put
                       (O_File, "        (");
                     Integer_Text_IO.Put (O_File, V, Width => 2);
                  else
                     Text_IO.Put
                       (O_File, "       (");
                     Integer_Text_IO.Put (O_File, V, Width => 3);
                  end if;
               end;
               First := False;

            else
               Integer_Text_IO.Put (O_File, Integer (Buffer (K)), Width => 4);
            end if;

            I := I + 1;

            if not Quiet and then K mod 400 = 0 then
               Text_IO.Put ('.');
            end if;
         end loop;
      end loop;

      Text_IO.Put_Line
        (O_File, ");");

      --  Output end of package

      Text_IO.New_Line (O_File);
      Text_IO.Put_Line
        (O_File, "end " & To_String (Root_Pck) & '.' & Unit_Name & ';');

      Stream_IO.Close (I_File);
      Text_IO.Close (O_File);

      if not Quiet then
         Text_IO.New_Line;
      end if;

      --  Register package into root package body

      Text_IO.Put_Line
        (RT_File, "         Register");
      Text_IO.Put_Line
        (RT_File, "            (""" & Filename & """,");
      Text_IO.Put_Line
        (RT_File, "             "
           & To_String (Root_Pck) & '.' & Unit_Name & ".Content'Access,");
      Text_IO.Put_Line
        (RT_File, "             GNAT.Calendar.Time_Of ("
           & GNAT.Calendar.Time_IO.Image
           (File_Time, "%Y, %m, %d, %H, %M, %S, 0.0));"));

      if not Quiet then
         Text_IO.Put_Line ("  -> registered");
      end if;

      --  Add with clause to root body

      Text_IO.Put_Line
        (R_File, "with " & To_String (Root_Pck) & '.' & Unit_Name & ';');
   end Create;

   ------------
   -- Header --
   ------------

   function Header return String is
   begin
      return "--  AWSRes v" & Version & " - Genarated on " &
        GNAT.Calendar.Time_IO.Image
        (Calendar.Clock, "%B %d %Y at %T");
   end Header;

   ------------------
   -- Package_Name --
   ------------------

   function Package_Name (Filename : in String) return String is
      From : constant String := ".";
      To   : constant String := "_";

      Map  : constant Strings.Maps.Character_Mapping
        := Strings.Maps.To_Mapping (From, To);
   begin
      return Strings.Fixed.Translate (Filename, Map);
   end Package_Name;

   ------------------------
   -- Parse_Command_Line --
   ------------------------

   procedure Parse_Command_Line is
   begin
      loop
         case GNAT.Command_Line.Getopt ("r: h q") is
            when ASCII.NUL =>
               exit;

            when 'r' =>
               Root_Pck := To_Unbounded_String (GNAT.Command_Line.Parameter);

            when 'q' =>
               Quiet := True;

            when 'h' =>
               raise Syntax_Error;

            when others =>
               raise Syntax_Error;
         end case;
      end loop;
   end Parse_Command_Line;

   Buffer : String (1 .. 100);
   Last   : Natural;

begin
   Parse_Command_Line;

   if not Quiet then
      Text_IO.Put_Line ("AWSRes - Resource Creator v" & Version);
      Text_IO.New_Line;
   end if;

   Text_IO.Create (RT_File, Text_IO.Out_File);
   Text_IO.Create (R_File, Text_IO.Out_File, To_String (Root_Pck) & ".adb");

   Text_IO.New_Line (R_File);
   Text_IO.Put_Line (R_File, Header);
   Text_IO.New_Line (R_File);

   Text_IO.New_Line (RT_File);
   Text_IO.Put_Line (RT_File, "with AWS.Resources.Embedded;");
   Text_IO.Put_Line (RT_File, "with GNAT.Calendar;");
   Text_IO.New_Line (RT_File);
   Text_IO.Put_Line (RT_File, "package body " & To_String (Root_Pck) & " is");
   Text_IO.New_Line (RT_File);

   Text_IO.Put_Line (RT_File, "   Initialized : Boolean := False;");
   Text_IO.New_Line (RT_File);
   Text_IO.Put_Line (RT_File, "   procedure Init is");
   Text_IO.Put_Line (RT_File, "      use AWS.Resources.Embedded;");
   Text_IO.Put_Line (RT_File, "   begin");
   Text_IO.Put_Line (RT_File, "      if not Initialized then");
   Text_IO.Put_Line (RT_File, "         Initialized := True;");

   --  Parse all files
   loop
      declare
         S : constant String
           := GNAT.Command_Line.Get_Argument (Do_Expansion => True);
      begin
         exit when S'Length = 0;
         Create (S);
      end;
   end loop;

   Text_IO.Put_Line (RT_File, "      end if;");
   Text_IO.Put_Line (RT_File, "   end Init;");
   Text_IO.New_Line (RT_File);
   Text_IO.Put_Line (RT_File, "begin");
   Text_IO.Put_Line (RT_File, "   Init;");
   Text_IO.Put_Line (RT_File, "end " & To_String (Root_Pck) & ";");

   --  Copy now all the temp root file into the body file

   Text_IO.Reset (RT_File, Text_IO.In_File);

   while not Text_IO.End_Of_File (RT_File) loop
      Text_IO.Get_Line (RT_File, Buffer, Last);
      Text_IO.Put_Line (R_File, Buffer (1 .. Last));
   end loop;

   Text_IO.Close (RT_File);
   Text_IO.Close (R_File);

   --  Generate now the root package spec

   Text_IO.Create (R_File, Text_IO.Out_File, To_String (Root_Pck) & ".ads");

   Text_IO.New_Line (R_File);
   Text_IO.Put_Line (R_File, Header);
   Text_IO.New_Line (R_File);
   Text_IO.Put_Line (R_File, "package " & To_String (Root_Pck) & " is");
   Text_IO.New_Line (R_File);

   Text_IO.Put_Line (R_File, "   procedure Init;");
   Text_IO.Put_Line (R_File, "   --  Register all resources files");

   Text_IO.New_Line (R_File);
   Text_IO.Put_Line (R_File, "end " & To_String (Root_Pck) & ";");

   Text_IO.Close (R_File);

exception
   when Syntax_Error =>
      Text_IO.Put_Line ("AWSRes - Resource Creator v" & Version);
      Text_IO.New_Line;
      Text_IO.Put_Line ("Usage : awsres [-hrq] file1 [file2...]");
      Text_IO.New_Line;
      Text_IO.Put_Line
        ("        -h      : display help");
      Text_IO.Put_Line
        ("        -r name : name of the root package (default res)");
      Text_IO.Put_Line
        ("        -q      : quiet mode");
end AwsRes;
