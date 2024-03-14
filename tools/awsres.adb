------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2002-2012, AdaCore                     --
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

--  This programs create a parent package Root_Pck and one children for any
--  files passed as argument.

with Ada.Calendar;
with Ada.Characters.Handling;
with Ada.Command_Line;
with Ada.Directories;
with Ada.Exceptions;
with Ada.Integer_Text_IO;
with Ada.Streams;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;

with GNAT.Calendar.Time_IO;
with GNAT.Command_Line;
with GNAT.Regexp;
with GNAT.SHA1;

with AWS.Resources.Streams.Disk;
with AWS.Resources.Streams.ZLib;
with AWS.Utils;

with ZLib;

procedure AwsRes is

   use Ada;
   use Ada.Exceptions;
   use Ada.Strings.Unbounded;
   use AWS;

   Syntax_Error : exception;

   Version  : constant String := "1.3";

   Glob_Pat : constant Strings.Maps.Character_Set :=
                Strings.Maps.To_Set ("*?");
   --  Globbing patterns

   Root_Pck : Unbounded_String := To_Unbounded_String ("res");
   Output   : Unbounded_String := To_Unbounded_String (".");
   Prefix   : Unbounded_String; --  prefix to resources names
   Quiet    : Boolean := False;
   Ada_Name : Boolean := False;

   RT_File  : Text_IO.File_Type;
   --  Root temp file

   R_File   : Text_IO.File_Type;
   --  Root spec/body file

   Compress : Boolean := False;
   --  By default resources are not compressed

   Recursive : Boolean := False;
   --  Do we need to recursively parse sub-directories

   procedure Create (Filename : String);
   --  Create resource package for Filename

   function Package_Name (Filename : String) return String;
   --  Returns package name for Filename

   procedure Parse_Command_Line;
   --  Parse command line

   function Header return String;
   --  Returns file header (AWSRes version, date, time)

   procedure Handle_Resource (Directory, Pattern : String);
   --  Parse the given directory for resources files

   function Output_Filename (Name : String) return String;
   --  Returns the pathname for the output file

   ------------
   -- Create --
   ------------

   procedure Create (Filename : String) is
      use Streams;

      package RS renames AWS.Resources.Streams;

      Max_Data  : constant := 14;
      --  Maximum number of data in a single line

      Unit_Name : constant String := Package_Name (Filename);
      Pck_Name  : constant String :=
                    To_String (Root_Pck) & '-' & Unit_Name & ".ads";

      Buffer    : Stream_Element_Array (1 .. 1_024 * 200);
      --  We need a buffer large enough to contain as much data as
      --  possible. This is more efficient for the compression, 200kb is
      --  certainly large enough for an embedded resource.

      Last      : Stream_Element_Offset;
      I         : Natural;

      File_Time : Calendar.Time;

      O_File    : Text_IO.File_Type;
      I_File    : RS.Stream_Access := new RS.Disk.Stream_Type;

      First     : Boolean := True;

      procedure Unchecked_Free is new Unchecked_Deallocation
        (RS.Stream_Type'Class, RS.Stream_Access);

   begin
      if not Quiet then
         Text_IO.Put ("creating " & Filename);
      end if;

      File_Time := Utils.File_Time_Stamp (Filename);

      begin
         Text_IO.Create (O_File, Text_IO.Out_File, Output_Filename (Pck_Name));
      exception
         when Text_IO.Use_Error =>
            Text_IO.New_Line;

            if Ada_Name and Recursive then
               raise Text_IO.Use_Error
                 with "Filename too long, remove -a option when -R used";
            else
               raise Text_IO.Use_Error
                 with "Cannot create embedded resource file";
            end if;
      end;

      RS.Disk.Open (RS.Disk.Stream_Type (I_File.all), Filename);

      if Compress then
         I_File := RS.ZLib.Deflate_Create (I_File, Header => ZLib.GZip);
      end if;

      --  Output package declaration

      Text_IO.New_Line (O_File);
      Text_IO.Put_Line (O_File, Header);
      Text_IO.New_Line (O_File);
      Text_IO.Put_Line (O_File, "pragma Style_Checks (Off);");
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
         RS.Read (I_File.all, Buffer, Last);

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
               --  No space after the open parentesis (style check)
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
               Integer_Text_IO.Put
                 (O_File, Integer (Buffer (K)), Width => 4);
            end if;

            I := I + 1;

            if not Quiet and then K mod 400 = 0 then
               Text_IO.Put ('.');
            end if;
         end loop;

         exit when Last < Buffer'Last;
      end loop;

      --  The resource was empty, nothing output yet

      if First then
         Text_IO.Put (O_File, "         (1 .. 0 => <>");
      end if;

      Text_IO.Put_Line (O_File, ");");

      --  Output end of package

      Text_IO.New_Line (O_File);
      Text_IO.Put_Line
        (O_File, "end " & To_String (Root_Pck) & '.' & Unit_Name & ';');

      RS.Close (I_File.all);
      Unchecked_Free (I_File);

      Text_IO.Close (O_File);

      if not Quiet then
         Text_IO.New_Line;
      end if;

      --  Register package into root package body

      Text_IO.Put_Line (RT_File, "         Register");

      declare
         Max_Len : constant := 50;

         --  The resource name must not have back-slash
         F_Name : constant String :=
                    To_String (Prefix)
                      & Strings.Fixed.Translate
                          (Filename, Strings.Maps.To_Mapping ("\", "/"));
         F, L   : Natural;
      begin
         Text_IO.Put (RT_File, "            (""");

         F := F_Name'First;

         loop
            L := Natural'Min (F_Name'Last, F + Max_Len);
            Text_IO.Put (RT_File, F_Name (F .. L));
            F := L + 1;

            exit when F > F_Name'Last;

            Text_IO.Put_Line (RT_File, """");
            Text_IO.Put (RT_File, "             & """);
         end loop;

         if Compress then
            Text_IO.Put_Line (RT_File, ".gz"",");
         else
            Text_IO.Put_Line (RT_File, """,");
         end if;
      end;

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

   ---------------------
   -- Handle_Resource --
   ---------------------

   procedure Handle_Resource (Directory, Pattern : String) is

      use Directories;

      File_And_Dir : constant Directories.Filter_Type :=
                       (Directories.Directory | Ordinary_File => True,
                        others                                => False);

      Regexp       : constant GNAT.Regexp.Regexp :=
                       GNAT.Regexp.Compile (Pattern, Glob => True);

      procedure Handle (Directory_Entry : Directory_Entry_Type);

      ------------
      -- Handle --
      ------------

      procedure Handle (Directory_Entry : Directory_Entry_Type) is
         Kind : File_Kind renames Directories.Kind (Directory_Entry);
      begin
         if Kind = Ordinary_File
           and then GNAT.Regexp.Match (Simple_Name (Directory_Entry), Regexp)
         then
            if Directory = "." then
               Create (Simple_Name (Directory_Entry));

            else
               Create (Compose (Directory, Simple_Name (Directory_Entry)));
            end if;

         elsif Recursive
           and then Kind = Directories.Directory
           and then Simple_Name (Directory_Entry) /= "."
           and then Simple_Name (Directory_Entry) /= ".."
         then
            if Directory = "." then
               Handle_Resource (Simple_Name (Directory_Entry), Pattern);
            else
               Handle_Resource
                 (Compose (Directory, Simple_Name (Directory_Entry)), Pattern);
            end if;
         end if;
      end Handle;

   begin
      Directories.Search (Directory, "*", File_And_Dir, Handle'Access);
   end Handle_Resource;

   ------------
   -- Header --
   ------------

   function Header return String is
   begin
      return "--  AWSRes v" & Version & " - Generated on " &
        GNAT.Calendar.Time_IO.Image (Calendar.Clock, "%B %d %Y at %T");
   end Header;

   -----------------
   -- Output_File --
   -----------------

   function Output_Filename (Name : String) return String is
      O : constant String := To_String (Output);
   begin
      if O = "." then
         return Name;
      else
         return Directories.Compose (O, Name);
      end if;
   end Output_Filename;

   ------------------
   -- Package_Name --
   ------------------

   function Package_Name (Filename : String) return String is
      From : constant String := "./\-";
      To   : constant String := "___x";

      Map  : constant Strings.Maps.Character_Mapping :=
               Strings.Maps.To_Mapping (From, To);
   begin
      if Ada_Name then
         return Characters.Handling.To_Lower
           (Strings.Fixed.Translate (Filename, Map));

      else
         --  Else encode package name using SHA1, this it can start with a
         --  digit prefix the result with "p_".

         return "p_" & GNAT.SHA1.Digest (Filename);
      end if;
   end Package_Name;

   ------------------------
   -- Parse_Command_Line --
   ------------------------

   procedure Parse_Command_Line is
   begin
      GNAT.Command_Line.Initialize_Option_Scan
        (Stop_At_First_Non_Switch => True);

      loop
         case GNAT.Command_Line.Getopt ("a r: h q z u o: p: R") is
            when ASCII.NUL =>
               exit;

            when 'a' =>
               Ada_Name := True;

            when 'o' =>
               Output := To_Unbounded_String (GNAT.Command_Line.Parameter);

            when 'R' =>
               Recursive := True;

            when 'p' =>
               Prefix := To_Unbounded_String (GNAT.Command_Line.Parameter);

            when 'r' =>
               Root_Pck := To_Unbounded_String (GNAT.Command_Line.Parameter);

            when 'q' =>
               Quiet := True;

            when 'z' =>
               Compress := True;

            when 'u' =>
               Compress := False;

            when 'h' =>
               raise Syntax_Error;

            when others =>
               raise Syntax_Error;
         end case;
      end loop;

   exception
      when GNAT.Command_Line.Invalid_Switch =>
         raise Syntax_Error;
   end Parse_Command_Line;

   Buffer : String (1 .. 2_048);
   Last   : Natural;

begin
   Parse_Command_Line;

   if not Quiet then
      Text_IO.Put_Line ("AWSRes - Resource Creator v" & Version);
      Text_IO.New_Line;
   end if;

   Text_IO.Create (RT_File, Text_IO.Out_File);
   Text_IO.Create
     (R_File, Text_IO.Out_File,
      Output_Filename (To_String (Root_Pck) & ".adb"));

   Text_IO.New_Line (R_File);
   Text_IO.Put_Line (R_File, Header);
   Text_IO.New_Line (R_File);
   Text_IO.Put_Line (R_File, "pragma Style_Checks (Off);");
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

   --  Parse directories/files

   loop
      declare
         use Directories;

         S    : constant String :=
                  Utils.Dequote
                    (GNAT.Command_Line.Get_Argument (Do_Expansion => False));
         Glob : constant Boolean := Strings.Fixed.Index (S, Glob_Pat) /= 0;
         Dir  : constant Natural :=
                  Strings.Fixed.Index
                    (S, Strings.Maps.To_Set ("/\"), Going => Strings.Backward);
      begin
         exit when S'Length = 0;

         if S = "-z" then
            Compress := True;

         elsif S = "-u" then
            Compress := False;

         else
            if not Glob and then Exists (S) and then Kind (S) = Directory then
               Handle_Resource (S, "*.*");

            --  No directory specified

            elsif Dir = 0 then
               Handle_Resource (".", S);

            --  A directory specified

            else
               Handle_Resource (S (S'First .. Dir - 1), S (Dir + 1 .. S'Last));
            end if;
         end if;
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

   Text_IO.Create
     (R_File, Text_IO.Out_File,
      Output_Filename (To_String (Root_Pck) & ".ads"));

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

   Command_Line.Set_Exit_Status (Command_Line.Success);

exception
   when Syntax_Error =>
      Text_IO.Put_Line ("AWSRes - Resource Creator v" & Version);
      Text_IO.New_Line;
      Text_IO.Put_Line
        ("Usage : awsres [-hopqrRzu] file1/dir1 [-zu] [file2/dir2...]");
      Text_IO.New_Line;
      Text_IO.Put_Line
        ("        -a      : packages are named after the actual filenames");
      Text_IO.Put_Line
        ("        -h      : display help");
      Text_IO.Put_Line
        ("        -o dir  : specify the output directory");
      Text_IO.Put_Line
        ("        -p str  : prefix all resource names with the given string");
      Text_IO.Put_Line
        ("        -R      : activate recursivity");
      Text_IO.Put_Line
        ("        -r name : name of the root package (default res)");
      Text_IO.Put_Line
        ("        -z      : enable compression of following resources");
      Text_IO.Put_Line
        ("        -u      : disable compression of following resources");
      Text_IO.Put_Line
        ("        -q      : quiet mode");

      Command_Line.Set_Exit_Status (Command_Line.Failure);

   when E : Text_IO.Use_Error =>
      Text_IO.Put_Line (Exception_Message (E));

      Command_Line.Set_Exit_Status (Command_Line.Failure);
end AwsRes;
