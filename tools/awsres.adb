------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2002-2009, AdaCore                     --
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

--  This programs create a parent package Root_Pck and one children for any
--  files passed as argument.

with Ada.Calendar;
with Ada.Command_Line;
with Ada.Directories;
with Ada.Integer_Text_IO;
with Ada.Streams;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;

with GNAT.Command_Line;
with GNAT.Calendar.Time_IO;

with AWS.Resources.Streams.Disk;
with AWS.Resources.Streams.ZLib;
with AWS.Utils;

with ZLib;

procedure AwsRes is

   use Ada;
   use Ada.Strings.Unbounded;
   use AWS;

   Syntax_Error : exception;

   Version  : constant String := "1.2";

   Root_Pck : Unbounded_String := To_Unbounded_String ("res");
   Quiet    : Boolean := False;

   RT_File  : Text_IO.File_Type;
   --  Root temp file

   R_File   : Text_IO.File_Type;
   --  Root spec/body file

   Compress : Boolean := False;
   --  By default resources are not compressed

   procedure Create (Filename : String);
   --  Create resource package for Filename

   function Package_Name (Filename : String) return String;
   --  Returns package name for Filename

   procedure Parse_Command_Line;
   --  Parse command line

   function Header return String;
   --  Returns file header (AWSRes version, date, time)

   ------------
   -- Create --
   ------------

   procedure Create (Filename : String) is
      use Streams;

      package RS renames AWS.Resources.Streams;

      Max_Data  : constant := 14;
      --  Maximum number of data in a single line

      Unit_Name : constant String := Package_Name (Filename);
      Pck_Name  : constant String := To_String (Root_Pck) & '-'
        & Directories.Simple_Name (Unit_Name) & ".ads";

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

      procedure Free is new Ada.Unchecked_Deallocation
                              (RS.Stream_Type'Class, RS.Stream_Access);
   begin
      if not Quiet then
         Text_IO.Put ("creating " & Filename);
      end if;

      File_Time := Utils.File_Time_Stamp (Filename);

      Text_IO.Create (O_File, Text_IO.Out_File, Pck_Name);

      RS.Disk.Open (RS.Disk.Stream_Type (I_File.all), Filename);

      if Compress then
         I_File := RS.ZLib.Deflate_Create (I_File, Header => ZLib.GZip);
      end if;

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

      Text_IO.Put_Line (O_File, ");");

      --  Output end of package

      Text_IO.New_Line (O_File);
      Text_IO.Put_Line
        (O_File, "end " & To_String (Root_Pck) & '.' & Unit_Name & ';');

      RS.Close (I_File.all);
      Free     (I_File);

      Text_IO.Close (O_File);

      if not Quiet then
         Text_IO.New_Line;
      end if;

      --  Register package into root package body

      Text_IO.Put_Line (RT_File, "         Register");

      if Compress then
         Text_IO.Put_Line (RT_File, "            ("""
                           & Directories.Simple_Name (Filename) & ".gz"",");
      else
         Text_IO.Put_Line (RT_File, "            ("""
                           & Directories.Simple_Name (Filename) & """,");
      end if;

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

   function Package_Name (Filename : String) return String is
      From : constant String := ".";
      To   : constant String := "_";

      Map  : constant Strings.Maps.Character_Mapping
        := Strings.Maps.To_Mapping (From, To);
   begin
      return Strings.Fixed.Translate
        (Directories.Simple_Name (Filename), Map);
   end Package_Name;

   ------------------------
   -- Parse_Command_Line --
   ------------------------

   procedure Parse_Command_Line is
   begin
      GNAT.Command_Line.Initialize_Option_Scan
        (Stop_At_First_Non_Switch => True);

      loop
         case GNAT.Command_Line.Getopt ("r: h q z u") is
            when ASCII.NUL =>
               exit;

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

         if S = "-z" then
            Compress := True;
         elsif S = "-u" then
            Compress := False;
         else
            Create (S);
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

   Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Success);

exception
   when Syntax_Error =>
      Text_IO.Put_Line ("AWSRes - Resource Creator v" & Version);
      Text_IO.New_Line;
      Text_IO.Put_Line ("Usage : awsres [-hrqzu] file1 [-zu] [file2...]");
      Text_IO.New_Line;
      Text_IO.Put_Line
        ("        -h      : display help");
      Text_IO.Put_Line
        ("        -r name : name of the root package (default res)");
      Text_IO.Put_Line
        ("        -z      : enable compression of following resources");
      Text_IO.Put_Line
        ("        -u      : disable compression of following resources");
      Text_IO.Put_Line
        ("        -q      : quiet mode");

   Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
end AwsRes;
