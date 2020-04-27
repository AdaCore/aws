------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2003-2020, AdaCore                     --
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

with Ada.Command_Line;
with Ada.Directories;
with Ada.Exceptions;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with GNAT.Command_Line;

with AWS.Utils;
with SOAP.Name_Space;

with Ada2WSDL.Generator;
with Ada2WSDL.Options;
with Ada2WSDL.Parser;

procedure Ada2WSDL.Main is

   use Ada;
   use Ada.Exceptions;
   use Ada.Strings.Unbounded;

   procedure Usage;
   --  Display usage string

   procedure Parse_Command_Line;
   --  Parse command line and set options

   ------------------------
   -- Parse_Command_Line --
   ------------------------

   procedure Parse_Command_Line is
   begin
      loop
         case GNAT.Command_Line.Getopt
           ("l f q v a: o: s: t: I: P: n: noenum d doc lit sea")
         is
            when ASCII.NUL =>
               exit;

            when 'f' =>
               Options.Overwrite_WSDL := True;

            when 'o' =>
               Options.WSDL_File_Name :=
                 To_Unbounded_String (GNAT.Command_Line.Parameter);

            when 't' =>
               Options.Tree_File_Path :=
                 To_Unbounded_String
                   (AWS.Utils.Normalized_Directory
                        (GNAT.Command_Line.Parameter));

            when 'a' =>
               Options.SOAP_Address :=
                 To_Unbounded_String (GNAT.Command_Line.Parameter);

            when 's' =>
               if GNAT.Command_Line.Full_Switch = "sea" then
                  Options.SEA := True;
               else
                  Options.WS_Name :=
                    To_Unbounded_String (GNAT.Command_Line.Parameter);
               end if;

            when 'q' =>
               Options.Quiet := True;

            when 'n' =>
               if GNAT.Command_Line.Full_Switch = "noenum" then
                  Options.Enum_To_String := True;

               elsif GNAT.Command_Line.Full_Switch = "n" then
                  SOAP.Name_Space.Set_AWS_NS
                    (Value => GNAT.Command_Line.Parameter);

               else
                  Usage;
                  raise Parameter_Error;
               end if;

            when 'v' =>
               Options.Verbose := True;
               Text_IO.New_Line;
               Text_IO.Put_Line ("Ada2WSDL v" & Version);

            when 'P' =>
               Options.Project_Filename :=
                 To_Unbounded_String (GNAT.Command_Line.Parameter);

            when 'd' =>
               if GNAT.Command_Line.Full_Switch = "doc" then
                  Options.Document := True;

               elsif GNAT.Command_Line.Full_Switch = "d" then
                  Options.Timestamp := False;

               else
                  Usage;
                  raise Parameter_Error;
               end if;

            when 'l' =>
               if GNAT.Command_Line.Full_Switch = "lit" then
                  Options.Literal := True;

               elsif GNAT.Command_Line.Full_Switch = "l" then
                  Options.LaL := True;

               else
                  Usage;
                  raise Parameter_Error;
               end if;

            when others =>
               Usage;
               raise Parameter_Error;
         end case;
      end loop;

      Options.File_Name :=
        To_Unbounded_String (GNAT.Command_Line.Get_Argument);

      --  If there is no argument file name or no destination directory,
      --  we will get empty strings here

      if Options.File_Name = Null_Unbounded_String then
         Text_IO.Put_Line
           (Text_IO.Standard_Error, "Ada2WSDL: file name missing");
         Usage;
         raise Parameter_Error;
      end if;

      if Options.WSDL_File_Name = Null_Unbounded_String then
         Options.WSDL_File_Name :=
           To_Unbounded_String
             (Directories.Base_Name (To_String (Options.File_Name)));
         Append (Options.WSDL_File_Name, ".wsdl");
      end if;

      if Options.Document and then not Options.Literal then
         Text_IO.Put_Line
           (Text_IO.Standard_Error,
            "Ada2WSDL:  document/encoded not supported");
         raise Parameter_Error;
      end if;

      if GNAT.Command_Line.Get_Argument /= "" then
         Text_IO.Put_Line
           (Text_IO.Standard_Error, "Ada2WSDL: only one file name allowed");
         Usage;
         raise Parameter_Error;
      end if;

   exception
      when GNAT.Command_Line.Invalid_Switch =>
         Text_IO.Put_Line
           (Text_IO.Standard_Error,
            "Ada2WSDL: invalid switch : " & GNAT.Command_Line.Full_Switch);
         Usage;

         raise Parameter_Error;

      when GNAT.Command_Line.Invalid_Parameter =>
         Text_IO.Put_Line
           (Text_IO.Standard_Error,
            "Ada2WSDL: parameter missed for : "
              & GNAT.Command_Line.Full_Switch);
         Usage;

         raise Parameter_Error;
   end Parse_Command_Line;

   -----------
   -- Usage --
   -----------

   procedure Usage is
      use Text_IO;

      Current_Output : constant File_Access := Text_IO.Current_Output;
   begin
      Set_Output (Standard_Error);

      New_Line;
      Put_Line ("Usage: ada2wsdl [opts] filename");
      New_Line;
      Put_Line ("ada2wsdl options:");
      New_Line;
      Put_Line ("  -f       Replace an existing WSDL document");
      Put_Line ("  -q       Quiet mode");
      Put_Line ("  -v       Verbose mode - output the version");
      Put_Line ("  -doc     Generate document style WSDL binding");
      Put_Line ("  -lit     Generate literal data style WSDL encoding");
      Put_Line
        ("  -P proj  A project file to use for building the spec");
      Put_Line ("  -o file  WSDL file, <filename>.wsdl by default");
      Put_Line ("  -t path  Path to tree file directory");
      Put_Line ("  -a url   Web Service server address (URL)");
      Put_Line ("  -s name  Web Service name (default package name)");
      Put_Line ("  -n name  Schema root name (default soapaws)");
      Put_Line ("  -noenum  Map Ada enumeration to xsd:string");
      Put_Line ("  -sea     Generate old style SOAP Encoded array");
      Put_Line ("  -d       no date/time stamp in WSDL");

      Set_Output (Current_Output.all);
   end Usage;

begin
   Parse_Command_Line;

   declare
      Filename : constant String := To_String (Options.WSDL_File_Name);
   begin
      if AWS.Utils.Is_Regular_File (Filename)
        and then not Options.Overwrite_WSDL
      then
         Text_IO.Put_Line
           (Text_IO.Standard_Error,
            Filename & " already exists, use -f option to replace.");
         Usage;
         raise Parameter_Error;
      end if;

      Parser.Start;

      Generator.Write (Filename);
   end;

exception

   when Fatal_Error | Parameter_Error =>
      --  Everything has already been reported
      Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);

   when E : Spec_Error =>
      Text_IO.New_Line;
      Text_IO.Put_Line ("ada2wsdl: " & Exception_Message (E));
      Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);

   when E : others =>
      Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);

      declare
         Current_Output : constant Text_IO.File_Access
           := Text_IO.Current_Output;
      begin
         Text_IO.Set_Output (Text_IO.Standard_Error);
         Text_IO.New_Line;

         Text_IO.Put_Line ("Unexpected bug in Ada2WSDL v" & Version);
         Text_IO.New_Line;
         Text_IO.Put (Exception_Name (E));
         Text_IO.Put (" was raised: ");

         if Exception_Message (E)'Length = 0 then
            Text_IO.Put_Line ("(no exception message)");
         else
            Text_IO.Put_Line (Exception_Message (E));
         end if;

         Text_IO.Put_Line ("Please report.");
         Text_IO.Set_Output (Current_Output.all);
      end;
end Ada2WSDL.Main;
