------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2003-2017, AdaCore                     --
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
with Ada.Exceptions;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with GNAT.Command_Line;
with GNAT.Directory_Operations;
with GNAT.OS_Lib;
with GNAT.String_Split;

with AWS.Client;
with AWS.Response;
with SOAP.Name_Space;
with SOAP.WSDL;

with WSDL2AWS.Generator;
with WSDL2AWS.WSDL.Parser;

procedure WSDL2AWS.Main is

   use Ada.Exceptions;
   use Ada.Strings.Unbounded;
   use GNAT;

   use AWS;
   use type WSDL2AWS.WSDL.Parser.Verbose_Level;

   Syntax_Error : exception;

   procedure Parse_Command_Line;
   --  Parse command line arguments

   function Get_Document (URL : Unbounded_String) return Unbounded_String;
   --  Get WSDL document pointed to by URL, returns the name of the local
   --  filename.

   Gen : Generator.Object;
   Def : SOAP.WSDL.Object;

   Filename     : Unbounded_String;
   Out_Filename : Unbounded_String;
   Proxy        : Unbounded_String;
   Pu, Pp       : Unbounded_String;
   Force        : Boolean := False;

   WSDL_Des     : Boolean := False;

   Verbose      : WSDL2AWS.WSDL.Parser.Verbose_Level := 0;

   Gen_CB       : Boolean := False;
   Types        : Boolean := False;
   Spec         : Boolean := False;
   Main         : Boolean := False;

   ------------------
   -- Get_Document --
   ------------------

   function Get_Document (URL : Unbounded_String) return Unbounded_String is
      use Ada;

      L_URL    : constant String := To_String (URL);
      Filename : Unbounded_String :=
                   To_Unbounded_String
                     (Directory_Operations.File_Name (L_URL));
      Response : AWS.Response.Data;
      File     : Text_IO.File_Type;
   begin
      if Out_Filename /= Null_Unbounded_String then
         Filename := Out_Filename;
      end if;

      --  Check if filename exists and can be overwritten

      if OS_Lib.Is_Regular_File (To_String (Filename)) and then not Force then
         Exceptions.Raise_Exception
           (Constraint_Error'Identity,
            "WSDL file " & To_String (Filename)
              & " already present, use -f option to overwrite");
      end if;

      --  Get document

      Response := AWS.Client.Get
        (L_URL,
         Proxy      => To_String (Proxy),
         Proxy_User => To_String (Pu),
         Proxy_Pwd  => To_String (Pp));

      --  Write WSDL

      Text_IO.Create (File, Text_IO.Out_File, To_String (Filename));
      Text_IO.Put_Line (File, AWS.Response.Message_Body (Response));
      Text_IO.Close (File);

      --  Returns the filename

      return Filename;

   exception
      when Text_IO.Name_Error =>
         Text_IO.Put_Line ("Can't create file " & To_String (Filename));
         raise;
   end Get_Document;

   ------------------------
   -- Parse_Command_Line --
   ------------------------

   procedure Parse_Command_Line is
      All_Options : Unbounded_String;
   begin
      --  Get all options

      for K in 1 .. Ada.Command_Line.Argument_Count loop
         Append (All_Options, Ada.Command_Line.Argument (K));

         if K /= Ada.Command_Line.Argument_Count then
            Append (All_Options, " ");
         end if;
      end loop;

      Generator.Options (Gen, To_String (All_Options));

      --  Now parse arguments

      loop
         case Command_Line.Getopt
           ("d q a e: f v s o: p: proxy: pu: pp: doc wsdl cvs nostub noskel "
            & "x: debug cb traces types: spec: main: n: timeouts:")
         is
            when ASCII.NUL => exit;

            when 'q' =>
               Generator.Quiet (Gen);

            when 'a' =>
               Generator.Ada_Style (Gen);

            when 'e' =>
               Generator.Endpoint (Gen, GNAT.Command_Line.Parameter);

            when 'f' =>
               Force := True;
               Generator.Overwrite (Gen);

            when 'o' =>
               Out_Filename
                 := To_Unbounded_String (GNAT.Command_Line.Parameter);

            when 'd' =>
               if Command_Line.Full_Switch = "doc" then
                  WSDL2AWS.WSDL.Parser.Accept_Document (Gen);
               elsif Command_Line.Full_Switch = "d" then
                  Generator.Disable_Time_Stamp (Gen);
               elsif Command_Line.Full_Switch = "debug" then
                  Generator.Debug (Gen);
               else
                  raise Syntax_Error;
               end if;

            when 's' =>
               if Command_Line.Full_Switch = "spec" then
                  Generator.Specs_From
                    (Gen, GNAT.Command_Line.Parameter);
                  Spec := True;

               elsif Command_Line.Full_Switch = "s" then
                  WSDL2AWS.WSDL.Parser.Continue_On_Error;

               else
                  raise Syntax_Error;
               end if;

            when 'v' =>
               Verbose := Verbose + 1;
               WSDL2AWS.WSDL.Parser.Verbose (Verbose);

            when 'w' =>
               if Command_Line.Full_Switch = "wsdl" then
                  WSDL_Des := True;
               else
                  raise Syntax_Error;
               end if;

            when 'c' =>
               if Command_Line.Full_Switch = "cvs" then
                  Generator.CVS_Tag (Gen);

               elsif Command_Line.Full_Switch = "cb" then
                  Generator.Gen_CB (Gen);
                  Gen_CB := True;

               else
                  raise Syntax_Error;
               end if;

            when 'm' =>
               if Command_Line.Full_Switch = "main" then
                  Generator.Main (Gen, GNAT.Command_Line.Parameter);
                  Main := True;

               else
                  raise Syntax_Error;
               end if;

            when 'n' =>
               if Command_Line.Full_Switch = "nostub" then
                  Generator.No_Stub (Gen);

               elsif Command_Line.Full_Switch = "noskel" then
                  Generator.No_Skel (Gen);

               elsif Command_Line.Full_Switch = "n" then
                  SOAP.Name_Space.Set_AWS_NS
                    (Value => GNAT.Command_Line.Parameter);

               else
                  raise Syntax_Error;
               end if;

            when 'p' =>
               if Command_Line.Full_Switch = "proxy" then
                  Proxy := To_Unbounded_String (GNAT.Command_Line.Parameter);

               elsif Command_Line.Full_Switch = "pu" then
                  Pu := To_Unbounded_String (GNAT.Command_Line.Parameter);

               elsif Command_Line.Full_Switch = "pp" then
                  Pp := To_Unbounded_String (GNAT.Command_Line.Parameter);

               elsif Command_Line.Full_Switch = "p" then
                  Generator.Set_Prefix
                    (Gen, GNAT.Command_Line.Parameter);

               else
                  raise Syntax_Error;
               end if;

            when 't' =>
               if Command_Line.Full_Switch = "types" then
                  Generator.Types_From
                    (Gen, GNAT.Command_Line.Parameter);
                  Types := True;

               elsif Command_Line.Full_Switch = "timeouts" then
                  declare
                     use String_Split;
                     Timeouts : constant String := GNAT.Command_Line.Parameter;
                     Slices   : String_Split.Slice_Set;
                  begin
                     String_Split.Create (Slices, Timeouts, ",");

                     case String_Split.Slice_Count (Slices) is
                        when 1 =>
                           Generator.Set_Timeouts
                             (Gen,
                              Client.Timeouts
                                (Connect => Duration'Value (Slice (Slices, 1)),
                                 Send    => Duration'Value (Slice (Slices, 1)),
                                 Receive => Duration'Value
                                   (Slice (Slices, 1))));
                        when 3 =>
                           Generator.Set_Timeouts
                             (Gen,
                              Client.Timeouts
                                (Connect => Duration'Value (Slice (Slices, 1)),
                                 Send    => Duration'Value (Slice (Slices, 2)),
                                 Receive => Duration'Value
                                   (Slice (Slices, 3))));
                        when 4 =>
                           Generator.Set_Timeouts
                             (Gen,
                              Client.Timeouts
                                (Connect  => Duration'Value
                                   (Slice (Slices, 1)),
                                 Send     => Duration'Value
                                   (Slice (Slices, 2)),
                                 Receive  => Duration'Value
                                   (Slice (Slices, 3)),
                                 Response => Duration'Value
                                   (Slice (Slices, 4))));
                        when others =>
                           raise Syntax_Error
                             with "wrong number of arguments for timeouts";
                     end case;
                  end;

               elsif Command_Line.Full_Switch = "traces" then
                  Generator.Traces (Gen);

               else
                  raise Syntax_Error;
               end if;

            when 'x' =>
               Generator.Exclude (Gen, GNAT.Command_Line.Parameter);

            when others =>
               raise Program_Error;
         end case;
      end loop;

      Filename := To_Unbounded_String (Command_Line.Get_Argument);
   end Parse_Command_Line;

   use Ada.Text_IO;

begin
   Parse_Command_Line;

   --  Checks parameters

   if Proxy = Null_Unbounded_String then

      if Pu /= Null_Unbounded_String or else Pp /= Null_Unbounded_String then
         Raise_Exception
           (Constraint_Error'Identity,
            "Proxy user/password specified, but no proxy set.");
      end if;

   elsif Proxy /= Null_Unbounded_String then
      Generator.Set_Proxy
        (Gen, To_String (Proxy), To_String (Pu), To_String (Pp));

   end if;

   if Gen_CB and then not Types and then not Spec then
      Raise_Exception
        (Constraint_Error'Identity,
         "Callback can't be generated if no Ada spec specified");
   end if;

   if Main and then not Gen_CB then
      Raise_Exception
        (Constraint_Error'Identity,
         "Server main can't be generated if callback not generated.");
   end if;

   if Filename = Null_Unbounded_String then
      --  No file specified
      raise Syntax_Error;

   elsif Length (Filename) > 7 and then Slice (Filename, 1, 7) = "http://" then
      --  This is an URL, retrieve the WSDL document
      Filename := Get_Document (Filename);

   elsif Out_Filename /= Null_Unbounded_String then
      --  Not an URL, file specified, this option is not valid
      Raise_Exception
        (Constraint_Error'Identity,
         "Option -o must be used when parsing a Web document (URL).");
   end if;

   --  Set WSDL file

   if WSDL_Des then
      Generator.WSDL_File (Gen, To_String (Filename));
   end if;

   --  Load WSDL document, return it

   Def := SOAP.WSDL.Load (To_String (Filename));

   --  Parse the document and generate the code

   WSDL2AWS.WSDL.Parser.Parse (Gen, Def);

   Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Success);

exception
   when Syntax_Error | Command_Line.Invalid_Switch =>
      New_Line;
      Put_Line ("wsdl2aws SOAP Generator v" & Version);
      New_Line;
      Put_Line ("Usage: wsdl2aws [options] <file|URL>");
      Put_Line ("   -q           Quiet mode");
      Put_Line ("   -d           no date/time stamp in generated sources");
      Put_Line ("   -debug       Generate debug code");
      Put_Line ("   -a           Ada style identifiers");
      Put_Line ("   -f           Force files creation stub/skeleton/types");
      Put_Line ("   -e URL       Endpoint to use");
      Put_Line ("   -s           Skip non supported SOAP routines");
      Put_Line ("   -o name      Output filename for Web Document (URL mode)");
      Put_Line ("   -p name      Name prefix for all SOAPActions");
      Put_Line
        ("   -doc         Document style binding handled as RPC");
      Put_Line ("   -v           Verbose mode");
      Put_Line ("   -v -v        Very verbose mode");
      Put_Line ("   -wsdl        Add WSDL file in unit comment");
      Put_Line ("   -cvs         Add CVS tag in unit's headers");
      Put_Line ("   -nostub      Do not create stub units");
      Put_Line ("   -noskel      Do not create skeleton units");
      Put_Line ("   -cb          Generate SOAP server callback routine");
      Put_Line ("   -traces      Generate SOAP client callbacks");
      Put_Line ("   -x operation Exclude operation from code generation");
      Put_Line ("   -spec  spec  Use procs/types from Ada spec");
      Put_Line ("   -types spec  Use types from Ada spec");
      Put_Line ("   -main file   Generate SOAP main server's procedure");
      Put_Line ("   -n name      Schema root name (default soapaws)");
      Put_Line ("   -proxy addr  Name or IP of the proxy");
      Put_Line ("   -pu name     The proxy user name");
      Put_Line ("   -pp pwd      The proxy password");
      Put_Line ("   -timeouts    The connect/read/write/response timeouts");
      New_Line;
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);

   when W : SOAP.WSDL.WSDL_Error =>
      New_Line;
      Put_Line ("Error: " & Exception_Message (W));
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);

   when E : others =>
      New_Line;
      Put_Line ("wsdl2aws SOAP Generator v" & Version);
      New_Line;
      Put_Line ("Error: " & Exception_Information (E));
      New_Line;
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
end WSDL2AWS.Main;
