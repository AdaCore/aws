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

--  $Id$

with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with GNAT.Command_Line;
with GNAT.Directory_Operations;
with GNAT.OS_Lib;

with AWS.Client;
with AWS.Response;
with SOAP.Generator;
with SOAP.WSDL.Parser;

procedure WSDL2AWS is

   use Ada.Exceptions;
   use Ada.Strings.Unbounded;
   use GNAT;
   use type SOAP.WSDL.Parser.Verbose_Level;

   Syntax_Error : exception;

   procedure Parse_Command_Line;
   --  Parse command line arguments

   function Get_Document (URL : in Unbounded_String) return Unbounded_String;
   --  Get WSDL document pointed to by URL, returns the name of the local
   --  filename.

   Gen : SOAP.Generator.Object;
   Def : SOAP.WSDL.Object;

   Filename     : Unbounded_String;
   Out_Filename : Unbounded_String;
   Proxy        : Unbounded_String;
   Pu, Pp       : Unbounded_String;
   Force        : Boolean := False;

   WSDL_Des     : Boolean := False;

   Verbose      : SOAP.WSDL.Parser.Verbose_Level := 0;

   Gen_Cb       : Boolean := False;
   Types        : Boolean := False;

   ------------------
   -- Get_Document --
   ------------------

   function Get_Document (URL : in Unbounded_String) return Unbounded_String is
      use Ada;

      L_URL    : constant String := To_String (URL);
      Filename : Unbounded_String
        := To_Unbounded_String (Directory_Operations.File_Name (L_URL));
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

      declare
         WSDL : constant String := AWS.Response.Message_Body (Response);
         Last : Natural;
      begin
         --  Look for end of WSDL document, and cut it after the closing
         --  definition tag. This is to work-around a problem with some
         --  servers returning a script tag at the end of the file.
         --
         --  This is of course a bug in those servers but we don't want to
         --  crash here.

         Last := Strings.Fixed.Index (WSDL, "</definitions>");

         if Last = 0 then
            Exceptions.Raise_Exception
              (Constraint_Error'Identity,
               "This does not look like a WSDL document");
         else
            Text_IO.Create (File, Text_IO.Out_File, To_String (Filename));

            Text_IO.Put_Line (File, WSDL (WSDL'First .. Last + 13));

            Text_IO.Close (File);
         end if;
      end;

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

      SOAP.Generator.Options (Gen, To_String (All_Options));

      --  Now parse arguments

      loop
         case Command_Line.Getopt
           ("q a f v s o: proxy: pu: pp: doc wsdl cvs nostub noskel cb types:")
         is
            when ASCII.NUL => exit;

            when 'q' =>
               SOAP.Generator.Quiet (Gen);

            when 'a' =>
               SOAP.Generator.Ada_Style (Gen);

            when 'f' =>
               Force := True;
               SOAP.Generator.Overwrite (Gen);

            when 'o' =>
               Out_Filename
                 := To_Unbounded_String (GNAT.Command_Line.Parameter);

            when 'd' =>
               if Command_Line.Full_Switch = "doc" then
                  SOAP.WSDL.Parser.Accept_Document (Gen);
               else
                  raise Syntax_Error;
               end if;

            when 's' =>
               SOAP.WSDL.Parser.Continue_On_Error;

            when 'v' =>
               Verbose := Verbose + 1;
               SOAP.WSDL.Parser.Verbose (Verbose);

            when 'w' =>
               if Command_Line.Full_Switch = "wsdl" then
                  WSDL_Des := True;
               else
                  raise Syntax_Error;
               end if;

            when 'c' =>
               if Command_Line.Full_Switch = "cvs" then
                  SOAP.Generator.CVS_Tag (Gen);

               elsif Command_Line.Full_Switch = "cb" then
                  SOAP.Generator.Gen_Cb (Gen);
                  Gen_Cb := True;

               else
                  raise Syntax_Error;
               end if;

            when 'n' =>
               if Command_Line.Full_Switch = "nostub" then
                  SOAP.Generator.No_Stub (Gen);

               elsif Command_Line.Full_Switch = "noskel" then
                  SOAP.Generator.No_Skel (Gen);

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

               else
                  raise Syntax_Error;
               end if;

            when 't' =>
               if Command_Line.Full_Switch = "types" then
                  SOAP.Generator.Types_From (Gen, GNAT.Command_Line.Parameter);
                  Types := True;

               else
                  raise Syntax_Error;
               end if;

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
      SOAP.Generator.Set_Proxy
        (Gen, To_String (Proxy), To_String (Pu), To_String (Pp));

   end if;

   if Gen_Cb and then not Types then
      Raise_Exception
        (Constraint_Error'Identity,
         "Callback can't be generated if no Ada spec specified");
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
      SOAP.Generator.WSDL_File (Gen, To_String (Filename));
   end if;

   --  Load WSDL document, return it

   Def := SOAP.WSDL.Load (To_String (Filename));

   --  Parse the document and generate the code

   SOAP.WSDL.Parser.Parse (Gen, Def);

exception
   when Syntax_Error | Command_Line.Invalid_Switch =>
      New_Line;
      Put_Line ("wsdl2aws SOAP Generator v" & SOAP.Generator.Version);
      New_Line;
      Put_Line ("Usage: wsdl2aws [options] <file|URL>");
      Put_Line ("   -q           Quiet mode");
      Put_Line ("   -a           Ada style identifier");
      Put_Line ("   -f           Force files creation stub/skeleton/WSDL");
      Put_Line ("   -s           Skip non supported SOAP routines");
      Put_Line ("   -o           Output filename for Web Document (URL mode)");
      Put_Line
        ("   -doc         Accept Document style binding (parsed as RPC)");
      Put_Line ("   -v           Verbose mode");
      Put_Line ("   -v -v        Very verbose mode");
      Put_Line ("   -wsdl        Add WSDL file in unit comment");
      Put_Line ("   -cvs         Add CVS tag in unit's headers");
      Put_Line ("   -nostub      Do not create stub units");
      Put_Line ("   -noskel      Do not create skeleton units");
      Put_Line ("   -cb          Generate SOAP callback routine");
      Put_Line ("   -types spec  Use types from Ada spec");
      Put_Line ("   -proxy addr  Name or IP of the proxy");
      Put_Line ("   -pu name     The proxy user name");
      Put_Line ("   -pp pwd      The proxy password");
      New_Line;

   when Name_Error =>
      New_Line;
      Put_Line ("wsdl2aws SOAP Generator v" & SOAP.Generator.Version);
      New_Line;
      Put_Line ("WSDL file not found : " & To_String (Filename));
      New_Line;

   when E : others =>
      New_Line;
      Put_Line ("wsdl2aws SOAP Generator v" & SOAP.Generator.Version);
      New_Line;
      Put_Line ("Error: " & Exception_Information (E));
      New_Line;
end WSDL2AWS;
