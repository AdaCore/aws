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

with Ada.Exceptions;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with GNAT.Command_Line;

with SOAP.Generator;
with SOAP.WSDL.Parser;

procedure WSDL2AWS is

   use Ada;
   use Ada.Exceptions;
   use Ada.Strings.Unbounded;
   use GNAT;

   Syntax_Error : exception;

   procedure Parse_Command_Line;

   Gen : SOAP.Generator.Object;
   Def : SOAP.WSDL.Object;

   Filename : Unbounded_String;

   WSDL_Des : Boolean := False;

   ------------------------
   -- Parse_Command_Line --
   ------------------------

   procedure Parse_Command_Line is
   begin
      loop
         case Command_Line.Getopt ("q a f v rpc wsdl cvs nostub noskel") is
            when ASCII.NUL => exit;

            when 'q' =>
               SOAP.Generator.Quiet (Gen);

            when 'a' =>
               SOAP.Generator.Ada_Style (Gen);

            when 'f' =>
               SOAP.Generator.Overwrite (Gen);

            when 'r' =>
               if Command_Line.Full_Switch = "rpc" then
                  SOAP.WSDL.Parser.Accept_RPC (Gen);
               else
                  raise Syntax_Error;
               end if;

            when 'v' =>
               SOAP.WSDL.Parser.Verbose;

            when 'w' =>
               if Command_Line.Full_Switch = "wsdl" then
                  WSDL_Des := True;
               else
                  raise Syntax_Error;
               end if;

            when 'c' =>
               if Command_Line.Full_Switch = "cvs" then
                  SOAP.Generator.CVS_Tag (Gen);
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

            when others =>
               raise Program_Error;
         end case;
      end loop;

      Filename := To_Unbounded_String (Command_Line.Get_Argument);

      if WSDL_Des then
         SOAP.Generator.WSDL_File (Gen, To_String (Filename));
      end if;
   end Parse_Command_Line;

begin
   Parse_Command_Line;

   if Filename = Null_Unbounded_String then
      raise Syntax_Error;
   else
      Def := SOAP.WSDL.Load (To_String (Filename));
   end if;

   SOAP.WSDL.Parser.Parse (Gen, Def);

exception
   when Syntax_Error | Command_Line.Invalid_Switch =>
      Text_IO.New_Line;
      Text_IO.Put_Line ("wsdl2aws SOAP Generator v" & SOAP.Generator.Version);
      Text_IO.New_Line;
      Text_IO.Put_Line ("Usage: wsdl2aws [options] <file>");
      Text_IO.Put_Line ("   -q       Quiet mode");
      Text_IO.Put_Line ("   -a       Ada style identifier");
      Text_IO.Put_Line ("   -f       Force stub/skeleton generation");
      Text_IO.Put_Line ("   -rpc     Accept RPC style binding");
      Text_IO.Put_Line ("   -v       Verbose mode");
      Text_IO.Put_Line ("   -wsdl    Add WSDL file in unit comment");
      Text_IO.Put_Line ("   -cvs     Add CVS tag in unit's headers");
      Text_IO.Put_Line ("   -nostub  Do not create stub units");
      Text_IO.Put_Line ("   -noskel  Do not create skeleton units");
      Text_IO.New_Line;

   when E : others =>
      Text_IO.New_Line;
      Text_IO.Put_Line ("wsdl2aws SOAP Generator v" & SOAP.Generator.Version);
      Text_IO.New_Line;
      Text_IO.Put_Line ("Error: " & Exception_Information (E));
      Text_IO.New_Line;
end WSDL2AWS;
