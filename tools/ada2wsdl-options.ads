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

with Ada.Strings.Unbounded;

package Ada2WSDL.Options is

   use Ada.Strings.Unbounded;

   LaL : Boolean := False;

   Verbose        : Boolean := False;
   --  If this flag is set ON, Ada2WSDL generates the message about itself,
   --  including GNAT version with which it is built.

   Quiet          : Boolean := False;
   --  If this flag is set ON, Ada2WSDL does not output information about the
   --  generated SOAP routines.

   Overwrite_WSDL : Boolean := False;
   --  Should an existing WSDL be overwritten

   SOAP_Address   : Unbounded_String := To_Unbounded_String ("http://.../");
   --  SOAP address (URL) for the Web Services server

   File_Name      : Unbounded_String;
   --  Input Ada spec filename

   WSDL_File_Name : Unbounded_String;
   --  Output WSDL document filename

   WS_Name        : Unbounded_String;
   --  Name of the Web Service, default value is the name of the Ada package

   Project_Filename : Unbounded_String;
   --  The project filename passed to the -P option

   Tree_File_Path : Unbounded_String;
   --  Path to the generated tree file, this is needed when, for example, using
   --  a project file whose object directory is not the current directory.

   Enum_To_String : Boolean := False;
   --  If True all enumeration types will be mapped to strings

   SEA            : Boolean := False;
   --  Whether to generate SOAP-Encoded array (old format)

   Document : Boolean := False;
   --  Whether to generate document style binding instead of RPC

   Literal     : Boolean := False;
   --  Whether to generate literal style parameters instead of encoded

   Timestamp   : Boolean := True;
   --  Do not generate date/time tag into the WSDL document for being able to
   --  compare them. This is an internal option only.

end Ada2WSDL.Options;
