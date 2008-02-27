------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2003-2008                          --
--                                 AdaCore                                  --
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

with Ada.Strings.Unbounded;

package Ada2WSDL.Options is

   use Ada.Strings.Unbounded;

   Initialized : Boolean := False;
   --  Set to True by Initialize, if initialization is successful

   Verbose        : Boolean := False;
   --  If this flag is set ON, Ada2WSDL generates the message about itself,
   --  including ASIS/GNAT version with which it is built

   Quiet          : Boolean := False;
   --  If this flag is set ON, Ada2WSDL does not generate a confirmation
   --  in case when the sample body has successfully been created

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

   Tree_File_Path : Unbounded_String;
   --  Path to the generated tree file, this is needed when, for example, using
   --  a project file whose object directory is not the current directory.

   Enum_To_String : Boolean := False;
   --  If True all enumeration types will be mapped to strings

   procedure Set_Default;
   --  Set default path options for the Asis compile step

end Ada2WSDL.Options;
