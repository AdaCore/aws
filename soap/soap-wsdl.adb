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

--  This package provides services to handle WSDL.

with Ada.Exceptions;

with Input_Sources.File;
with Sax.Readers;
with Tree_Readers;

package body SOAP.WSDL is

   use Ada;

   -----------------
   -- Is_Standard --
   -----------------

   function Is_Standard (XSD_Type : in String) return Boolean is
   begin
      if XSD_Type = "string"
        or else XSD_Type = "integer"
        or else XSD_Type = "int"
        or else XSD_Type = "float"
        or else XSD_Type = "boolean"
        or else XSD_Type = "timeInstant"
        or else XSD_Type = "dateTime"
        or else XSD_Type = "base64Binary"
      then
         return True;

      else
         return False;
      end if;
   end Is_Standard;

   ----------
   -- Load --
   ----------

   function Load (Filename : in String) return Object is
      use Input_Sources.File;
      use Tree_Readers;

      Source : File_Input;
      Reader : Tree_Reader;
   begin
      Open (Filename, Source);

      --  If True, xmlns:* attributes will be reported in Start_Element
      Set_Feature (Reader, Sax.Readers.Namespace_Prefixes_Feature, True);
      Set_Feature (Reader, Sax.Readers.Validation_Feature, False);

      Parse (Reader, Source);

      Close (Source);

      return Object (Tree_Readers.Get_Tree (Reader));
   end Load;

   ------------
   -- To_Ada --
   ------------

   function To_Ada (P : in Parameter_Type) return String is
   begin
      case P is
         when P_Integer => return "Integer";
         when P_Float   => return "Long_Float";
         when P_String  => return "String";
         when P_Boolean => return "Boolean";
         when P_Time    => return "Ada.Calendar.Time";
         when P_B64     => return "String";
      end case;
   end To_Ada;

   -------------
   -- To_Type --
   -------------

   function To_Type (XSD_Type : in String) return Parameter_Type is
      use Exceptions;
   begin
      if XSD_Type = "string" then
         return P_String;

      elsif XSD_Type = "integer" or else XSD_Type = "int" then
         return P_Integer;

      elsif XSD_Type = "float" then
         return P_Float;

      elsif XSD_Type = "boolean" then
         return P_Boolean;

      elsif XSD_Type = "timeInstant" or else XSD_Type = "dateTime" then
         return P_Time;

      elsif XSD_Type = "base64Binary" then
         return P_B64;

      else
         Raise_Exception
           (WSDL_Error'Identity,
            "(To_Type) Type " & XSD_Type & " not supported.");
      end if;
   end To_Type;

end SOAP.WSDL;
