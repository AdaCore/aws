------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2001                          --
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

with Ada.Strings.Unbounded;

with SOAP.Parameters;

package SOAP.Message is

   use Ada.Strings.Unbounded;

   type Object is tagged private;

   function XML_Image (M : in Object) return Unbounded_String;
   --  Returns the XML image for the wrapper and parameters. This is designed
   --  to be used by Payload and Response object.

   function Name_Space   (M : in Object'Class) return String;
   --  Returns message Namespace.

   function Wrapper_Name (M : in Object'class) return String;
   --  Returns wrapper name.

   function Parameters   (M : in Object'class) return SOAP.Parameters.List;
   --  Returns the parameter.

   procedure Set_Name_Space
     (M    : in out Object'Class;
      Name : in     String);
   --  Set message's Namespace.

   procedure Set_Wrapper_Name
     (M     : in out Object'Class;
      Name  : in     String);
   --  Set message's wrapper name.

   procedure Set_Parameters
     (M     : in out Object'Class;
      P_Set : in     SOAP.Parameters.List);
   --  Set message's parameters.

private

   type Object is tagged record
      Name_Space   : Unbounded_String;
      Wrapper_Name : Unbounded_String;
      P            : SOAP.Parameters.List;
   end record;

end SOAP.Message;
