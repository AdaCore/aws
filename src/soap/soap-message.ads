------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2012, AdaCore                     --
--                                                                          --
--  This library is free software;  you can redistribute it and/or modify   --
--  it under terms of the  GNU General Public License  as published by the  --
--  Free Software  Foundation;  either version 3,  or (at your  option) any --
--  later version. This library is distributed in the hope that it will be  --
--  useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                    --
--                                                                          --
--  As a special exception under Section 7 of GPL version 3, you are        --
--  granted additional permissions described in the GCC Runtime Library     --
--  Exception, version 3.1, as published by the Free Software Foundation.   --
--                                                                          --
--  You should have received a copy of the GNU General Public License and   --
--  a copy of the GCC Runtime Library Exception along with this program;    --
--  see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see   --
--  <http://www.gnu.org/licenses/>.                                         --
--                                                                          --
--  As a special exception, if other files instantiate generics from this   --
--  unit, or you link this unit with other files to produce an executable,  --
--  this  unit  does not  by itself cause  the resulting executable to be   --
--  covered by the GNU General Public License. This exception does not      --
--  however invalidate any other reasons why the executable file  might be  --
--  covered by the  GNU Public License.                                     --
------------------------------------------------------------------------------

with Ada.Strings.Unbounded;

with SOAP.Name_Space;
with SOAP.Parameters;

package SOAP.Message is

   use Ada.Strings.Unbounded;

   type Object is tagged private;

   function XML_Image (M : Object) return Unbounded_String;
   --  Returns the XML image for the wrapper and parameters. This is designed
   --  to be used by Payload and Response object.

   function Name_Space   (M : Object'Class) return SOAP.Name_Space.Object;
   --  Returns message Namespace

   function Wrapper_Name (M : Object'class) return String;
   --  Returns wrapper name

   function Parameters   (M : Object'class) return SOAP.Parameters.List;
   --  Returns the parameter

   procedure Set_Name_Space
     (M  : in out Object'Class;
      NS : SOAP.Name_Space.Object);
   --  Set message's Namespace

   procedure Set_Wrapper_Name
     (M     : in out Object'Class;
      Name  : String);
   --  Set message's wrapper name

   procedure Set_Parameters
     (M     : in out Object'Class;
      P_Set : SOAP.Parameters.List);
   --  Set message's parameters

private

   type Object is tagged record
      Name_Space   : SOAP.Name_Space.Object;
      Wrapper_Name : Unbounded_String;
      P            : SOAP.Parameters.List;
   end record;

end SOAP.Message;
