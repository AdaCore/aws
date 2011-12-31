------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2004-2012, AdaCore                     --
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

package SOAP.Name_Space is

   type Object is private;

   Default_Prefix : constant String := "xmlns";

   AWS : constant Object;

   No_Name_Space : constant Object;

   function Create
     (Name, Value : String;
      Prefix      : String := Default_Prefix) return Object;
   --  Returns a name space

   procedure Set
     (O           : in out Object;
      Name, Value : String;
      Prefix      : String := Default_Prefix);
   --  Set value for the name space object

   function Is_Defined (O : Object) return Boolean;
   --  Returns True if the name space is defined

   function Prefix (O : Object) return String;
   --  Returns the name space prefix

   function Name (O : Object) return String;
   --  Returns the name space name

   function Value (O : Object) return String;
   --  Returns the name space value

   function Image (O : Object) return String;
   --  Return the name space string representation prefix:name="value"

private

   use Ada.Strings.Unbounded;

   type Object is record
      Prefix : Unbounded_String;
      Name   : Unbounded_String;
      Value  : Unbounded_String;
   end record;

   No_Name_Space : constant Object :=
                     (Null_Unbounded_String,
                      Null_Unbounded_String,
                      Null_Unbounded_String);

   AWS : constant Object :=
           (To_Unbounded_String (Default_Prefix),
            To_Unbounded_String ("awsns"),
            To_Unbounded_String ("http://soapaws/"));

end SOAP.Name_Space;
