------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2015-2017, AdaCore                     --
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

with SOAP.Name_Space;

package SOAP.WSDL.Name_Spaces is

   procedure Register (Key, Value : String);
   --  Register a new name-space. The Key is either the name or the URL. The
   --  value is the corresponding other part.

   function Contains (Key : String) return Boolean;
   --  Returns true if the Key is a known name-space

   function Get
     (Prefix  : String;
      Default : Name_Space.Object := Name_Space.No_Name_Space)
      return Name_Space.Object;
   --  Get the name-space for Prefix or default if Prefix is empty (or not
   --  found).

   function Get (Key : String) return String;
   --  Get the name-space corresponding value for Key

   function Is_XSD (Name : String) return Boolean;
   --  Returns true if the Name corresponds to the XSD URL

   function Is_XSD (NS : Name_Space.Object) return Boolean;
   --  Returns true if NS corresponds to the XSD URL

   procedure Iterate
     (Process : not null access procedure (Key, Value : String));
   --  Iterate over all name-space and call process

end SOAP.WSDL.Name_Spaces;
