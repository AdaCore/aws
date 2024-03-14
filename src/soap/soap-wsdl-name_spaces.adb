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

pragma Ada_2012;

with AWS.Containers.Key_Value;

package body SOAP.WSDL.Name_Spaces is

   package Name_Spaces renames AWS.Containers.Key_Value;
   NS : Name_Spaces.Map;

   --------------
   -- Contains --
   --------------

   function Contains (Key : String) return Boolean is
   begin
      return NS.Contains (Key);
   end Contains;

   ---------
   -- Get --
   ---------

   function Get
     (Prefix  : String;
      Default : Name_Space.Object := Name_Space.No_Name_Space)
      return Name_Space.Object is
   begin
      if Prefix = "" then
         return Default;
      else
         return Name_Space.Create (Prefix, Get (Prefix));
      end if;
   end Get;

   function Get (Key : String) return String is
   begin
      if NS.Contains (Key) then
         return NS (Key);
      else
         raise SOAP_Error with "unknown name-space " & Key;
      end if;
   end Get;

   ------------
   -- Is_XSD --
   ------------

   function Is_XSD (Name : String) return Boolean is
   begin
      return NS.Contains (Name)
        and then (NS (Name) = SOAP.Name_Space.XSD_URL
                  or else NS (Name) = "http://www.w3.org/2000/10/XMLSchema"
                  or else NS (Name) = "http://www.w3.org/1999/XMLSchema");
   end Is_XSD;

   function Is_XSD (NS : Name_Space.Object) return Boolean is
   begin
      return Is_XSD (Name_Space.Name (NS));
   end Is_XSD;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (Process : not null access procedure (Key, Value : String)) is
   begin
      for C in NS.Iterate loop
         Process (Name_Spaces.Key (C), Name_Spaces.Element (C));
      end loop;
   end Iterate;

   --------------
   -- Register --
   --------------

   procedure Register (Key, Value : String) is
   begin
      NS.Insert (Key, Value);
   end Register;

end SOAP.WSDL.Name_Spaces;
