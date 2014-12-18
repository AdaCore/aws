------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2004-2014, AdaCore                     --
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

package body SOAP.Name_Space is

   Root_AWS_NS : Unbounded_String := To_Unbounded_String ("soapaws");

   ---------
   -- AWS --
   ---------

   function AWS return Object is
   begin
      return Create
        (Name  => "awsns",
         Value => "http://" & To_String (Root_AWS_NS) & "/");
   end AWS;

   ------------
   -- Create --
   ------------

   function Create
     (Name, Value : String;
      Prefix      : String := Default_Prefix) return Object is
   begin
      return
        (Prefix => To_Unbounded_String (Prefix),
         Name   => To_Unbounded_String (Name),
         Value  => To_Unbounded_String (Value));
   end Create;

   -----------
   -- Image --
   -----------

   function Image (O : Object) return String is
   begin
      return Prefix (O) & ':' & Name (O) & "=""" & Value (O) & '"';
   end Image;

   -----------------------
   -- Is_Default_AWS_NS --
   -----------------------

   function Is_Default_AWS_NS return Boolean is
   begin
      return Root_AWS_NS = "soapaws";
   end Is_Default_AWS_NS;

   ----------------
   -- Is_Defined --
   ----------------

   function Is_Defined (O : Object) return Boolean is
   begin
      return O.Name /= Null_Unbounded_String;
   end Is_Defined;

   ----------
   -- Name --
   ----------

   function Name (O : Object) return String is
   begin
      return To_String (O.Name);
   end Name;

   ------------
   -- Prefix --
   ------------

   function Prefix (O : Object) return String is
   begin
      return To_String (O.Prefix);
   end Prefix;

   ---------
   -- Set --
   ---------

   procedure Set
     (O           : in out Object;
      Name, Value : String;
      Prefix      : String := Default_Prefix) is
   begin
      O := Create (Name, Value, Prefix);
   end Set;

   ----------------
   -- Set_AWS_NS --
   ----------------

   procedure Set_AWS_NS (Value : String) is
   begin
      Root_AWS_NS := To_Unbounded_String (Value);
   end Set_AWS_NS;

   -----------
   -- Value --
   -----------

   function Value (O : Object) return String is
   begin
      return To_String (O.Value);
   end Value;

end SOAP.Name_Space;
