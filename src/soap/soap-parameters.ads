------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2019, AdaCore                     --
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

with Ada.Calendar;
with Ada.Strings.Unbounded;

with SOAP.Types;

package SOAP.Parameters is

   use Ada.Strings.Unbounded;

   Data_Error : exception renames Types.Data_Error;

   Max_Parameters : constant := 50;
   --  This is the maximum number of parameters supported by this
   --  implementation.

   type List is private;

   function Argument_Count (P : List) return Natural with
     Post => Argument_Count'Result <= Max_Parameters;
   --  Returns the number of parameters in P

   function Argument (P : List; Name : String) return Types.Object'Class;
   --  Returns parameters named Name in P. Raises Types.Data_Error if not
   --  found.

   function Argument (P : List; N : Positive) return Types.Object'Class;
   --  Returns Nth parameters in P. Raises Types.Data_Error if not found

   function Exist (P : List; Name : String) return Boolean;
   --  Returns True if parameter named Name exist in P and False otherwise

   function Get (P : List; Name : String) return Types.Long with Inline;
   --  Returns parameter named Name in P as a Long value. Raises
   --  Types.Data_Error if this parameter does not exist or is not a Long.

   function Get (P : List; Name : String) return Integer with Inline;
   --  Returns parameter named Name in P as an Integer value. Raises
   --  Types.Data_Error if this parameter does not exist or is not an Integer.

   function Get (P : List; Name : String) return Types.Short with Inline;
   --  Returns parameter named Name in P as a Short value. Raises
   --  Types.Data_Error if this parameter does not exist or is not an Short.

   function Get (P : List; Name : String) return Types.Byte with Inline;
   --  Returns parameter named Name in P as a Byte value. Raises
   --  Types.Data_Error if this parameter does not exist or is not a Byte.

   function Get (P : List; Name : String) return Float with Inline;
   --  Returns parameter named Name in P as a Float value. Raises
   --  Types.Data_Error if this parameter does not exist or is not a Float.

   function Get (P : List; Name : String) return Long_Float with Inline;
   --  Returns parameter named Name in P as a Float value. Raises
   --  Types.Data_Error if this parameter does not exist or is not a Double.

   function Get (P : List; Name : String) return String with Inline;
   --  Returns parameter named Name in P as a String value. Raises
   --  Types.Data_Error if this parameter does not exist or is not a String.

   function Get (P : List; Name : String) return Unbounded_String with Inline;
   --  Idem as above, but return an Unbounded_String

   function Get (P : List; Name : String) return Boolean with Inline;
   --  Returns parameter named Name in P as a Boolean value. Raises
   --  Types.Data_Error if this parameter does not exist or is not a Boolean.

   function Get (P : List; Name : String) return Ada.Calendar.Time with Inline;
   --  Returns parameter named Name in P as a Time value. Raises
   --  Types.Data_Error if this parameter does not exist or is not a time.

   function Get (P : List; Name : String) return Duration with Inline;
   --  Returns parameter named Name in P as a Duration value. Raises
   --  Types.Data_Error if this parameter does not exist or is not a Duration.

   function Get (P : List; Name : String) return Types.Unsigned_Long
     with Inline;
   --  Returns parameter named Name in P as a Unsigned_Long value. Raises
   --  Types.Data_Error if this parameter does not exist or is not an
   --  Unsigned_Long.

   function Get (P : List; Name : String) return Types.Unsigned_Int
     with Inline;
   --  Returns parameter named Name in P as a Unsigned_Int value. Raises
   --  Types.Data_Error if this parameter does not exist or is not an
   --  Unsigned_Int.

   function Get (P : List; Name : String) return Types.Unsigned_Short
     with Inline;
   --  Returns parameter named Name in P as a Unsigned_Short value. Raises
   --  Types.Data_Error if this parameter does not exist or is not an
   --  Unsigned_Short.

   function Get (P : List; Name : String) return Types.Unsigned_Byte
     with Inline;
   --  Returns parameter named Name in P as a Unsigned_Byte value. Raises
   --  Types.Data_Error if this parameter does not exist or is not an
   --  Unsigned_Byte.

   function Get (P : List; Name : String) return Types.SOAP_Base64 with Inline;
   --  Returns parameter named Name in P as a SOAP Base64 value. Raises
   --  Types.Data_Error if this parameter does not exist or is not a SOAP
   --  Base64.

   function Get (P : List; Name : String) return Types.SOAP_Record with Inline;
   --  Returns parameter named Name in P as a SOAP Struct value. Raises
   --  Types.Data_Error if this parameter does not exist or is not a SOAP
   --  Struct.

   function Get (P : List; Name : String) return Types.SOAP_Array with Inline;
   --  Returns parameter named Name in P as a SOAP Array value. Raises
   --  Types.Data_Error if this parameter does not exist or is not a SOAP
   --  Array.

   ------------------
   -- Constructors --
   ------------------

   function "&" (P : List; O : Types.Object'Class) return List with
     Post => Argument_Count ("&"'Result) = Argument_Count (P) + 1;

   function "+" (O : Types.Object'Class) return List with
     Post => Argument_Count ("+"'Result) = 1;

   ----------------
   -- Validation --
   ----------------

   procedure Check (P : List; N : Natural);
   --  Checks that there is exactly N parameters or raise Types.Data_Error

   procedure Check_Integer (P : List; Name : String);
   --  Checks that parameter named Name exist and is an Integer value

   procedure Check_Float (P : List; Name : String);
   --  Checks that parameter named Name exist and is a Float value

   procedure Check_Boolean (P : List; Name : String);
   --  Checks that parameter named Name exist and is a Boolean value

   procedure Check_Time_Instant (P : List; Name : String);
   --  Checks that parameter named Name exist and is a Time_Instant value

   procedure Check_Duration (P : List; Name : String);
   --  Checks that parameter named Name exists and is a Duration value

   procedure Check_Base64 (P : List; Name : String);
   --  Checks that parameter named Name exist and is a Base64 value

   procedure Check_Null (P : List; Name : String);
   --  Checks that parameter named Name exist and is a Null value

   procedure Check_Record (P : List; Name : String);
   --  Checks that parameter named Name exist and is a Record value

   procedure Check_Array (P : List; Name : String);
   --  Checks that parameter named Name exist and is an Array value

private

   type List is record
      V : Types.Object_Set (1 .. Max_Parameters);
      N : Natural := 0;
   end record;

end SOAP.Parameters;
