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

with SOAP.Types;

package SOAP.Parameters is

   Max_Parameters : constant := 50;
   --  This is the maximum number of parameters supported by this
   --  implementation.

   type List is private;

   function Argument_Count (P : in List) return Natural;
   --  Returns the number of parameters in P.

   function Argument (P : in List; Name : in String) return Types.Object'Class;
   --  Returns parameters named Name in P. Raises Types.Data_Error if not
   --  found.

   function Argument (P : in List; N : in Positive) return Types.Object'Class;
   --  Returns Nth parameters in P. Raises Types.Data_Error if not found.

   function Exist (P : in List; Name : in String) return Boolean;
   --  Returns True if parameter named Name exist in P and False otherwise.

   function Get (P : in List; Name : in String) return Integer;
   --  Returns parameter named Name in P as an Integer value. Raises
   --  Types.Data_Error if this parameter does not exist or is not an Integer.

   function Get (P : in List; Name : in String) return Long_Float;
   --  Returns parameter named Name in P as a Float value. Raises
   --  Types.Data_Error if this parameter does not exist or is not a Float.

   function Get (P : in List; Name : in String) return String;
   --  Returns parameter named Name in P as a String value. Raises
   --  Types.Data_Error if this parameter does not exist or is not a String.

   function Get (P : in List; Name : in String) return Boolean;
   --  Returns parameter named Name in P as a Boolean value. Raises
   --  Types.Data_Error if this parameter does not exist or is not a Boolean.

   function Get (P : in List; Name : in String) return Types.SOAP_Record;
   --  Returns parameter named Name in P as a SOAP Struct value. Raises
   --  Types.Data_Error if this parameter does not exist or is not a SOAP
   --  Struct.

   function Get (P : in List; Name : in String) return Types.SOAP_Array;
   --  Returns parameter named Name in P as a SOAP Array value. Raises
   --  Types.Data_Error if this parameter does not exist or is not a SOAP
   --  Array.

   ------------------
   -- Constructors --
   ------------------

   function "&" (P : in List; O : in Types.Object'Class) return List;
   function "+" (O : in Types.Object'Class) return List;

   ----------------
   -- Validation --
   ----------------

   procedure Check (P : in List; N : in Natural);
   --  Checks that there is exactly N parameters or raise Types.Data_Error.

   procedure Check_Integer (P : in List; Name : in String);
   --  Checks that parameter named Name exist and is an Integer value.

   procedure Check_Float (P : in List; Name : in String);
   --  Checks that parameter named Name exist and is a Float value.

   procedure Check_Boolean (P : in List; Name : in String);
   --  Checks that parameter named Name exist and is a Boolean value.

   procedure Check_Time_Instant (P : in List; Name : in String);
   --  Checks that parameter named Name exist and is a Time_Instant value.

   procedure Check_Base64 (P : in List; Name : in String);
   --  Checks that parameter named Name exist and is a Base64 value.

   procedure Check_Null (P : in List; Name : in String);
   --  Checks that parameter named Name exist and is a Null value.

   procedure Check_Record (P : in List; Name : in String);
   --  Checks that parameter named Name exist and is a Record value.

   procedure Check_Array (P : in List; Name : in String);
   --  Checks that parameter named Name exist and is an Array value.

private

   type List is record
      V : Types.Object_Set (1 .. Max_Parameters);
      N : Natural := 0;
   end record;

end SOAP.Parameters;
