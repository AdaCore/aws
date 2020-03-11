------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2020, AdaCore                     --
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

with Ada.Tags;

package body SOAP.Parameters is

   ---------
   -- "&" --
   ---------

   function "&" (P : List; O : Types.Object'Class) return List is
      NP : List := P;
   begin
      NP.N := NP.N + 1;
      NP.V (NP.N) := Types."+" (O);
      return NP;
   end "&";

   ---------
   -- "+" --
   ---------

   function "+" (O : Types.Object'Class) return List is
      P : List;
   begin
      P.V (1) := Types."+" (O);
      P.N := 1;
      return P;
   end "+";

   --------------
   -- Argument --
   --------------

   function Argument
     (P    : List;
      Name : String) return Types.Object'Class
   is
      use type Types.Object_Safe_Pointer;
   begin
      for K in 1 .. P.N loop
         if Types.Name (-P.V (K)) = Name then
            return -P.V (K);
         end if;
      end loop;

      raise Data_Error with "Argument named " & Name & " not found";
   end Argument;

   --------------
   -- Argument --
   --------------

   function Argument
     (P : List;
      N : Positive) return Types.Object'Class
   is
      use type Types.Object_Safe_Pointer;
   begin
      return -P.V (N);
   end Argument;

   --------------------
   -- Argument_Count --
   --------------------

   function Argument_Count (P : List) return Natural is
   begin
      return P.N;
   end Argument_Count;

   -----------
   -- Check --
   -----------

   procedure Check (P : List; N : Natural) is
   begin
      if P.N /= N then
         raise Data_Error with "(check) Too many arguments";
      end if;
   end Check;

   -----------------
   -- Check_Array --
   -----------------

   procedure Check_Array (P : List; Name : String) is
      O : constant Types.Object'Class := Argument (P, Name);
   begin
      if O not in Types.SOAP_Array then
         raise Data_Error
           with "(check) SOAP_Array expected, found object "
             & Ada.Tags.Expanded_Name (O'Tag);
      end if;
   end Check_Array;

   ------------------
   -- Check_Base64 --
   ------------------

   procedure Check_Base64 (P : List; Name : String) is
      O : constant Types.Object'Class := Argument (P, Name);
   begin
      if O not in Types.SOAP_Base64 then
         raise Data_Error
           with "(check) SOAP_Base64 expected, found object "
             & Ada.Tags.Expanded_Name (O'Tag);
      end if;
   end Check_Base64;

   -------------------
   -- Check_Boolean --
   -------------------

   procedure Check_Boolean (P : List; Name : String) is
      O : constant Types.Object'Class := Argument (P, Name);
   begin
      if O not in Types.XSD_Boolean then
         raise Data_Error
           with "(check) XSD_Boolean expected, found object "
             & Ada.Tags.Expanded_Name (O'Tag);
      end if;
   end Check_Boolean;

   --------------------
   -- Check_Duration --
   --------------------

   procedure Check_Duration (P : List; Name : String) is
      O : constant Types.Object'Class := Argument (P, Name);
   begin
      if O not in Types.XSD_Duration then
         raise Data_Error
           with "(check) XSD_Duration expected, found object "
             & Ada.Tags.Expanded_Name (O'Tag);
      end if;
   end Check_Duration;

   -----------------
   -- Check_Float --
   -----------------

   procedure Check_Float (P : List; Name : String) is
      O : constant Types.Object'Class := Argument (P, Name);
   begin
      if O not in Types.XSD_Float then
         raise Data_Error
           with "(check) XSD_Float expected, found object "
             & Ada.Tags.Expanded_Name (O'Tag);
      end if;
   end Check_Float;

   -------------------
   -- Check_Integer --
   -------------------

   procedure Check_Integer (P : List; Name : String) is
      O : constant Types.Object'Class := Argument (P, Name);
   begin
      if O not in Types.XSD_Integer then
         raise Data_Error
           with "(check) XSD_Integer expected, found object "
             & Ada.Tags.Expanded_Name (O'Tag);
      end if;
   end Check_Integer;

   ----------------
   -- Check_Null --
   ----------------

   procedure Check_Null (P : List; Name : String) is
      O : constant Types.Object'Class := Argument (P, Name);
   begin
      if O not in Types.XSD_Null then
         raise Data_Error
           with "(check) XSD_Null expected, found object "
             & Ada.Tags.Expanded_Name (O'Tag);
      end if;
   end Check_Null;

   ------------------
   -- Check_Record --
   ------------------

   procedure Check_Record (P : List; Name : String) is
      O : constant Types.Object'Class := Argument (P, Name);
   begin
      if O not in Types.SOAP_Record then
         raise Data_Error
           with "(check) SOAP_Record expected, found object "
             & Ada.Tags.Expanded_Name (O'Tag);
      end if;
   end Check_Record;

   ------------------------
   -- Check_Time_Instant --
   ------------------------

   procedure Check_Time_Instant (P : List; Name : String) is
      O : constant Types.Object'Class := Argument (P, Name);
   begin
      if O not in Types.XSD_Time_Instant then
         raise Data_Error
           with "(check) XSD_Time_Instant expected, found object "
             & Ada.Tags.Expanded_Name (O'Tag);
      end if;
   end Check_Time_Instant;

   -----------
   -- Exist --
   -----------

   function Exist (P : List; Name : String) return Boolean is
      use type Types.Object_Safe_Pointer;
   begin
      for K in 1 .. P.N loop
         if Types.Name (-P.V (K)) = Name then
            return True;
         end if;
      end loop;

      return False;
   end Exist;

   ---------
   -- Get --
   ---------

   function Get (P : List; Name : String) return Types.Long is
   begin
      return Types.Get (Argument (P, Name));
   end Get;

   function Get (P : List; Name : String) return Integer is
   begin
      return Types.Get (Argument (P, Name));
   end Get;

   function Get (P : List; Name : String) return Types.Short is
   begin
      return Types.Get (Argument (P, Name));
   end Get;

   function Get (P : List; Name : String) return Types.Byte is
   begin
      return Types.Get (Argument (P, Name));
   end Get;

   function Get (P : List; Name : String) return Float is
      pragma Suppress (Validity_Check);
   begin
      return Types.Get (Argument (P, Name));
   end Get;

   function Get (P : List; Name : String) return Long_Float is
      pragma Suppress (Validity_Check);
   begin
      return Types.Get (Argument (P, Name));
   end Get;

   function Get (P : List; Name : String) return String is
   begin
      return Types.Get (Argument (P, Name));
   end Get;

   function Get (P : List; Name : String) return Unbounded_String is
   begin
      return Types.Get (Argument (P, Name));
   end Get;

   function Get (P : List; Name : String) return Boolean is
   begin
      return Types.Get (Argument (P, Name));
   end Get;

   function Get (P : List; Name : String) return Ada.Calendar.Time is
   begin
      return Types.Get (Argument (P, Name));
   end Get;

   function Get (P : List; Name : String) return Duration is
   begin
      return Types.Get (Argument (P, Name));
   end Get;

   function Get (P : List; Name : String) return Types.Unsigned_Long is
   begin
      return Types.Get (Argument (P, Name));
   end Get;

   function Get (P : List; Name : String) return Types.Unsigned_Int is
   begin
      return Types.Get (Argument (P, Name));
   end Get;

   function Get (P : List; Name : String) return Types.Unsigned_Short is
   begin
      return Types.Get (Argument (P, Name));
   end Get;

   function Get (P : List; Name : String) return Types.Unsigned_Byte is
   begin
      return Types.Get (Argument (P, Name));
   end Get;

   function Get (P : List; Name : String) return Types.SOAP_Base64 is
   begin
      return Types.Get (Argument (P, Name));
   end Get;

   function Get (P : List; Name : String) return Types.SOAP_Record is
   begin
      return Types.Get (Argument (P, Name));
   end Get;

   function Get (P : List; Name : String) return Types.SOAP_Array is
   begin
      return Types.Get (Argument (P, Name));
   end Get;

end SOAP.Parameters;
