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

with Ada.Tags;
with Ada.Exceptions;

with SOAP.Types;

package body SOAP.Parameters is

   use Ada;

   ---------
   -- "&" --
   ---------

   function "&" (P : in List; O : in Types.Object'Class) return List is
      NP : List := P;
   begin
      NP.N := NP.N + 1;
      NP.V (NP.N) := Types."+" (O);
      return NP;
   end "&";

   ---------
   -- "+" --
   ---------

   function "+" (O : in Types.Object'Class) return List is
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
     (P    : in List;
      Name : in String)
      return Types.Object'Class is
   begin
      for K in 1 .. P.N loop
         if Types.Name (P.V (K).O.all) = Name then
            return P.V (K).O.all;
         end if;
      end loop;

      Exceptions.Raise_Exception
        (Types.Data_Error'Identity,
         "Argument named '" & Name & "' not found.");
   end Argument;

   --------------
   -- Argument --
   --------------

   function Argument
     (P : in List;
      N : in Positive)
      return Types.Object'Class is
   begin
      return P.V (N).O.all;
   end Argument;

   --------------------
   -- Argument_Count --
   --------------------

   function Argument_Count (P : in List) return Natural is
   begin
      return P.N;
   end Argument_Count;

   -----------
   -- Check --
   -----------

   procedure Check (P : in List; N : in Natural) is
   begin
      if P.N /= N then
         Exceptions.Raise_Exception
           (Types.Data_Error'Identity,
            "(check) Too many arguments.");
      end if;
   end Check;

   -----------------
   -- Check_Array --
   -----------------

   procedure Check_Array (P : in List; Name : in String) is
      O : Types.Object'Class := Argument (P, Name);
   begin
      if O not in Types.SOAP_Array then
         Exceptions.Raise_Exception
           (Types.Data_Error'Identity,
            "(check) SOAP_Array expected, found object "
            & Ada.Tags.Expanded_Name (O'Tag));
      end if;
   end Check_Array;

   ------------------
   -- Check_Base64 --
   ------------------

   procedure Check_Base64 (P : in List; Name : in String) is
      O : Types.Object'Class := Argument (P, Name);
   begin
      if O not in Types.SOAP_Base64 then
         Exceptions.Raise_Exception
           (Types.Data_Error'Identity,
            "(check) SOAP_Base64 expected, found object "
            & Ada.Tags.Expanded_Name (O'Tag));
      end if;
   end Check_Base64;

   -------------------
   -- Check_Boolean --
   -------------------

   procedure Check_Boolean (P : in List; Name : in String) is
      O : Types.Object'Class := Argument (P, Name);
   begin
      if O not in Types.XSD_Boolean then
         Exceptions.Raise_Exception
           (Types.Data_Error'Identity,
            "(check) XSD_Boolean expected, found object "
            & Ada.Tags.Expanded_Name (O'Tag));
      end if;
   end Check_Boolean;

   -----------------
   -- Check_Float --
   -----------------

   procedure Check_Float (P : in List; Name : in String) is
      O : Types.Object'Class := Argument (P, Name);
   begin
      if O not in Types.XSD_Float then
         Exceptions.Raise_Exception
           (Types.Data_Error'Identity,
            "(check) XSD_Float expected, found object "
            & Ada.Tags.Expanded_Name (O'Tag));
      end if;
   end Check_Float;

   -------------------
   -- Check_Integer --
   -------------------

   procedure Check_Integer (P : in List; Name : in String) is
      O : Types.Object'Class := Argument (P, Name);
   begin
      if O not in Types.XSD_Integer then
         Exceptions.Raise_Exception
           (Types.Data_Error'Identity,
            "(check) XSD_Integer expected, found object "
            & Ada.Tags.Expanded_Name (O'Tag));
      end if;
   end Check_Integer;

   ----------------
   -- Check_Null --
   ----------------

   procedure Check_Null (P : in List; Name : in String) is
      O : Types.Object'Class := Argument (P, Name);
   begin
      if O not in Types.XSD_Null then
         Exceptions.Raise_Exception
           (Types.Data_Error'Identity,
            "(check) XSD_Null expected, found object "
            & Ada.Tags.Expanded_Name (O'Tag));
      end if;
   end Check_Null;

   ------------------
   -- Check_Record --
   ------------------

   procedure Check_Record (P : in List; Name : in String) is
      O : Types.Object'Class := Argument (P, Name);
   begin
      if O not in Types.SOAP_Record then
         Exceptions.Raise_Exception
           (Types.Data_Error'Identity,
            "(check) SOAP_Record expected, found object "
            & Ada.Tags.Expanded_Name (O'Tag));
      end if;
   end Check_Record;

   ------------------------
   -- Check_Time_Instant --
   ------------------------

   procedure Check_Time_Instant (P : in List; Name : in String) is
      O : Types.Object'Class := Argument (P, Name);
   begin
      if O not in Types.XSD_Time_Instant then
         Exceptions.Raise_Exception
           (Types.Data_Error'Identity,
            "(check) XSD_Time_Instant expected, found object "
            & Ada.Tags.Expanded_Name (O'Tag));
      end if;
   end Check_Time_Instant;

   -----------
   -- Exist --
   -----------

   function Exist (P : in List; Name : in String) return Boolean is
   begin
      for K in 1 .. P.N loop
         if Types.Name (P.V (K).O.all) = Name then
            return True;
         end if;
      end loop;

      return False;
   end Exist;

   ---------
   -- Get --
   ---------

   function Get (P : in List; Name : in String) return Integer is
   begin
      return Types.Get (Argument (P, Name));
   end Get;

   function Get (P : in List; Name : in String) return Long_Float is
   begin
      return Types.Get (Argument (P, Name));
   end Get;

   function Get (P : in List; Name : in String) return String is
   begin
      return Types.Get (Argument (P, Name));
   end Get;

   function Get (P : in List; Name : in String) return Boolean is
   begin
      return Types.Get (Argument (P, Name));
   end Get;

   function Get (P : in List; Name : in String) return Types.SOAP_Record is
   begin
      return Types.Get (Argument (P, Name));
   end Get;

   function Get (P : in List; Name : in String) return Types.SOAP_Array is
   begin
      return Types.Get (Argument (P, Name));
   end Get;

end SOAP.Parameters;
