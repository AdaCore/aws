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

with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;

package body SOAP.Utils is

   use Ada.Strings.Unbounded;

   ------------
   -- Encode --
   ------------

   function Encode (Str : in String) return String is
      Result : Unbounded_String;
   begin
      for K in Str'Range loop
         case Str (K) is
            when '<'    => Append (Result, "&lt;");
            when '>'    => Append (Result, "&gt;");
            when '&'    => Append (Result, "&amp;");
            when '''    => Append (Result, "&apos;");
            when '"'    => Append (Result, "&quot;");
            when others => Append (Result, Str (K));
         end case;
      end loop;

      return To_String (Result);
   end Encode;

   ---------
   -- Get --
   ---------

   function Get (Item : in Types.Object'Class) return Unbounded_String is
   begin
      return To_Unbounded_String (String'(Types.Get (Item)));
   end Get;

   --------------
   -- Is_Array --
   --------------

   function Is_Array (Name : in String) return Boolean is
   begin
      return Name'Length > 7
        and then Name (Name'First .. Name'First + 6) = "ArrayOf";
   end Is_Array;

   -----------
   -- No_NS --
   -----------

   function No_NS (Name : in String) return String is
      K : constant Natural := Ada.Strings.Fixed.Index (Name, ":");
   begin
      if K = 0 then
         return Name;
      else
         return Name (K + 1 .. Name'Last);
      end if;
   end No_NS;

   -------------------
   -- Safe_Pointers --
   -------------------

   package body Safe_Pointers is

      procedure Free is new Ada.Unchecked_Deallocation (T, T_Access);

      procedure Free is new Ada.Unchecked_Deallocation (Natural, Ref_Counter);

      ------------
      -- Adjust --
      ------------

      procedure Adjust (SP : in out Safe_Pointer) is
      begin
         SP.Ref.all := SP.Ref.all + 1;
      end Adjust;

      --------------
      -- Finalize --
      --------------

      procedure Finalize (SP : in out Safe_Pointer) is
      begin
         SP.Ref.all := SP.Ref.all - 1;

         if SP.Ref.all = 0 then
            Free (SP.Item);
            Free (SP.Ref);
         end if;
      end Finalize;

      ----------------
      -- Initialize --
      ----------------

      procedure Initialize (SP : in out Safe_Pointer) is
      begin
         SP.Ref := new Natural'(1);
      end Initialize;

      ----------------------mart
      -- To_Safe_Pointer --
      ----------------------

      function To_Safe_Pointer (Item : in T) return Safe_Pointer is
      begin
         return (Ada.Finalization.Controlled with
                   new T'(Item), new Natural'(1));
      end To_Safe_Pointer;

   end Safe_Pointers;

   ---------
   -- Tag --
   ---------

   function Tag (Name : in String; Start : in Boolean) return String is
   begin
      if Start then
         return '<' & Name & '>';
      else
         return "</" & Name & '>';
      end if;
   end Tag;

   -------------------
   -- To_Object_Set --
   -------------------

   function To_Object_Set (From : in T_Array) return Types.Object_Set is
      use SOAP.Types;
      Result : Types.Object_Set (From'Range);
   begin
      for K in From'Range loop
         Result (K) := +Get (From (K));
      end loop;

      return Result;
   end To_Object_Set;

   ----------------
   -- To_T_Array --
   ----------------

   function To_T_Array (From : in Types.Object_Set) return T_Array is
      use SOAP.Types;
      Result : T_Array (From'Range);
   begin
      for K in From'Range loop
         Result (K) := Get (-From (K));
      end loop;

      return T_Array'(Result);
   end To_T_Array;

   --------
   -- US --
   --------

   function US
     (V      : in Unbounded_String;
      Name   : in String  := "item")
      return Types.XSD_String is
   begin
      return Types.S (To_String (V), Name);
   end US;

   -------
   -- V --
   -------

   function V (O : in Types.XSD_String) return Unbounded_String is
   begin
      return To_Unbounded_String (Types.V (O));
   end V;

end SOAP.Utils;
