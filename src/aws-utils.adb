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

with Ada.Integer_Text_IO;
with Ada.Strings.Fixed;
with Ada.Numerics.Discrete_Random;
with Ada.Exceptions;
with Interfaces.C.Strings;

with Sockets.Thin;

package body AWS.Utils is

   use Ada;

   package Integer_Random is new Ada.Numerics.Discrete_Random (Random_Integer);

   Random_Generator : Integer_Random.Generator;

   -------------
   -- Get_MD5 --
   -------------

   function Get_MD5 (Data : in String) return MD5.Digest_String is
      Ctx : MD5.Context;
      HA  : MD5.Fingerprint;
   begin
      MD5.Init (Ctx);
      MD5.Update (Ctx, Data);
      MD5.Final (Ctx, HA);
      return MD5.Digest_To_Text (HA);
   end Get_MD5;

   -----------------
   -- Gethostname --
   -----------------

   function Gethostname return String is

      use Interfaces;

      Buffer : aliased C.char_array := (1 .. 100 => ' ');
      Name   : constant C.Strings.chars_ptr
        := C.Strings.To_Chars_Ptr (Buffer'Unchecked_Access);
      Len    : constant C.int := Buffer'Length;
      Res    : C.int;

   begin
      Res := Sockets.Thin.C_Gethostname (Name, Len);
      return C.Strings.Value (Name, C.size_t (Len));
   end Gethostname;

   ---------
   -- Hex --
   ---------

   function Hex (V : in Natural; Width : in Natural := 0) return String is
      use Strings;

      Hex_V : String (1 .. Integer'Size / 4 + 4);
   begin
      Ada.Integer_Text_IO.Put (Hex_V, V, 16);

      declare
         Result : constant String
           := Hex_V (Fixed.Index (Hex_V, "#") + 1
                       .. Fixed.Index (Hex_V, "#", Backward) - 1);
      begin
         if Width = 0 then
            return Result;

         elsif Result'Length < Width then
            declare
               use Ada.Strings.Fixed;
               Zero : constant String := (Width - Result'Length) * '0';
            begin
               return Zero & Result;
            end;

         else
            return Result (Result'Last - Width + 1 .. Result'Last);
         end if;
      end;
   end Hex;

   ---------------
   -- Hex_Value --
   ---------------

   function Hex_Value (Hex : in String) return Natural is

      function Value (C : in Character) return Natural;
      pragma Inline (Value);
      --  Return value for single character C.

      function Value (C : in Character) return Natural is
      begin
         case C is
            when '0'       => return 0;
            when '1'       => return 1;
            when '2'       => return 2;
            when '3'       => return 3;
            when '4'       => return 4;
            when '5'       => return 5;
            when '6'       => return 6;
            when '7'       => return 7;
            when '8'       => return 8;
            when '9'       => return 9;
            when 'a' | 'A' => return 10;
            when 'b' | 'B' => return 11;
            when 'c' | 'C' => return 12;
            when 'd' | 'D' => return 13;
            when 'e' | 'E' => return 14;
            when 'f' | 'F' => return 15;
            when others    => raise Constraint_Error;
         end case;
      end Value;

      R   : Natural := 0;
      Exp : Natural := 1;

   begin
      for K in reverse Hex'Range loop
         R := R + Exp * Value (Hex (K));
         Exp := Exp * 16;
      end loop;

      return R;
   end Hex_Value;

   -----------
   -- Image --
   -----------

   function Image (N : in Natural) return String is
      N_Img : constant String := Natural'Image (N);
   begin
      return N_Img (N_Img'First + 1 .. N_Img'Last);
   end Image;

   -----------
   -- Image --
   -----------

   function Image (D : in Duration) return String is
      D_Img : constant String  := Duration'Image (D);
      K     : constant Natural := Strings.Fixed.Index (D_Img, ".");
   begin
      if K = 0 then
         return D_Img (D_Img'First + 1 .. D_Img'Last);
      else
         return D_Img (D_Img'First + 1 .. K + 2);
      end if;
   end Image;

   ----------------------------
   -- Parse_HTTP_Header_Line --
   ----------------------------

   procedure Parse_HTTP_Header_Line
     (Data   : in     String;
      Result :    out Result_Set)
   is
      use Strings.Unbounded;

      EDel  : constant String := ",";
      --  Delimiter between entities in the http header line

      NVDel : constant String := "=";
      --  Delimiter between name and value

      Attribute               : Enum;
      Name_First              : Natural := Data'First;
      Name_Last               : Natural;
      Value_First, Value_Last : Natural;
      Recognized              : Boolean;
      Value                   : Unbounded_String;

   begin
      loop
         Name_Last
           := Strings.Fixed.Index (Data (Name_First .. Data'Last), NVDel);

         exit when Name_Last = 0;

         begin
            Attribute  := Enum'Value (Data (Name_First .. Name_Last - 1));
            Recognized := True;
         exception
            when Constraint_Error =>
               --  Ignoring unrecognized value
               Recognized := False;
         end;

         Value_First := Name_Last + NVDel'Length;

         --  Quoted value

         if Data (Value_First) = '"' then
            Value_Last := Strings.Fixed.Index
              (Data (Value_First + 1 .. Data'Last), """");

            --  If format error

            if Value_Last = 0 then
               Ada.Exceptions.Raise_Exception
                 (Constraint_Error'Identity,
                  "HTTP header line format error: " & Data);
            end if;

            Value := To_Unbounded_String
              (Data (Value_First + 1 .. Value_Last - 1));

            Name_First := Value_Last + EDel'Length + 1;

         else
            --  Unquoted value

            Value_Last := Ada.Strings.Fixed.Index
              (Data (Value_First .. Data'Last), EDel);

            if Value_Last = 0 then
               Value_Last := Data'Last + 1;
            end if;

            Value := To_Unbounded_String
              (Data (Value_First .. Value_Last - 1));

            Name_First := Value_Last + EDel'Length;
         end if;

         if Recognized then
            Result (Attribute) := Value;
         end if;

         exit when Name_First >= Data'Last;
      end loop;
   end Parse_HTTP_Header_Line;

   ------------
   -- Random --
   ------------

   function Random return Random_Integer is
   begin
      return Integer_Random.Random (Random_Generator);
   end Random;

begin
   Integer_Random.Reset (Random_Generator);
end AWS.Utils;
