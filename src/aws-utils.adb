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

   -----------------
   -- Gethostname --
   -----------------

   function Gethostname return String is

      use Interfaces;

      Buffer : aliased C.char_array := (1 .. 100 => ' ');
      Name   : C.Strings.chars_ptr
        := C.Strings.To_Chars_Ptr (Buffer'Unchecked_Access);
      Len    : C.int := Buffer'Length;
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
