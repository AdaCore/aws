------------------------------------------------------------------------------
--                            Secure Sockets Layer                          --
--                         Binding to OpenSSL library                       --
--                                                                          --
--                            Copyright (C) 2005                            --
--                                 AdaCore                                  --
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

with Ada.Numerics.Discrete_Random;
with Ada.Unchecked_Conversion;
with Interfaces.C.Strings;
with System;

with SSL.Thin;

package body SSL.Ada_Random is

   use type System.Address;

   package C renames Interfaces.C;
   package CS renames C.Strings;

   type Byte_Type is mod 256;
   pragma Convention (C, Byte_Type);

   type Byte_Array is array (Positive range <>) of Byte_Type;
   pragma Pack (Byte_Array);
   pragma Convention (C, Byte_Array);

   subtype C_Byte_Array is Byte_Array (Positive'Range);

   type Unsigned is mod 2**System.Word_Size;

   subtype Bytes_4 is Byte_Array (1 .. Unsigned'Size / System.Storage_Unit);

   package Unsigned_Random is new Ada.Numerics.Discrete_Random (Unsigned);

   Generator   : Unsigned_Random.Generator;
   Initialized : Boolean := False;

   function To_Integer is new Ada.Unchecked_Conversion (Unsigned, Integer);
   function To_Bytes_4 is new Ada.Unchecked_Conversion (Unsigned, Bytes_4);
   function To_Unsigned is new Ada.Unchecked_Conversion (Bytes_4, Unsigned);

   procedure Seed (Buf : in C_Byte_Array; Num : Integer);
   pragma Convention (C, Seed);

   function Bytes (Buf : access C_Byte_Array; Num : Integer) return Integer;
   pragma Convention (C, Bytes);

   procedure Cleanup;
   pragma Convention (C, Cleanup);

   procedure Add (Buf : in C_Byte_Array; Num : Integer;  Entropy : Integer);
   pragma Convention (C, Add);

   function Status return Integer;
   pragma Convention (C, Status);

   Method : aliased Thin.Rand_Meth_St
     := (Seed       => Seed'Address,
         Bytes      => Bytes'Address,
         Cleanup    => Cleanup'Address,
         Add        => Add'Address,
         Pseudorand => Bytes'Address,
         Status     => Status'Address);

   function To_Initiator (Buf : in Byte_Array) return Integer;

   ---------
   -- Add --
   ---------

   procedure Add (Buf : in C_Byte_Array; Num : Integer;  Entropy : Integer) is
      use Unsigned_Random;
   begin
      Reset (Generator,
             To_Initiator (To_Bytes_4 (Random (Generator)) & Buf (1 .. Num)));

      Initialized := True;
   end Add;

   -----------
   -- Bytes --
   -----------

   function Bytes (Buf : access C_Byte_Array; Num : Integer) return Integer is
      B4    : Bytes_4;
      Index : Positive := B4'Last + 1;

      function Get_Next return Byte_Type;

      --------------
      -- Get_Next --
      --------------

      function Get_Next return Byte_Type is
      begin
         Index := Index + 1;

         if Index > B4'Last then
            Index := B4'First;
            B4    := To_Bytes_4 (Unsigned_Random.Random (Generator));
         end if;

         return B4 (Index);
      end Get_Next;

   begin
      for J in 1 .. Num loop
         Buf (J) := Get_Next;
      end loop;

      return 1;
   end Bytes;

   -------------
   -- Cleanup --
   -------------

   procedure Cleanup is
   begin
      Unsigned_Random.Reset (Generator, 0);
   end Cleanup;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Thin.RAND_set_rand_method (Method'Access);
   end Initialize;

   ----------
   -- Seed --
   ----------

   procedure Seed (Buf : in C_Byte_Array; Num : Integer) is
   begin
      Unsigned_Random.Reset (Generator, To_Initiator (Buf (1 .. Num)));

      Initialized := True;
   end Seed;

   ------------
   -- Status --
   ------------

   function Status return Integer is
   begin
      return Boolean'Pos (Initialized);
   end Status;

   function To_Initiator (Buf : in Byte_Array) return Integer is
      Init : Unsigned := 0;
   begin
      for J in Buf'Range loop
         declare
            function Next_Unsigned return Unsigned;
            pragma Inline (Next_Unsigned);

            -------------------
            -- Next_Unsigned --
            -------------------

            function Next_Unsigned return Unsigned is
            begin
               case Buf'Last - J is
                  when 0 =>
                     return To_Unsigned ((Buf (J), Buf (Buf'First), 0, 0));
                  when 1 =>
                     return To_Unsigned
                       ((Buf (J), Buf (J + 1), Buf (Buf'First), 0));
                  when 2 =>
                     return To_Unsigned
                       ((Buf (J),
                         Buf (J + 1),
                         Buf (J + 2),
                         Buf (Buf'First)));
                  when others =>
                     return To_Unsigned (Buf (J .. J + 3));
               end case;
            end Next_Unsigned;
         begin
            Init := Init xor Next_Unsigned;
         end;
      end loop;

      return To_Integer (Init);
   end To_Initiator;

end SSL.Ada_Random;
