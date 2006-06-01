------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2006                          --
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

with Ada.Integer_Text_IO;
with Ada.Numerics.Long_Elementary_Functions;
with Ada.Streams.Stream_IO;
with Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants;
with Ada.Text_IO;
with Ada.Numerics.Discrete_Random;

with AWS.OS_Lib;

package body AWS.Utils is

   package Integer_Random is new Numerics.Discrete_Random (Random_Integer);

   procedure Compress_Decompress
     (Filter       : in out ZLib.Filter_Type;
      Filename_In  : in     String;
      Filename_Out : in     String);
   --  Compress or decompress (depending on the filter initialization)
   --  from Filename_In to Filename_Out.

   Random_Generator : Integer_Random.Generator;

   --------------
   -- Compress --
   --------------

   procedure Compress
     (Filename : in String;
      Level    : in ZLib.Compression_Level := ZLib.Default_Compression)
   is
      Filter : ZLib.Filter_Type;

   begin
      ZLib.Deflate_Init (Filter, Level => Level, Header => ZLib.GZip);

      Compress_Decompress (Filter, Filename, Filename & ".gz");

      ZLib.Close (Filter);
   exception
      when others =>
         ZLib.Close (Filter, Ignore_Error => True);
         raise;
   end Compress;

   -------------------------
   -- Compress_Decompress --
   -------------------------

   procedure Compress_Decompress
     (Filter       : in out ZLib.Filter_Type;
      Filename_In  : in     String;
      Filename_Out : in     String)
   is
      use Streams;
      procedure Data_In
        (Item : out Stream_Element_Array;
         Last : out Stream_Element_Offset);
      --  Retrieve a chunk of data from the file

      procedure Data_Out
        (Item : in Stream_Element_Array);
      --  Write a chunk of data into the compressed file

      procedure Translate is new ZLib.Generic_Translate (Data_In, Data_Out);

      File_In, File_Out : Stream_IO.File_Type;

      -------------
      -- Data_In --
      -------------

      procedure Data_In
        (Item : out Stream_Element_Array;
         Last : out Stream_Element_Offset) is
      begin
         Stream_IO.Read (File_In, Item, Last);
      end Data_In;

      --------------
      -- Data_Out --
      --------------

      procedure Data_Out (Item : in Stream_Element_Array) is
      begin
         Stream_IO.Write (File_Out, Item);
      end Data_Out;

   begin
      Stream_IO.Open (File_In, Stream_IO.In_File, Filename_In);
      Stream_IO.Create (File_Out, Stream_IO.Out_File, Filename_Out);

      Translate (Filter);

      Stream_IO.Close (File_Out);

      --  Everything was ok, let's remove the original file now

      Stream_IO.Delete (File_In);

   exception
      when others =>
         if Stream_IO.Is_Open (File_In) then
            Stream_IO.Close (File_In);
         end if;

         if Stream_IO.Is_Open (File_Out) then
            Stream_IO.Close (File_Out);
         end if;
         raise;
   end Compress_Decompress;

   -------------
   -- Counter --
   -------------

   protected body Counter is

      ---------------
      -- Decrement --
      ---------------

      procedure Decrement (Amount : Natural := 1) is
      begin
         C := C - Amount;
      end Decrement;

      procedure Decrement (Amount : Natural := 1; Value : out Natural) is
      begin
         C := C - Amount;
         Value := C;
      end Decrement;

      ---------------
      -- Increment --
      ---------------

      procedure Increment (Amount : Natural := 1) is
      begin
         C := C + Amount;
      end Increment;

      -----------
      -- Value --
      -----------

      function Value return Natural is
      begin
         return C;
      end Value;

      ----------
      -- Zero --
      ----------

      entry Zero when C = 0 is
      begin
         null;
      end Zero;

   end Counter;

   -------------------
   -- CRLF_2_Spaces --
   -------------------

   function CRLF_2_Spaces (Str : in String) return String is
   begin
      return Strings.Fixed.Trim
        (Strings.Fixed.Translate
           (Str, Strings.Maps.To_Mapping
              (From => ASCII.CR & ASCII.LF, To   => "  ")),
         Strings.Right);
   end CRLF_2_Spaces;

   ----------------
   -- Decompress --
   ----------------

   procedure Decompress (Filename : in String) is
      Filter : ZLib.Filter_Type;

   begin
      ZLib.Inflate_Init (Filter, Header => ZLib.GZip);

      Compress_Decompress
        (Filter, Filename, OS_Lib.Base_Name (Filename, ".gz"));

      ZLib.Close (Filter);
   exception
      when others =>
         ZLib.Close (Filter, Ignore_Error => True);
         raise;
   end Decompress;

   ---------
   -- Hex --
   ---------

   function Hex (V : in Natural; Width : in Natural := 0) return String is
      use Strings;

      Hex_V : String (1 .. Integer'Size / 4 + 4);
   begin
      Integer_Text_IO.Put (Hex_V, V, 16);

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

      -----------
      -- Value --
      -----------

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

   ---------------
   -- Is_Number --
   ---------------

   function Is_Number (S : in String) return Boolean is
      use Strings.Maps;
   begin
      return S'Length > 0
        and then Is_Subset (To_Set (S), Constants.Decimal_Digit_Set);
   end Is_Number;

   -------------
   -- Mailbox --
   -------------

   package body Mailbox_G is

      protected body Mailbox is

         ---------
         -- Add --
         ---------

         entry Add (M : in Message) when Current_Size < Max_Size is
         begin
            Current_Size := Current_Size + 1;
            Current := Current + 1;

            if Current > Max_Size then
               Current := Buffer'First;
            end if;

            Buffer (Current) := M;
         end Add;

         ---------
         -- Get --
         ---------

         entry Get (M : out Message) when Current_Size > 0 is
         begin
            Current_Size := Current_Size - 1;
            Last := Last + 1;

            if Last > Max_Size then
               Last := Buffer'First;
            end if;

            M := Buffer (Last);
         end Get;

         ----------
         -- Size --
         ----------

         function Size return Natural is
         begin
            return Current_Size;
         end Size;

      end Mailbox;

   end Mailbox_G;

   -----------
   -- Quote --
   -----------

   function Quote
     (Str : in String; Replace : in String := """") return String
   is
      function Replace_Quote (Source : in String) return String;

      -------------------
      -- Replace_Quote --
      -------------------

      function Replace_Quote (Source : in String) return String is
         use Ada.Strings.Fixed;
         Idx : Natural;
      begin
         Idx := Index (Source, """");

         if Idx = 0 then
            return Source;
         else
            return Source (Source'First .. Idx - 1) & Replace &
                   Replace_Quote (Source (Idx + 1 .. Source'Last));
         end if;
      end Replace_Quote;

   begin
      if Replace'Length = 1 then
         --  When length of replace string is 1, we could replace it faster.

         if Replace (Replace'First) = '"' then
            --  Do not need to replace.
            return '"' & Str & '"';

         else
            declare
               Result : String (Str'Range);
            begin
               for J in Str'Range loop
                  if Str (J) = '"' then
                     Result (J) := Replace (Replace'First);
                  else
                     Result (J) := Str (J);
                  end if;
               end loop;

               return '"' & Result & '"';
            end;
         end if;

      else
         return '"' & Replace_Quote (Str) & '"';
      end if;
   end Quote;

   ------------
   -- Random --
   ------------

   function Random return Random_Integer is
   begin
      return Integer_Random.Random (Random_Generator);
   end Random;

   ------------------
   -- RW_Semaphore --
   ------------------

   protected body RW_Semaphore is

      ----------
      -- Read --
      ----------

      entry Read when W = 0 and then Write'Count = 0 is
      begin
         R := R + 1;
      end Read;

      ------------------
      -- Release_Read --
      ------------------

      procedure Release_Read is
      begin
         R := R - 1;
      end Release_Read;

      -------------------
      -- Release_Write --
      -------------------

      procedure Release_Write is
      begin
         W := W - 1;
      end Release_Write;

      -----------
      -- Write --
      -----------

      entry Write when R = 0 and then W < Writers is
      begin
         W := W + 1;
      end Write;

   end RW_Semaphore;

   ---------------
   -- Semaphore --
   ---------------

   protected body Semaphore is

      -------------
      -- Release --
      -------------

      procedure Release is
      begin
         if TID = Current_Task then
            Seized := Seized - 1;
         else
            raise Tasking_Error;
         end if;
      end Release;

      -----------
      -- Seize --
      -----------

      entry Seize when True is
      begin
         if Seize'Caller = TID then
            Seized := Seized + 1;
         else
            requeue Seize_Internal;
         end if;
      end Seize;

      --------------------
      -- Seize_Internal --
      --------------------

      entry Seize_Internal when Seized = 0 is
      begin
         Seized := Seized + 1;
         TID    := Seize_Internal'Caller;
      end Seize_Internal;

   end Semaphore;

   -----------------------
   -- Significant_Image --
   -----------------------

   function Significant_Image (Item : Duration; N : Positive) return String is
      AI   : constant Duration   := abs Item;
      L10  : constant Long_Float := Ada.Numerics.Long_Elementary_Functions.Log
                                      (Long_Float (AI), 10.0);
      L10T : constant Long_Float := Long_Float'Truncation (L10);
      PP   : constant Integer    := Integer (L10T);
      Aft  : constant Natural
        := Integer'Max
             (N - PP - Boolean'Pos (AI >= 1.0 or else L10 = L10T), 0);
      Img  : String (1 .. Integer'Max (PP, 1) + Aft + 1 - Boolean'Pos (Aft = 0)
                          + Boolean'Pos (AI >= 10.0)
                          + Boolean'Pos (Item < 0.0));
      package Duration_IO is new Ada.Text_IO.Fixed_IO (Duration);
   begin
      if Aft = 0 then
         Ada.Integer_Text_IO.Put (Img, Integer (Item));
      else
         Duration_IO.Put (Img, Item, Aft => Aft);
      end if;

      return Img;
   end Significant_Image;

begin
   Integer_Random.Reset (Random_Generator);
end AWS.Utils;
