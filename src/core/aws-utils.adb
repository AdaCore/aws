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

pragma Ada_2012;

with Ada.Calendar.Time_Zones;
with Ada.Characters.Handling;
with Ada.Integer_Text_IO;
with Ada.Numerics.Discrete_Random;
with Ada.Numerics.Long_Elementary_Functions;
with Ada.Streams.Stream_IO;
with Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants;
with Ada.Text_IO;

pragma Warnings (Off);
with Ada.Strings.Unbounded.Aux;
pragma Warnings (On);

with System;

with AWS.OS_Lib;

package body AWS.Utils is

   use type Ada.Directories.File_Kind;

   package Integer_Random is new Numerics.Discrete_Random (Random_Integer);

   function Local_To_GMT (DT : Calendar.Time) return Calendar.Time
     with Inline;

   procedure Compress_Decompress
     (Filter       : in out ZLib.Filter_Type;
      Filename_In  : String;
      Filename_Out : String);
   --  Compress or decompress (depending on the filter initialization)
   --  from Filename_In to Filename_Out.

   function Is_Valid_UTF8
     (Str : Aux.Big_String_Access; Last : Natural) return Boolean;
   --  Returns True if the string pointed to by Str and terminating to Last
   --  is well-formed UTF-8.

   Random_Generator : Integer_Random.Generator;

   ---------------------
   -- Append_With_Sep --
   ---------------------

   procedure Append_With_Sep
     (Content : in out Unbounded_String;
      Value   : String;
      Sep     : String := ", ") is
   begin
      if Content /= Null_Unbounded_String then
         Append (Content, Sep);
      end if;
      Append (Content, Value);
   end Append_With_Sep;

   --------------
   -- Compress --
   --------------

   procedure Compress
     (Filename : String;
      Level    : ZLib.Compression_Level := ZLib.Default_Compression)
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
      Filename_In  : String;
      Filename_Out : String)
   is
      procedure Data_In
        (Item : out Stream_Element_Array;
         Last : out Stream_Element_Offset);
      --  Retrieve a chunk of data from the file

      procedure Data_Out
        (Item : Stream_Element_Array);
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

      procedure Data_Out (Item : Stream_Element_Array) is
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

      procedure Increment (Amount : Natural := 1; Value : out Natural) is
      begin
         C := C + Amount;
         Value := C;
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

   function CRLF_2_Spaces (Str : String) return String is
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

   procedure Decompress (Filename : String) is
      Filter : ZLib.Filter_Type;

   begin
      ZLib.Inflate_Init (Filter, Header => ZLib.GZip);

      Compress_Decompress
        (Filter, Filename, Directories.Base_Name (Filename));

      ZLib.Close (Filter);
   exception
      when others =>
         ZLib.Close (Filter, Ignore_Error => True);
         raise;
   end Decompress;

   -------------
   -- Dequote --
   -------------

   function Dequote (Str : String) return String is
   begin
      if Str'Length < 2
        or else (Str (Str'First) /= '"' or else Str (Str'Last) /= '"')
      then
         return Str;
      else
         return Str (Str'First + 1 .. Str'Last - 1);
      end if;
   end Dequote;

   ---------------
   -- File_Size --
   ---------------

   function File_Size (Filename : String) return File_Size_Type is
   begin
      if Is_Regular_File (Filename) then
         return Directories.Size (Filename);
      else
         raise No_Such_File with "File " & Filename & " not found.";
      end if;
   end File_Size;

   ---------------------
   -- File_Time_Stamp --
   ---------------------

   function File_Time_Stamp (Filename : String) return Ada.Calendar.Time is
   begin
      if Is_Regular_File (Filename)
        or else Is_Directory (Filename)
      then
         return Local_To_GMT (Directories.Modification_Time (Filename));
      else
         raise No_Such_File with "File " & Filename & " not found.";
      end if;
   end File_Time_Stamp;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Object : in out Finalizer) is
   begin
      Object.Action.all;
   end Finalize;

   -------------------------------
   -- For_Every_Directory_Entry --
   -------------------------------

   procedure For_Every_Directory_Entry (Directory_Name : String) is

      Dir_Name : constant String := Normalized_Directory (Directory_Name);

      Iter : Directories.Search_Type;
      Item : Directories.Directory_Entry_Type;
      Quit : Boolean := False;
   begin
      Directories.Start_Search (Iter, Directory_Name, "");

      while Directories.More_Entries (Iter) loop
         Directories.Get_Next_Entry (Iter, Item);

         Action
           (Directories.Simple_Name (Item),
            Is_Directory (Dir_Name & Directories.Simple_Name (Item)),
            Quit => Quit);

         exit when Quit;
      end loop;

      Directories.End_Search (Iter);

   exception
      when Text_IO.Name_Error =>
         raise No_Such_File;
   end For_Every_Directory_Entry;

   ---------------
   -- GMT_Clock --
   ---------------

   function GMT_Clock return Calendar.Time is
   begin
      return Local_To_GMT (Calendar.Clock);
   end GMT_Clock;

   -----------------
   -- Head_Before --
   -----------------

   function Head_Before (Source, Pattern : String) return String is
      Index : constant Natural := Strings.Fixed.Index (Source, Pattern);
   begin
      if Index = 0 then
         return Source;
      else
         return Source (Source'First .. Index - 1);
      end if;
   end Head_Before;

   ---------
   -- Hex --
   ---------

   function Hex (V : Natural; Width : Natural := 0) return Hex_String is
      use Strings;

      Hex_V : String (1 .. Integer'Size / 4 + 4);
   begin
      Integer_Text_IO.Put (Hex_V, V, 16);

      declare
         Result : constant String :=
                    Hex_V (Fixed.Index (Hex_V, "#") + 1
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

   function Hex_Value (Hex : Hex_String) return Natural is

      function Value (C : Character) return Natural with Inline;
      --  Return value for single character C

      -----------
      -- Value --
      -----------

      function Value (C : Character) return Natural is
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

      R : Natural := 0;

   begin
      for H of Hex loop
         R := R * 16 + Value (H);
      end loop;

      return R;
   end Hex_Value;

   -----------
   -- Image --
   -----------

   function Image (N : Natural) return String is
      N_Img : constant String := Natural'Image (N);
   begin
      return N_Img (N_Img'First + 1 .. N_Img'Last);
   end Image;

   -----------
   -- Image --
   -----------

   function Image (N : Stream_Element_Offset) return String is
      N_Img : constant String := Stream_Element_Offset'Image (N);
   begin
      if N_Img (N_Img'First) = ' ' then
         return N_Img (N_Img'First + 1 .. N_Img'Last);
      else
         return N_Img;
      end if;
   end Image;

   -----------
   -- Image --
   -----------

   function Image (D : Duration; Aft : Positive := 2) return String is
      D_Img : constant String  := Duration'Image (D);
      K     : constant Natural := Strings.Fixed.Index (D_Img, ".");
      Last  : constant Positive := Positive'Min (K + Aft, D_Img'Last);
   begin
      if K = 0 then
         return D_Img (D_Img'First + 1 .. D_Img'Last);
      else
         return D_Img (D_Img'First + 1 .. Last);
      end if;
   end Image;

   ------------------
   -- Is_Directory --
   ------------------

   function Is_Directory (Filename : String) return Boolean is
   begin
      return Directories.Exists (Filename)
        and then Directories.Kind (Filename) = Directories.Directory;
   end Is_Directory;

   ---------------
   -- Is_Number --
   ---------------

   function Is_Number
     (S              : String;
      Allow_Negative : Boolean := False) return Boolean
   is
      use Strings.Maps;
      Start : Positive := S'First;
   begin
      if Allow_Negative and then S'Length > 1 and then S (Start) = '-' then
         Start := S'First + 1;
      end if;

      return S'Length > 0
        and then Is_Subset
          (To_Set (S (Start .. S'Last)), Constants.Decimal_Digit_Set);
   end Is_Number;

   ---------------------
   -- Is_Regular_File --
   ---------------------

   function Is_Regular_File (Filename : String) return Boolean is
   begin
      return Directories.Exists (Filename)
        and then Directories.Kind (Filename) = Directories.Ordinary_File;
   end Is_Regular_File;

   ------------------------
   -- Is_Valid_HTTP_Date --
   ------------------------

   function Is_Valid_HTTP_Date (HTTP_Date : String) return Boolean is
      Mask   : constant String  := "Aaa, 99 Aaa 9999 99:99:99 GMT";
      Offset : constant Integer := HTTP_Date'First - 1;
      --  Make sure the function works for inputs with 'First <> 1
      Result : Boolean := True;
   begin
      for I in Mask'Range loop
         Result := I + Offset in HTTP_Date'Range;

         exit when not Result;

         case Mask (I) is
            when 'A' =>
               Result := HTTP_Date (I + Offset) in 'A' .. 'Z';

            when 'a' =>
               Result := HTTP_Date (I + Offset) in 'a' .. 'z';

            when '9' =>
               Result := HTTP_Date (I + Offset) in '0' .. '9';

            when others =>
               Result := Mask (I) = HTTP_Date (I + Offset);
         end case;
      end loop;

      return Result;
   end Is_Valid_HTTP_Date;

   -------------------
   -- Is_Valid_UTF8 --
   -------------------

   function Is_Valid_UTF8
     (Str : Aux.Big_String_Access; Last : Natural) return Boolean
   is
      --  Code points

      subtype CP1 is Character
        range Character'Val (16#00#) .. Character'Val (16#7F#);
      subtype CP2 is Character
        range Character'Val (16#C2#) .. Character'Val (16#DF#);
      subtype CP3 is Character
        range Character'Val (16#E0#) .. Character'Val (16#EF#);
      subtype CP4 is Character
        range Character'Val (16#F0#) .. Character'Val (16#F4#);

      C80 : constant Character := Character'Val (16#80#);
      C8F : constant Character := Character'Val (16#8F#);
      C90 : constant Character := Character'Val (16#90#);
      C9F : constant Character := Character'Val (16#9F#);
      CA0 : constant Character := Character'Val (16#A0#);
      CBF : constant Character := Character'Val (16#BF#);

      procedure Read_C
        (C              : in out Positive;
         Res            : in out Boolean;
         R_From, R_Last : Character)
      with Inline;
      --  Read a single character, check that it is in the range
      --  R_From .. R_Last and set Res accordingly. This routine moves C
      --  to the next character then.

      procedure Read_S2 (C : in out Positive; Res : in out Boolean)
        with Inline;
      --  Read an UTF-8 character with 2 bytes sequence

      procedure Read_S3 (C : in out Positive; Res : in out Boolean)
        with Inline;
      --  Read an UTF-8 character with 3 bytes sequence

      procedure Read_S4 (C : in out Positive; Res : in out Boolean)
        with Inline;
      --  Read an UTF-8 character with 4 bytes sequence

      ------------
      -- Read_C --
      ------------

      procedure Read_C
        (C              : in out Positive;
         Res            : in out Boolean;
         R_From, R_Last : Character) is
      begin
         if Res
           and then C < Last
           and then Str (C + 1) in R_From .. R_Last
         then
            C := C + 1;
            Res := True;

         else
            Res := False;
         end if;
      end Read_C;

      -------------
      -- Read_S2 --
      -------------

      procedure Read_S2 (C : in out Positive; Res : in out Boolean) is
      begin
         Read_C (C, Res, C80, CBF);
      end Read_S2;

      -------------
      -- Read_S3 --
      -------------

      procedure Read_S3 (C : in out Positive; Res : in out Boolean) is
         CE0 : constant Character := Character'Val (16#E0#);
         CED : constant Character := Character'Val (16#ED#);
      begin
         case Str (C) is
            when CE0 =>
               Read_C (C, Res, CA0, CBF);
            when CED =>
               Read_C (C, Res, C80, C9F);
            when others =>
               Read_C (C, Res, C80, CBF);
         end case;

         Read_C (C, Res, C80, CBF);
      end Read_S3;

      -------------
      -- Read_S4 --
      -------------

      procedure Read_S4 (C : in out Positive; Res : in out Boolean) is
         CF0 : constant Character := Character'Val (16#F0#);
         CF4 : constant Character := Character'Val (16#F4#);
      begin
         case Str (C) is
            when CF0 =>
               Read_C (C, Res, C90, CBF);
            when CF4 =>
               Read_C (C, Res, C80, C8F);
            when others =>
               Read_C (C, Res, C80, CBF);
         end case;

         Read_C (C, Res, C80, CBF);
         Read_C (C, Res, C80, CBF);
      end Read_S4;

      C : Positive := 1;
      R : Boolean := True;

   begin
      while R and then C <= Last loop
         case Str (C) is
            when CP1    => null;
            when CP2    => Read_S2 (C, R);
            when CP3    => Read_S3 (C, R);
            when CP4    => Read_S4 (C, R);
            when others => return False;
         end case;

         C := C + 1;
      end loop;

      return R;
   end Is_Valid_UTF8;

   function Is_Valid_UTF8 (Value : Unbounded_String) return Boolean is
      Str  : Aux.Big_String_Access;
      Last : Natural;
   begin
      Aux.Get_String (Value, Str, Last);
      return Is_Valid_UTF8 (Str, Last);
   end Is_Valid_UTF8;

   function Is_Valid_UTF8 (Value : String) return Boolean is
      X   : aliased Aux.Big_String;
      for X'Address use Value'Address;
      Str : constant Aux.Big_String_Access := X'Unchecked_Access;
   begin
      return Is_Valid_UTF8 (Str, Value'Last);
   end Is_Valid_UTF8;

   ------------------
   -- Local_To_GMT --
   ------------------

   function Local_To_GMT (DT : Calendar.Time) return Calendar.Time is
      use Ada.Calendar;
   begin
      return DT - Duration (Time_Zones.UTC_Time_Offset (DT)) * 60;
   end Local_To_GMT;

   -------------
   -- Mailbox --
   -------------

   package body Mailbox_G is

      protected body Mailbox is

         ---------
         -- Add --
         ---------

         entry Add (M : Message) when Current_Size < Max_Size is
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

         procedure Get (M : out Message; Success : out Boolean) is
         begin
            Success := Current_Size > 0;

            if not Success then
               return;
            end if;

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
   -- Match --
   -----------

   function Match (Str, Pattern : String) return Boolean is
      use Ada.Characters;
      U_Str     : constant String := Handling.To_Upper (Str);
      U_Pattern : constant String := Handling.To_Upper (Pattern);
   begin
      return Pattern'Length <= Str'Length
        and then U_Str (1 .. Pattern'Length) = U_Pattern;
   end Match;

   --------------------------
   -- Normalized_Directory --
   --------------------------

   function Normalized_Directory (Directory : String) return String is
   begin
      if Directory'Length = 0
        or else Directory (Directory'Last) = '/'
        or else Directory (Directory'Last) = '\'
      then
         return Directory;
      else
         return Directory & OS_Lib.Directory_Separator;
      end if;
   end Normalized_Directory;

   -----------
   -- Quote --
   -----------

   function Quote
     (Str : String; Replace : String := """") return String
   is
      function Replace_Quote (Source : String) return String;

      -------------------
      -- Replace_Quote --
      -------------------

      function Replace_Quote (Source : String) return String is
         use Ada.Strings.Fixed;
         Idx : Natural;
      begin
         Idx := Index (Source, """");

         if Idx = 0 then
            return Source;
         else
            return Source (Source'First .. Idx - 1) & Replace
              & Replace_Quote (Source (Idx + 1 .. Source'Last));
         end if;
      end Replace_Quote;

   begin
      if Replace'Length = 1 then
         --  When length of replace string is 1, we could replace it faster

         if Replace (Replace'First) = '"' then
            --  Do not need to replace
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

   -------------------
   -- Random_String --
   -------------------

   procedure Random_String (Item : out String) is
      Chars : constant String
        := "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
      Rand  : Random_Integer := 0;
   begin
      for Elem of Item loop
         if Rand = 0 then
            Rand := Random;
         end if;

         Elem := Chars (Integer (Rand rem Chars'Length) + 1);
         Rand := Rand / Chars'Length;
      end loop;
   end Random_String;

   function Random_String (Length : Natural) return String is
      Result : String (1 .. Length);
   begin
      Random_String (Result);
      return Result;
   end Random_String;

   ------------------
   -- RW_Semaphore --
   ------------------

   protected body RW_Semaphore is

      ----------
      -- Read --
      ----------

      entry Read when W = 0 and then RW_Semaphore.Write'Count = 0 is
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
         if Seized = 0 or else Seize'Caller = TID then
            Seized := Seized + 1;
            TID    := Seize'Caller;
         else
            requeue Seize_Internal with abort;
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

   function Significant_Image
     (Item : Duration; N : Positive) return String
   is
      type Largest_Integer is range System.Min_Int .. System.Max_Int;
      package LIO is new Text_IO.Integer_IO (Largest_Integer);
      package Duration_IO is new Text_IO.Fixed_IO (Duration);

      function Log_10 return Long_Float;

      AI : constant Long_Float := abs Long_Float (Item);

      ------------
      -- Log_10 --
      ------------

      function Log_10 return Long_Float is
      begin
         if Item = 0.0 then
            return 0.0;
         else
            return Numerics.Long_Elementary_Functions.Log (AI, 10.0);
         end if;
      end Log_10;

      L10  : constant Long_Float := Log_10;
      L10T : constant Long_Float := Long_Float'Truncation (L10);
      PP   : constant Integer    := Integer (L10T);
      Aft  : constant Natural :=
               Integer'Max
                 (N - PP - Boolean'Pos (AI >= 1.0 or else L10 = L10T), 0);
      Img  : String (1 .. Integer'Max (PP, 1) + Aft + 1 + Boolean'Pos (Aft > 0)
                          + Boolean'Pos (AI >= 10.0)
                          + Boolean'Pos (Item < 0.0));
   begin
      if Aft = 0 then
         LIO.Put (Img, Largest_Integer (Item));
      else
         Duration_IO.Put (Img, Item, Aft => Aft);
      end if;

      if Img (1) = ' ' then
         return Img (2 .. Img'Last);

      elsif Img (Img'Last) = '0'
        and then (Img (1) = '1' or else Img (1 .. 2) = "-1")
      then
         declare
            Last : Natural := Img'Last;
         begin
            if Aft > 0 then
               Last := Last - 1;

               if Img (Last) = '.' then
                  Last := Last - 1;
               end if;
            end if;

            return Img (1 .. Last);
         end;

      else
         raise Constraint_Error with
           Duration'Image (Item) & Integer'Image (N);
      end if;
   end Significant_Image;

   ------------------
   -- Test_And_Set --
   ------------------

   protected body Test_And_Set is

      ------------
      -- Locked --
      ------------

      function Locked return Boolean is
      begin
         return Flag;
      end Locked;

      --------------
      -- Try_Lock --
      --------------

      procedure Try_Lock (Succeeded : out Boolean) is
      begin
         Succeeded := not Flag;

         if Succeeded then
            Flag := True;
         end if;
      end Try_Lock;

      ------------
      -- Unlock --
      ------------

      procedure Unlock is
      begin
         Flag := False;
      end Unlock;

   end Test_And_Set;

   ---------------
   -- Time_Zone --
   ---------------

   function Time_Zone return Time_Zone_String is

      subtype String2 is String (1 .. 2);

      function Image2 (N : Natural) return String2;
      --  Returns N's image padded with a leading 0 if needed
      --
      ------------
      -- Image2 --
      ------------

      function Image2 (N : Natural) return String2 is
         Ni : constant String := Image (N);
      begin
         if Ni'Length = 2 then
            return Ni;
         else
            return '0' & Ni;
         end if;
      end Image2;

      use type Calendar.Time_Zones.Time_Offset;
      TZ   : constant Calendar.Time_Zones.Time_Offset :=
               Calendar.Time_Zones.UTC_Time_Offset;
      ATZ  : constant Calendar.Time_Zones.Time_Offset := abs TZ;
      H, M : Natural;
   begin
      if TZ = 0 then
         return "";

      else
         H := Natural (ATZ) / 60;
         M := Natural (ATZ) rem 60;

         if TZ >= 0 then
            return "+" & Image2 (H) & Image2 (M);
         else
            return "-" & Image2 (H) & Image2 (M);
         end if;
      end if;
   end Time_Zone;

begin
   Integer_Random.Reset (Random_Generator);
end AWS.Utils;
