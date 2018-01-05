------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2017, AdaCore                     --
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

with Ada.Calendar.Formatting;
with Ada.Calendar.Time_Zones;

with Ada.Calendar;
with Ada.Characters.Handling;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Unchecked_Deallocation;

with Unicode.CES.Basic_8bit;
with Unicode.CES.Utf8;

with AWS.Utils;
with SOAP.Message.XML;

package body SOAP.Utils is

   ---------
   -- Any --
   ---------

   function Any
     (V         : Types.XSD_Any_Type;
      Name      : String := "item";
      Type_Name : String := Types.XML_String;
      NS        : Name_Space.Object := Name_Space.No_Name_Space)
      return Types.XSD_Any_Type is
   begin
      return SOAP.Types.Any (Types.Object'Class (V), Name, Type_Name, NS);
   end Any;

   -------
   -- C --
   -------

   function C
     (V         : Character;
      Name      : String := "item";
      Type_Name : String := "Character";
      NS        : Name_Space.Object := Name_Space.No_Name_Space)
      return Types.XSD_String is
   begin
      return Types.S (String'(1 => V), Name, Type_Name, NS);
   end C;

   ------------
   -- Encode --
   ------------

   procedure Encode
     (S      : Unbounded_String;
      Result : in out Unbounded_String)
   is
      Ch : Character;
   begin
      for K in 1 .. Length (S) loop
         Ch := Element (S, K);

         case Ch is
            when '<'    => Append (Result, "&lt;");
            when '>'    => Append (Result, "&gt;");
            when '&'    => Append (Result, "&amp;");
            when '''    => Append (Result, "&apos;");
            when '"'    => Append (Result, "&quot;");
            when Character'Val (0) .. Character'Val (31) =>
               Append (Result, "&#");
               Append (Result, AWS.Utils.Image (Natural (Character'Pos (Ch))));
               Append (Result, ';');
            when others => Append (Result, Ch);
         end case;
      end loop;
   end Encode;

   function Encode (Str : String) return String is
      Result : Unbounded_String;
   begin
      Encode (To_Unbounded_String (Str), Result);
      return To_String (Result);
   end Encode;

   ---------------
   -- From_Utf8 --
   ---------------

   function From_Utf8 (Str : String) return String is
   begin
      return Unicode.CES.Basic_8bit.From_Utf32
        (Unicode.CES.Utf8.To_Utf32 (Str));
   end From_Utf8;

   function From_Utf8 (Str : Unbounded_String) return Unbounded_String is
      Idx      : Integer := 1;
      Buf      : String (1 .. 6);
      Buf_Last : Integer := 0;
      Ch32     : Unicode.Unicode_Char;
      W        : Integer;
      Result   : Unbounded_String;
   begin
      loop
         while Idx <= Length (Str)
           and then Buf_Last < Buf'Last
         loop
            Buf (Buf_Last + 1) := Element (Str, Idx);
            Idx := Idx + 1;
            Buf_Last := Buf_Last + 1;
         end loop;

         exit when Buf_Last = 0;

         W := 1;
         Unicode.CES.Utf8.Read (Buf, W, Ch32);
         W := W - 1;

         for I in 1 .. Buf_Last - W loop
            Buf (I) := Buf (I + W);
         end loop;

         Buf_Last := Buf_Last - W;
         Append (Result, Character'Val (Ch32));
      end loop;

      return Result;
   end From_Utf8;

   function From_Utf8 (Str : String) return String_Access is

      Result : String_Access := new String (1 .. 2000);
      Last   : Integer := 0;

      procedure Append (Ch : Character) with Inline;
      --  Append Ch into Result, adjust Result size if needed

      procedure Adjust_Result with Inline;
      --  Adjust final Result to the right size

      -------------------
      -- Adjust_Result --
      -------------------

      procedure Adjust_Result is
         Old : String_Access;
      begin
         if Last /= Result'Last then
            Old := Result;
            Result := new String (1 .. Last);
            Result.all := Old (1 .. Last);
            Free (Old);
         end if;
      end Adjust_Result;

      ------------
      -- Append --
      ------------

      procedure Append (Ch : Character) is
         Old : String_Access;
      begin
         if Last >= Result'Last then
            Old := Result;
            Result := new String (1 .. Old'Last * 2);
            Result (1 .. Old'Last) := Old.all;
            Free (Old);
         end if;
         Last := Last + 1;
         Result (Last) := Ch;
      end Append;

      Idx      : Integer := Str'First;
      Buf      : String (1 .. 6);
      Buf_Last : Integer := 0;
      Ch32     : Unicode.Unicode_Char;
      W        : Integer;
   begin
      loop
         while Idx <= Str'Last and then Buf_Last < Buf'Last loop
            Buf (Buf_Last + 1) := Str (Idx);
            Idx := Idx + 1;
            Buf_Last := Buf_Last + 1;
         end loop;

         exit when Buf_Last = 0;

         W := 1;
         Unicode.CES.Utf8.Read (Buf, W, Ch32);
         W := W - 1;

         for I in 1 .. Buf_Last - W loop
            Buf (I) := Buf (I + W);
         end loop;

         Buf_Last := Buf_Last - W;
         Append (Character'Val (Ch32));
      end loop;

      Adjust_Result;
      return Result;
   end From_Utf8;

   ---------
   -- Get --
   ---------

   function Get (Item : Types.Object'Class) return Unbounded_String is
   begin
      return To_Unbounded_String (String'(Types.Get (Item)));
   end Get;

   function Get (Item : Types.Object'Class) return Character is
      Str : constant String := String'(Types.Get (Item));
   begin
      return Str (1);
   end Get;

   function Get (Item : Types.Object'Class) return String is
      Enum : constant Types.SOAP_Enumeration :=
               Types.SOAP_Enumeration (Item);
   begin
      return Types.Image (Enum);
   end Get;

   --------------------------
   -- Is_Ada_Reserved_Word --
   --------------------------

   function Is_Ada_Reserved_Word (Name : String) return Boolean is
      N : constant String := Ada.Characters.Handling.To_Lower (Name);
   begin
      if N in "abort" | "else" | "new" | "return" | "abs" | "elsif"
        | "not" | "reverse" | "abstract" | "end" | "null" | "accept"
        | "entry" | "select" | "access" | "exception" | "separate"
        | "aliased" | "exit" | "of" | "subtype" | "all" | "or"
        | "and" | "for" | "others" | "tagged" | "array"
        | "function" | "out" | "task" | "at" | "terminate" | "generic"
        | "package" | "then" | "begin" | "goto" | "pragma" | "type"
        | "body" | "private" | "if" | "procedure" | "case" | "in"
        | "protected" | "until" | "constant" | "is" | "use"
        | "raise" | "declare" | "range" | "when" | "delay" | "limited"
        | "record" | "while" | "delta" | "loop" | "rem" | "with"
        | "digits" | "renames" | "do" | "mod" | "requeue" | "xor"
        | "synchronized" | "overriding" | "interface" | "some"
      then
         return True;
      else
         return False;
      end if;
   end Is_Ada_Reserved_Word;

   -----------
   -- No_NS --
   -----------

   function No_NS (Name : String) return String is
      K : constant Natural := Ada.Strings.Fixed.Index (Name, ":");
   begin
      if K = 0 then
         return Name;
      else
         return Name (K + 1 .. Name'Last);
      end if;
   end No_NS;

   --------
   -- NS --
   --------

   function NS (Name : String) return String is
      K : constant Natural := Ada.Strings.Fixed.Index (Name, ":");
   begin
      if K = 0 then
         return "";
      else
         return Name (Name'First .. K - 1);
      end if;
   end NS;

   -------------------
   -- Safe_Pointers --
   -------------------

   package body Safe_Pointers is

      procedure Unchecked_Free is
        new Ada.Unchecked_Deallocation (T, T_Access);

      procedure Unchecked_Free is
        new Ada.Unchecked_Deallocation (Natural, Ref_Counter);

      ------------
      -- Adjust --
      ------------

      overriding procedure Adjust (SP : in out Safe_Pointer) is
      begin
         SP.Ref.all := SP.Ref.all + 1;
      end Adjust;

      --------------
      -- Finalize --
      --------------

      overriding procedure Finalize (SP : in out Safe_Pointer) is
         Ref : Ref_Counter := SP.Ref;
      begin
         --  Ensure call is idempotent

         SP.Ref := null;

         if Ref /= null then
            Ref.all := Ref.all - 1;

            if Ref.all = 0 then
               Unchecked_Free (SP.Item);
               Unchecked_Free (Ref);
            end if;
         end if;
      end Finalize;

      ----------------
      -- Initialize --
      ----------------

      overriding procedure Initialize (SP : in out Safe_Pointer) is
      begin
         SP.Ref := new Natural'(1);
      end Initialize;

      ----------------------
      -- To_Safe_Pointer --
      ----------------------

      function To_Safe_Pointer (Item : T) return Safe_Pointer is
      begin
         return (Ada.Finalization.Controlled
                 with new T'(Item), new Natural'(1));
      end To_Safe_Pointer;

   end Safe_Pointers;

   ------------------
   -- SOAP_Wrapper --
   ------------------

   function SOAP_Wrapper
     (Request : AWS.Status.Data;
      Schema  : WSDL.Schema.Definition := WSDL.Schema.Empty)
      return AWS.Response.Data
   is
      SOAPAction : constant String := AWS.Status.SOAPAction (Request);
   begin
      if SOAPAction /= No_SOAPAction then
         declare
            Payload_Data : constant Unbounded_String :=
                             AWS.Status.Payload (Request);
            Payload      : constant Message.Payload.Object :=
                             Message.XML.Load_Payload
                               (Payload_Data, Schema => Schema);
         begin
            return SOAP_CB (SOAPAction, Payload, Request);
         end;

      else
         raise Constraint_Error;
      end if;
   end SOAP_Wrapper;

   ---------
   -- Tag --
   ---------

   function Tag (Name : String; Start : Boolean) return String is
   begin
      if Start then
         return '<' & Name & '>';
      else
         return "</" & Name & '>';
      end if;
   end Tag;

   ------------------
   -- Time_Instant --
   ------------------

   function Time_Instant
     (TI, Name  : String;
      Type_Name : String := Types.XML_Time_Instant)
      return Types.XSD_Time_Instant
   is
      use Ada.Calendar;
      use Ada.Calendar.Formatting;
      use type Ada.Calendar.Time_Zones.Time_Offset;

      subtype Year_Range      is Positive range TI'First      .. TI'First + 3;
      subtype Month_Range     is Positive range TI'First + 5  .. TI'First + 6;
      subtype Day_Range       is Positive range TI'First + 8  .. TI'First + 9;
      subtype Hour_Range      is Positive range TI'First + 11 .. TI'First + 12;
      subtype Minute_Range    is Positive range TI'First + 14 .. TI'First + 15;
      subtype Second_Range    is Positive range TI'First + 17 .. TI'First + 18;
      subtype TZ_Type_Range   is Positive range TI'First + 19 .. TI'First + 19;
      subtype TZ_Hour_Range   is Positive range TI'First + 20 .. TI'First + 21;
      subtype TZ_Minute_Range is Positive range TI'First + 23 .. TI'First + 24;

      T  : Types.Local_Time;
      TZ : Time_Zones.Time_Offset := 0;
   begin
      --  timeInstant format is CCYY-MM-DDThh:mm:ss[[+|-]hh:mm | Z]

      if TI'Last >= TZ_Type_Range'Last then
         --  Time zone specified

         if TI'Last >= TZ_Hour_Range'Last then
            TZ := Time_Zones.Time_Offset'Value (TI (TZ_Hour_Range)) * 60;

            if TI'Last = TZ_Minute_Range'Last then
               TZ := TZ + Time_Zones.Time_Offset'Value (TI (TZ_Minute_Range));
            end if;

            if TI (TZ_Type_Range) = "-" then
               TZ := -TZ;
            end if;
         end if;
      end if;
 
      T := Time_Of (Year      => Year_Number'Value (TI (Year_Range)),
                    Month     => Month_Number'Value (TI (Month_Range)),
                    Day       => Day_Number'Value (TI (Day_Range)),
                    Hour      => Hour_Number'Value (TI (Hour_Range)),
                    Minute    => Minute_Number'Value (TI (Minute_Range)),
                    Second    => Second_Number'Value (TI (Second_Range)),
                    Time_Zone => TZ);

      return Types.T (T, Name, Type_Name => Type_Name);
   end Time_Instant;

   -------------
   -- To_Name --
   -------------

   function To_Name (Q_Name : String) return String is
      use Ada;
   begin
      return Strings.Fixed.Translate
        (Q_Name, Strings.Maps.To_Mapping (":", "_"));
   end To_Name;

   -------------------
   -- To_Object_Set --
   -------------------

   function To_Object_Set
     (From : T_Array;
      NS   : Name_Space.Object) return Types.Object_Set
   is
      use SOAP.Types;
      Result : Types.Object_Set (From'Range);
   begin
      for K in From'Range loop
         Result (K) := +Get (From (K), Type_Name => Type_Name, NS => NS);
      end loop;

      return Result;
   end To_Object_Set;

   ---------------------
   -- To_Object_Set_C --
   ---------------------

   function To_Object_Set_C
     (From : T_Array;
      NS   : Name_Space.Object) return Types.Object_Set
   is
      use SOAP.Types;
      Result : Types.Object_Set (1 .. Integer (From'Last));
   begin
      for K in From'Range loop
         Result (Integer (K)) :=
           +Get (From (K), Type_Name => Type_Name, NS => NS);
      end loop;

      return Result;
   end To_Object_Set_C;

   ----------------
   -- To_T_Array --
   ----------------

   function To_T_Array (From : Types.Object_Set) return T_Array is
      use SOAP.Types;
      Result : T_Array (From'Range);
   begin
      for K in From'Range loop
         Result (K) := Get (-From (K));
      end loop;

      return T_Array'(Result);
   end To_T_Array;

   ------------------
   -- To_T_Array_C --
   ------------------

   function To_T_Array_C (From : Types.Object_Set) return T_Array is
      use SOAP.Types;
      Result : T_Array;
   begin
      for K in Result'Range loop
         Result (K) := Get (-From (Integer (K)));
      end loop;

      return Result;
   end To_T_Array_C;

   -------------
   -- To_Utf8 --
   -------------

   function To_Utf8 (Str : String) return String is
   begin
      return Unicode.CES.Utf8.From_Utf32
        (Unicode.CES.Basic_8bit.To_Utf32 (Str));
   end To_Utf8;

   function To_Utf8 (Str : Unbounded_String) return Unbounded_String is
      Chars : String (1 .. 6);
      Idx : Integer;
      Result : Unbounded_String;
   begin
      for I in 1 .. Length (Str) loop
         Idx := 0;
         Unicode.CES.Utf8.Encode
            (Character'Pos (Element (Str, I)), Chars, Idx);
         Append (Result, Chars (1 .. Idx));
      end loop;
      return Result;
   end To_Utf8;

   --------
   -- US --
   --------

   function US
     (V         : Unbounded_String;
      Name      : String := "item";
      Type_Name : String := Types.XML_String;
      NS        : Name_Space.Object := Name_Space.No_Name_Space)
      return Types.XSD_String is
   begin
      return Types.S (To_String (V), Name, Type_Name, NS);
   end US;

   -------
   -- V --
   -------

   function V (O : Types.XSD_String) return Unbounded_String is
   begin
      return To_Unbounded_String (Types.V (O));
   end V;

   function V (O : Types.XSD_String) return Character is
   begin
      return Types.V (O) (1);
   end V;

   -------------
   -- With_NS --
   -------------

   function With_NS (NS, Name : String) return String is
      use Ada;
      K : Natural;
   begin
      if NS = "" then
         return Name;

      else
         K := Strings.Fixed.Index (Name, ":");

         if K = 0 then
            K := Name'First;
         else
            K := K + 1;
         end if;

         return NS & ':' & Name (K .. Name'Last);
      end if;
   end With_NS;

end SOAP.Utils;
