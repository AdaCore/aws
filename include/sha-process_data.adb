--  (C) Copyright 1999 by John Halleck,All rights reserved.
--  Basic Transformation functions of NSA's Secure Hash Algorithm
--  This is part of a project at http://www.cc.utah.edu/~nahaj/

package body SHA.Process_Data is

   Default_Context : Context;  --  Standard context for people that don't need
   --                          --  to hash more than one stream at a time;

   ---------------------------------------------------------------------------
   --  Totally local functions.

   --  Raw transformation of the data.
   procedure Transform (Given : in out Context);
      --  This is the basic work horse of the standard.  Everything else here
      --  is just frame around this.

   --  Align data and place in buffer.  (Arbitrary chunks.)
   procedure Graft_On   (Given           : in out Context;
                         Raw             : Unsigned_32;
                         Size            : Bit_Index;
                         Increment_Count : Boolean := True);

   ---------------------------------------------------------------------------


   --  On with the show -----------------------------------------


   --  Quick and easy routine for the most common simple case.
   function Digest_A_String (Given : String)
            return Digest is
      Temp : Context; -- Let's make this totally independent of anything
      --              -- else the user may be doing.
      Result : Digest;
   begin
      Initialize (Temp);
      for I in Given'First .. Given'Last loop
         Add (Byte (Character'Pos (Given (I))), Temp);
      end loop;
      Finalize (Result, Temp);
      return Result;
   end Digest_A_String;


   --  Start out the buffer with a good starting state.
   --    Note that there are assumptions ALL over the code that the
   --    buffer starts out with zeros.
   procedure Initialize is
   begin
      if Default_Context.Initialized then
         raise SHA_Second_Initialize;
      end if;
      Default_Context := Initial_Value;
      Default_Context.Initialized := True;
   end Initialize;

   procedure Initialize (Given : in out Context) is
   begin
      if Given.Initialized then
         raise SHA_Second_Initialize;
      end if;
      Given := Initial_Value;
      Given.Initialized := True;
   end Initialize;


      --  Procedures to add to the data being hashed.

      procedure Add (Data  : Bit) is
      begin
         if not Default_Context.Initialized then
            raise SHA_Not_Initialized;
         end if;
         Graft_On (Default_Context, Unsigned_32 (Data), 1);
      end Add;

      procedure Add (Data  : Bit; Given : in out Context) is
      begin
         if not Given.Initialized then raise SHA_Not_Initialized; end if;
         Graft_On (Given, Unsigned_32 (Data), 1);
      end Add;


      procedure Add (Data  : Byte) is
      begin
         if not Default_Context.Initialized then
            raise SHA_Not_Initialized;
         end if;
         Graft_On (Default_Context, Unsigned_32 (Data), 8);
      end Add;

      procedure Add (Data  : Byte; Given : in out Context) is
      begin
         if not Given.Initialized then raise SHA_Not_Initialized; end if;
         Graft_On (Given, Unsigned_32 (Data), 8);
      end Add;



      procedure Add (Data  : Word) is
      begin
         if not Default_Context.Initialized then
            raise SHA_Not_Initialized;
         end if;
         Graft_On (Default_Context, Unsigned_32 (Data), 16);
      end Add;

      procedure Add (Data  : Word; Given : in out Context) is
      begin
         if not Given.Initialized then raise SHA_Not_Initialized; end if;
         Graft_On (Given, Unsigned_32 (Data), 16);
      end Add;


      procedure Add (Data  : Long) is
      begin
         if not Default_Context.Initialized then
            raise SHA_Not_Initialized;
         end if;
         Graft_On (Default_Context, Unsigned_32 (Data), 32);
      end Add;

      procedure Add (Data  : Long; Given : in out Context) is
      begin
         if not Given.Initialized then raise SHA_Not_Initialized; end if;
         Graft_On (Given, Unsigned_32 (Data), 32);
      end Add;


      procedure Add (Data  : Long;  Size : Bit_Index) is
      begin
         if not Default_Context.Initialized then
            raise SHA_Not_Initialized;
         end if;
         Graft_On (Default_Context, Unsigned_32 (Data), Size);
      end Add;

      procedure Add (Data  : Long; Size  : Bit_Index;
                     Given : in out Context) is
      begin
         if not Given.Initialized then raise SHA_Not_Initialized; end if;
         Graft_On (Given, Unsigned_32 (Data), Size);
      end Add;


   --  Get the final digest.

   function Finalize return Digest is
      Result : Digest;
   begin
      Finalize (Result, Default_Context);
      return Result;
   end Finalize;

   procedure Finalize (Result : out Digest) is
   begin
      Finalize (Result, Default_Context);
   end Finalize;

   procedure Finalize (Result : out Digest; Given : in out Context) is
   begin
      if not Given.Initialized then raise SHA_Not_Initialized; end if;

      --  The standard requires the Data be padded with a single 1 bit.
      Graft_On (Given, 1, 1, False);

      --  We may have to make room for the count to be put on the last block.
      if Given.Next_Word >= Given.Data'Last - 1 then -- Room for the count?
         if not (Given.Next_Word = Given.Data'Last - 1
                 and Given.Remaining_Bits = 32) then
            Transform (Given);
         end if;
      end if;

      --  Ok, now we can just add the count on.
      Given.Data (Given.Data'Last - 1) := Given.Count_High;
      Given.Data (Given.Data'Last)     := Given.Count_Low;

      --  And now we just transform that.
      Transform (Given);

      --  Ok, we are done.
      Given.Initialized := False;   --  One aught not to reused this without
                                    --  appropriate re-initialization.
      Result := Given.Current;

   end Finalize;

   ---------------------------------------------------------------------------
   --  Actually put the bits we have into the buffer properly aligned.

   procedure Graft_On (Given           : in out Context;
                       Raw             : Unsigned_32;
                       Size            : Bit_Index;
                       Increment_Count : Boolean := True) is

      Offset    : Integer range -31 .. 32; -- How far to move to align this?
      Overflow  : Bit_Index   := 0; -- How much is into the next word?
      Remainder : Unsigned_32 := 0; -- What data has to be done in cleanup?
      Value     : Unsigned_32 := Raw; -- What value are we Really working with?

   begin
      pragma Inline (Graft_On);

      --  Huh?
      if Size = 0 then return; end if;

      --  How do we have to align the data to fit?
      Offset := Integer (Given.Remaining_Bits) --  Amount used
              - Integer (Size);                --  Minus amount we have.

      if Offset > 0 then

         Value := Shift_Left (Value, Offset);

      elsif  Offset < 0 then

         Remainder := Shift_Left  (Value, 32 + Offset);
         --                         -- Really  "- -Offset"
         Value     := Shift_Right (Value, -Offset);
         Overflow  := Bit_Index (-Offset);

      end if;

      --  Insert the actual value into the table.
      Given.Data (Given.Next_Word) := Given.Data (Given.Next_Word) or Value;

      --  Update where we are in the table.
      if Offset > 0 then -- Not on a word boundry
         Given.Remaining_Bits := Given.Remaining_Bits - Size;
      elsif Given.Next_Word < Data_Buffer'Last then
         Given.Next_Word := Given.Next_Word + 1;
         Given.Remaining_Bits  := 32;
      else
         Transform (Given); -- Also clears everything out of the buffer.
      end if;

      --  Handle anything that overflows into the next word.
      if Overflow /= 0 then
         Given.Data (Given.Next_Word) := Given.Data (Given.Next_Word)
                                      or Remainder;
         Given.Remaining_Bits := 32 - Overflow;
      end if;

      if Increment_Count then
         Given.Count_Low := Given.Count_Low + Unsigned_32 (Size);
         if Given.Count_Low < Unsigned_32 (Size) then
            Given.Count_High := Given.Count_High + 1;
            if Given.Count_High = 0 then raise SHA_Overflow; end if;
            --  The standard is only defined up to a total size of what
            --  you are hashing of 2**64 bits.
         end if;
      end if;
   end Graft_On;

   ---------------------------------------------------------------------------
   --  The actual SHA transformation of a block of data.
   --  Yes, it is cryptic...  But it is a pretty much direct transliteration
   --  of the standard, variable names and all.

   procedure Transform (Given : in out Context) is
      Temp : Unsigned_32;

      --  Buffer to work in.
      type Work_Buffer        is array (0 .. 79) of Unsigned_32;
      W :  Work_Buffer;

      --  How much is filled from the data, how much is filled by expansion.
      Fill_Start : constant := Work_Buffer'First + Data_Buffer'Length;
      Data_End   : constant := Fill_Start - 1;

      A : Unsigned_32 := Given.Current (0);
      B : Unsigned_32 := Given.Current (1);
      C : Unsigned_32 := Given.Current (2);
      D : Unsigned_32 := Given.Current (3);
      E : Unsigned_32 := Given.Current (4);

   begin

      for I in Work_Buffer'First .. Data_End loop
         W (I) := Given.Data (Word_Range (I));
      end loop;

      for I in Fill_Start .. Work_Buffer'Last loop
         W (I) := Rotate_Left (
                     W (I - 3) xor W (I - 8) xor W (I - 14) xor W (I - 16),
                     1
                  );
      end loop;

      for I in Work_Buffer'Range loop
         Temp := W (I) + Rotate_Left (A, 5) + E;
         case I is
            when  0 .. 19 => Temp := Temp
                  + 16#5A827999#  +  ((B and C) or ((not B) and D));
            when 20 .. 39 => Temp := Temp
                  + 16#6ED9EBA1#  +  (B xor C xor D);
            when 40 .. 59 => Temp := Temp
                  + 16#8F1BBCDC#  +  ((B and C) or (B and D) or (C and D));
            when 60 .. 79 => Temp := Temp
                  + 16#CA62C1D6#  +  (B xor C xor D);
         end case;
         E := D;
         D := C;
         C := Rotate_Right (B, 2); --  The standard really says rotate left 30.
         B := A;
         A := Temp;
      end loop;

      Given.Current := (Given.Current (0) + A,
                        Given.Current (1) + B,
                        Given.Current (2) + C,
                        Given.Current (3) + D,
                        Given.Current (4) + E
                       );
      Given.Remaining_Bits  := 32;
      Given.Next_Word := 0;
      Given.Data      := (others => 0);  --  *THIS MUST BE DONE*
   end Transform;

end SHA.Process_Data;
