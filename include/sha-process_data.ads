--  (C) Copyright 2000 by John Halleck, All rights reserved.
--  Basic Routines of NSA's Secure Hash Algorithm.
--  This is part of a project at http://www.cc.utah.edu/~nahaj/

package SHA.Process_Data is

   --  If you want to accumulate more than one hash at a time, then
   --  you'll need to specify a context for each accumulation.
   type Context is private;  -- Current state of the operation.

   --  What we can put in a buffer.
   type Bit  is mod 2 **  1;
   type Byte is mod 2 **  8;
   type Word is mod 2 ** 16;
   type Long is mod 2 ** 32;
   --  I know that this might not agree with the terminology of the
   --  underlying machine, but I had to call them something.

   Bytes_In_Block   : constant := 64;
   --  Strictly speaking this is an internal number and I'd never make it
   --  visible...  but the HMAC standard requires knowledge of it for each
   --  hash function, so I'm exporting it.


   type Bit_Index is new Natural range 0 .. Bits_In_Word;


   --  Exceptions we have:

   SHA_Not_Initialized   : exception;  --  Buffer given not initialized.

   SHA_Second_Initialize : exception;  --  Second call to initialize.
                                       --  without intervening finalize.

   SHA_Overflow          : exception;  --  Not defined for more than 2**64 bits
                                       --  (So says the standard.)
   --  I realize that some folk want to just ignore this exception.  While not
   --  strictly allowed by the standard, the standard doesn't give a way to
   --  get around the restriction.   ***  SO  ***   this exception is carefully
   --  NOT raised UNTIL the full processing of the input is done.   So it is
   --  perfectly safe to catch and ignore this exception.

   ----------------------------------------------------------------------------

   --  Most folk just want to Digest a string, so we will have this entry
   --  Point to keep it simple.
   function Digest_A_String (Given : String) return Digest;

   ---------------------------------------------------------------------------

   --  For those that want more control, we provide actual entry points.

   --  Start out the buffer.
   procedure Initialize;
   procedure Initialize (Given : in out Context);

         --  Procedures to add to the data being hashed. The standard really
         --  does define the hash in terms of bits.  So, in opposition to
         --  common practice, I'm providing routines --  that can process
         --  Bytes or Non_Bytes.
         --  I let you freely intermix the sizes, even if it means partial
         --  word alignment in the actual buffer.

         procedure Add (Data  : Bit);
         procedure Add (Data  : Bit; Given : in out Context);

         procedure Add (Data  : Byte);
         procedure Add (Data  : Byte; Given : in out Context);

         procedure Add (Data  : Word);
         procedure Add (Data  : Word; Given : in out Context);

         procedure Add (Data  : Long);
         procedure Add (Data  : Long; Given : in out Context);

         --  Add arbitrary sized data.
         procedure Add (Data  : Long; Size : Bit_Index);
         procedure Add (Data  : Long; Size : Bit_Index;
                        Given : in out Context);

   --  Get the final digest.
   function  Finalize return Digest;
   procedure Finalize (Result : out Digest);
   procedure Finalize (Result : out Digest; Given : in out Context);

------------------------------------------------------------------------------

private

--  I couldn't think of any advantage to letting people see the details of
--  these structures.  And some advantage to being able to change the count
--  into an Unsigned_64 on machines that support it.


   Initial_Context : constant Digest :=  -- Directly from the standard.
      (16#67452301#, 16#EFCDAB89#, 16#98BADCFE#, 16#10325476#, 16#C3D2E1F0#);

   Words_In_Buffer : constant := 16;
   type Word_Range  is new Natural range 0 .. Words_In_Buffer - 1;
   type Data_Buffer is array (Word_Range) of Unsigned_32;

   type Context is record
      Data           : Data_Buffer    := (others => 0);
      Count_High     : Unsigned_32    := 0;
      Count_Low      : Unsigned_32    := 0;
      Remaining_Bits : Bit_Index      := 32;
      Next_Word      : Word_Range     := 0;
      Current        : Digest         := Initial_Context;
      Initialized    : Boolean        := False;
   end record;

   Initial_Value : constant Context
                 := ((others => 0), 0, 0, 32, 0, Initial_Context, False);

end SHA.Process_Data;
