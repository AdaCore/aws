--  (C) Copyright 2000 by John Halleck, All rights reserved.
--  Basic Definitions for NSA's Secure Hash Algorithm.
--  This code implements SHA-1 defined in FIPS PUB 180-1, 17 April 1995.
--  It is part of a project at http://www.cc.utah.edu/~nahaj/

with Interfaces; use Interfaces;
   --  The only modular types for which rotation and shift are officially
   --  defined are those in Interfaces.
   --  We use Unsigned_32 for the digest, and Unsigned_8 in some of the
   --  computation routines.

package SHA is
   pragma Pure (SHA);

   --  At top level we define ONLY the digest, so that programs that
   --  store, compare, and manipulate the digest can be written, without
   --  them all having to drag in everything needed to compute it.

   --  The standard is written in terms of 32 bit words.
   Bits_In_Word     : constant := 32; -- Bits per Digest word.
   Words_In_Digest  : constant :=  5;
   Bits_In_Digest   : constant := Bits_In_Word * Words_In_Digest;

   type Digest is array (0 .. Words_In_Digest - 1) of Unsigned_32;

end SHA;
