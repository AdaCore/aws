--  (C) Copyright 2000 by John Halleck, All Rights Reserved.
--  Convert SHA digests to printable strings.

package SHA.Strings is
   pragma Pure (SHA.Strings);

   --  Standard (Uppercase) Hex.
   type Hex_SHA_String is new String (1 .. (Bits_In_Digest / 8) * 2);

   --  Network Base 64 encoding. (See the Mime standard)
   type B64_SHA_String is new String (1 .. ((Bits_In_Digest / 8) + 2) / 3 * 4);

   --  And the routines to fill in the above.
   function Hex_From_SHA (Given : Digest) return Hex_SHA_String;
   function B64_From_SHA (Given : Digest) return B64_SHA_String;

end SHA.Strings;
