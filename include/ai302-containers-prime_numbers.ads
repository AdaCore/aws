------------------------------------------------------------------------------
--                                                                          --
--                   AI-302 Reference Implementation                        --
--                                                                          --
--              Copyright (C) 2003-2004 Matthew J Heaney                    --
--                                                                          --
-- The AI-302 Reference Implementation is free software; you can            --
-- redistribute it and/or modify it under terms of the GNU General Public   --
-- License as published by the Free Software Foundation; either version 2,  --
-- or (at your option) any later version.  Charles is distributed in the    --
-- hope that it will be useful, but WITHOUT ANY WARRANTY; without even the  --
-- implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. --
-- See the GNU General Public License for more details.  You should have    --
-- received a copy of the GNU General Public License distributed with       --
-- Charles;  see file COPYING.TXT.  If not, write to the Free Software      --
-- Foundation,  59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.    --
--                                                                          --
-- As a special exception, if other files instantiate generics from this    --
-- unit, or you link this unit with other files to produce an executable,   --
-- this unit does not by itself cause the resulting executable to be        --
-- covered by the GNU General Public License.  This exception does not      --
-- however invalidate any other reasons why the executable file might be    --
-- covered by the GNU Public License.                                       --
--                                                                          --
-- The AI-302 Reference Implementation is maintained by Matthew J Heaney.   --
--                                                                          --
-- mailto:matthewjheaney@earthlink.net                                      --
-- http://home.earthlink.net/~matthewjheaney/index.html                     --
--                                                                          --
------------------------------------------------------------------------------

package AI302.Containers.Prime_Numbers is
   pragma Pure (Prime_Numbers);

   type Primes_Type is array (Positive range <>) of Hash_Type;

   --NOTE:
   --I got these numbers from the SGI STL sources.  I don't understand
   --how A.S. came up with these values.  Is he trying to stay way from
   --powers of 2 maybe?
   --END NOTE.

   Primes : constant Primes_Type :=
     (5, 7, 11, 13, 17, 19, 23, 31, --for debugging
      53,         97,         193,       389,       769,
      1543,       3079,       6151,      12289,     24593,
      49157,      98317,      196613,    393241,    786433,
      1572869,    3145739,    6291469,   12582917,  25165843,
      50331653,   100663319,  201326611, 402653189, 805306457,
      1610612741, 3221225473, 4294967291);

   function To_Prime (Length : Count_Type) return Hash_Type;

end AI302.Containers.Prime_Numbers;
