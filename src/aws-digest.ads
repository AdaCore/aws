------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2009, AdaCore                     --
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

--  Some utilities for HTTP digest authentication

with GNAT.MD5;

package AWS.Digest is

   subtype Digest_String is GNAT.MD5.Message_Digest;

   subtype Nonce is String (1 .. 40);
   --  4 bytes base64 seconds, 4 bytes base64 global counter, 32 bytes digest
   --  of the private key, creation time and global counter.

   function Create_Nonce return Nonce;
   --  Create a Nonce value for the digest authentication
   --  see [RFC-2617 - 3.2.1]

   function Check_Nonce (Value : String) return Boolean;
   --  Check Nonce for validity and expiration.
   --  We could not send a type Nonce here, because Nonce received from
   --  HTTP client, and have to checked for the length too.

   function Create
     (Username, Realm, Password : String;
      Nonce                     : String;
      Method, URI               : String) return Digest_String;
   --  Returns a simple MD5 Digest

   function Create
     (Username, Realm, Password : String;
      Nonce, NC, CNonce, QOP    : String;
      Method, URI               : String) return Digest_String;
   --  Returns a more complex MD5 Digest if QOP field is not empty

   function Tail
     (Nonce, NC, CNonce, QOP, Method, URI : String) return String;
   --  Returns the precalculated tail part of the digest
   --  if QOP field is not empty
   --     Tail := ':' & Nonce & ':' & NC & ':' & CNonce & ':' & QOP & ':'
   --               & MD5.Digest (Method & ':' & URI);
   --  otherwise
   --     Tail := ':' & Nonce & ':' & MD5.Digest (Method & ':' & URI);

end AWS.Digest;
