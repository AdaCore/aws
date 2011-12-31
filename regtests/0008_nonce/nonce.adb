------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2004-2012, AdaCore                     --
--                                                                          --
--  This is free software;  you can redistribute it  and/or modify it       --
--  under terms of the  GNU General Public License as published  by the     --
--  Free Software  Foundation;  either version 3,  or (at your option) any  --
--  later version.  This software is distributed in the hope  that it will  --
--  be useful, but WITHOUT ANY WARRANTY;  without even the implied warranty --
--  of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU     --
--  General Public License for  more details.                               --
--                                                                          --
--  You should have  received  a copy of the GNU General  Public  License   --
--  distributed  with  this  software;   see  file COPYING3.  If not, go    --
--  to http://www.gnu.org/licenses for a complete copy of the license.      --
------------------------------------------------------------------------------

with Ada.Text_IO;
with AWS.Digest;

with Ada.Containers.Vectors;
with Nvect;

procedure Nonce is

   use Ada.Text_IO;
   use AWS.Digest;
   use Nvect;

   N_Tests : constant := 200_000;

   package Sort_Nvect is new Nvect.Generic_Sorting;

   Nonces : Nvect.Vector;
   Result : Natural := 0;

begin
   for K in 1 .. N_Tests loop
      Append (Nonces, Create_Nonce);
   end loop;

   Sort_Nvect.Sort (Nonces);

   for K in 1 .. N_Tests - 1 loop
      if Element (Nonces, K) = Element (Nonces, K + 1) then
         Result := Result + 1;
      end if;
   end loop;

   if Result = 0 then
      Put_Line ("OK, no duplicate found");
   else
      Put_Line ("Error: found the same nonce "
                  & Natural'Image (Result) & " times.");
   end if;
end Nonce;
