------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2004-2008, AdaCore                     --
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
