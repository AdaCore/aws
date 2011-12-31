------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2004-2012, AdaCore                     --
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

with System.Address_To_Access_Conversions;

with AWS.Net.Thin;
with G_Poll;

function Poll
  (Fds     : System.Address;
   Nfds    : AWS.OS_Lib.nfds_t;
   Timeout : C.int) return C.int
is
   use AWS;
   use AWS.Net;
   use type Interfaces.C.int;

   subtype Nfds_Range is OS_Lib.nfds_t range 1 .. Nfds;

   type FD_Array is array (Nfds_Range) of Thin.FD_Type;
   pragma Convention (C, FD_Array);

   type FD_Set_Type is record
      Count : C.int;
      Set   : FD_Array;
   end record;
   pragma Convention (C, FD_Set_Type);

   package Conversion is
     new System.Address_To_Access_Conversions (FD_Set_Type);

   procedure FD_ZERO (Set : System.Address);

   procedure FD_SET (FD : OS_Lib.FD_Type; Set : System.Address);

   ------------
   -- FD_SET --
   ------------

   procedure FD_SET (FD : Thin.FD_Type; Set : System.Address) is
      Sa : constant Conversion.Object_Pointer := Conversion.To_Pointer (Set);
   begin
      Sa.Count := Sa.Count + 1;
      Sa.Set (OS_Lib.nfds_t (Sa.Count)) := FD;
   end FD_SET;

   -------------
   -- FD_ZERO --
   -------------

   procedure FD_ZERO (Set : System.Address) is
      Sa : constant Conversion.Object_Pointer := Conversion.To_Pointer (Set);
   begin
      Sa.Count := 0;
   end FD_ZERO;

   function Win32_Poll is
     new G_Poll (FD_Set_Type, FD_ZERO, FD_SET, OS_Lib.FD_ISSET);

begin
   return Win32_Poll (Fds, Nfds, Timeout);
end Poll;
