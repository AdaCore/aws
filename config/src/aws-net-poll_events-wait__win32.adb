------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2004-2017, AdaCore                     --
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

--  Wait implementation on top of windows select call

pragma Ada_2012;

with AWS.Net.Poll_Events.G_Poll;

separate (AWS.Net.Poll_Events)

procedure Wait
  (Fds : in out Set; Timeout : Timeout_Type; Result : out Integer)
is
   use Interfaces;
   use type C.int;

   type FD_Array is array (1 .. Fds.Length) of OS_Lib.FD_Type
     with Convention => C;

   type FD_Set_Type is record
      Count : C.int;
      Set   : FD_Array;
   end record with Convention => C;

   procedure FD_ZERO (Set : in out FD_Set_Type) with Inline;

   procedure FD_SET (FD : OS_Lib.FD_Type; Set : in out FD_Set_Type)
     with Inline;

   function FD_ISSET (FD : OS_Lib.FD_Type; Set : FD_Set_Type) return C.int
     with Import, Convention => Stdcall, External_Name => "__WSAFDIsSet";

   ------------
   -- FD_SET --
   ------------

   procedure FD_SET (FD : OS_Lib.FD_Type; Set : in out FD_Set_Type) is
   begin
      Set.Count := Set.Count + 1;
      Set.Set (Integer (Set.Count)) := FD;
   end FD_SET;

   -------------
   -- FD_ZERO --
   -------------

   procedure FD_ZERO (Set : in out FD_Set_Type) is
   begin
      Set.Count := 0;
   end FD_ZERO;

   procedure Poll is new G_Poll (FD_Set_Type, FD_ZERO, FD_SET, FD_ISSET);

begin
   Poll (Fds, Timeout, Result);
end Wait;
