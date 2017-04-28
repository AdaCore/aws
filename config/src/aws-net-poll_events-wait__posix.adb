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

pragma Ada_2012;

with AWS.Net.Poll_Events.G_Poll;

--  Wait implementation on top of posix select call

separate (AWS.Net.Poll_Events)

procedure Wait
  (Fds : in out Set; Timeout : Timeout_Type; Result : out Integer)
is
   use Interfaces;
   use type OS_Lib.FD_Type;
   use type OS_Lib.nfds_t;

   type FD_Set_Type is array (0 .. Fds.Max_FD / C.long'Size) of C.long
     with Convention => C;

   procedure FD_ZERO (Set : in out FD_Set_Type);
   --  Use own FD_ZERO routine because FD_Set_Type size depend on Fds.Max_FD

   procedure FD_SET (FD : OS_Lib.FD_Type; Set : in out FD_Set_Type)
     with Import, Convention => C, External_Name => "__aws_set_socket_in_set";

   function FD_ISSET (FD : OS_Lib.FD_Type; Set : FD_Set_Type) return C.int
     with Import, Convention => C, External_Name => "__aws_is_socket_in_set";

   procedure FD_ZERO (Set : in out FD_Set_Type) is
   begin
      Set := (others => 0);
   end FD_ZERO;

   procedure Poll is new G_Poll (FD_Set_Type, FD_ZERO, FD_SET, FD_ISSET);

begin
   Poll (Fds, Timeout, Result);
end Wait;
