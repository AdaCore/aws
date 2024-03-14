------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2006-2017, AdaCore                     --
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

private with AWS.OS_Lib;

package AWS.Net.Poll_Events is

   Socket_Error : exception renames Net.Socket_Error;

   type Set (Size : Natural) is new FD_Set with private;

   overriding procedure Add
     (FD_Set : in out Set;
      FD     : FD_Type;
      Event  : Wait_Event_Set);

   overriding procedure Replace
     (FD_Set : in out Set;
      Index  : Positive;
      FD     : FD_Type);

   overriding procedure Set_Mode
     (FD_Set : in out Set; Index : Positive; Mode : Wait_Event_Set);

   overriding procedure Set_Event
     (FD_Set : in out Set;
      Index  : Positive;
      Event  : Wait_Event_Type;
      Value  : Boolean);

   overriding function Copy
     (FD_Set : not null access Set; Size : Natural) return FD_Set_Access;

   overriding procedure Remove (FD_Set : in out Set; Index : Positive);

   overriding function Length (FD_Set : Set) return Natural;

   overriding procedure Wait
     (FD_Set : in out Set; Timeout : Duration; Count : out Natural);

   overriding procedure Next (FD_Set : Set; Index : in out Positive);

   overriding function Status
     (FD_Set : Set; Index : Positive) return Event_Set;

private

   subtype Timeout_Type is Interfaces.C.int;

   type Pollfd is record
      FD      : OS_Lib.FD_Type;
      Events  : OS_Lib.Events_Type := 0;
      REvents : OS_Lib.Events_Type := 0;
   end record with Convention => C;

   type Poll_Set is array (Positive range <>) of Pollfd with Convention => C;

   type Set (Size : Natural) is new FD_Set (Size) with record
      Length : Natural := 0;
      Fds    : Poll_Set (1 .. Size);
      Max_FD : OS_Lib.FD_Type := 0;
   end record;

end AWS.Net.Poll_Events;
