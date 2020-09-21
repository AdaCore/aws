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

with Ada.Real_Time;
with Interfaces.C;

package body AWS.Net.Poll_Events is

   procedure Set_Mode (Item : out Pollfd; Mode : Wait_Event_Set);

   procedure Set_Event
     (Item : out Pollfd; Event : Wait_Event_Type; Value : Boolean);

   type Poll_Access is access all Set;

   procedure Check_Range (FD_Set : Set; Index : Positive) with Inline;

   procedure Wait
     (Fds : in out Set; Timeout : Timeout_Type; Result : out Integer);

   ---------
   -- Add --
   ---------

   overriding procedure Add
     (FD_Set : in out Set;
      FD     : FD_Type;
      Event  : Wait_Event_Set) is
   begin
      if FD_Set.Size = FD_Set.Length then
         raise Constraint_Error;
      end if;

      if FD < 0 then
         raise Socket_Error with
           "Wrong socket descriptor " & FD_Type'Image (FD);
      end if;

      if FD_Type (FD_Set.Max_FD) < FD then
         FD_Set.Max_FD := OS_Lib.FD_Type (FD);
      end if;

      FD_Set.Length := FD_Set.Length + 1;
      FD_Set.Fds (FD_Set.Length).FD := AWS.OS_Lib.FD_Type (FD);
      Set_Mode (FD_Set.Fds (FD_Set.Length), Event);
   end Add;

   -----------------
   -- Check_Range --
   -----------------

   procedure Check_Range (FD_Set : Set; Index : Positive) is
   begin
      if Index > FD_Set.Length then
         raise Constraint_Error;
      end if;
   end Check_Range;

   ----------
   -- Copy --
   ----------

   overriding function Copy
     (FD_Set : not null access Set; Size : Natural) return FD_Set_Access
   is
      use type OS_Lib.FD_Type;
      Result : Poll_Access;
   begin
      Result := new Set (Size);

      if FD_Set.Size < Size then
         --  New size is bigger
         Result.Length := FD_Set.Length;
         Result.Fds (1 .. FD_Set.Size) := FD_Set.Fds;

      else
         --  New size is smaller
         Result.Length := Size;
         Result.Fds    := FD_Set.Fds (1 .. Size);
      end if;

      for J in 1 .. Result.Length loop
         if Result.Max_FD < Result.Fds (J).FD then
            Result.Max_FD := Result.Fds (J).FD;
         end if;
      end loop;

      return Result.all'Access;
   end Copy;

   ------------
   -- Length --
   ------------

   overriding function Length (FD_Set : Set) return Natural is
   begin
      return FD_Set.Length;
   end Length;

   ----------
   -- Next --
   ----------

   overriding procedure Next (FD_Set : Set; Index : in out Positive) is
      use type OS_Lib.Events_Type;
   begin
      loop
         exit when Index > FD_Set.Length
           or else FD_Set.Fds (Index).REvents /= 0;

         Index := Index + 1;
      end loop;
   end Next;

   ------------
   -- Remove --
   ------------

   overriding procedure Remove (FD_Set : in out Set; Index : Positive) is
   begin
      Check_Range (FD_Set, Index);

      if Index < FD_Set.Length then
         FD_Set.Fds (Index) := FD_Set.Fds (FD_Set.Length);

      elsif Index > FD_Set.Length then
         raise Constraint_Error;
      end if;

      FD_Set.Length := FD_Set.Length - 1;
   end Remove;

   -------------
   -- Replace --
   -------------

   overriding procedure Replace
     (FD_Set : in out Set;
      Index  : Positive;
      FD     : FD_Type) is
   begin
      Check_Range (FD_Set, Index);
      FD_Set.Fds (Index).FD := OS_Lib.FD_Type (FD);

      if FD_Type (FD_Set.Max_FD) < FD then
         FD_Set.Max_FD := OS_Lib.FD_Type (FD);
      end if;
   end Replace;

   ---------------
   -- Set_Event --
   ---------------

   overriding procedure Set_Event
     (FD_Set : in out Set;
      Index  : Positive;
      Event  : Wait_Event_Type;
      Value  : Boolean) is
   begin
      Check_Range (FD_Set, Index);
      Set_Event (FD_Set.Fds (Index), Event, Value);
   end Set_Event;

   procedure Set_Event
     (Item : out Pollfd; Event : Wait_Event_Type; Value : Boolean)
   is
      use OS_Lib;
      To_C : constant array (Wait_Event_Type) of Events_Type :=
               (Input => POLLIN or POLLPRI, Output => POLLOUT);
   begin
      if Value then
         Item.Events := Item.Events or To_C (Event);
      else
         Item.Events := Item.Events and not To_C (Event);
      end if;
   end Set_Event;

   --------------
   -- Set_Mode --
   --------------

   overriding procedure Set_Mode
     (FD_Set : in out Set; Index : Positive; Mode : Wait_Event_Set) is
   begin
      Check_Range (FD_Set, Index);
      Set_Mode (FD_Set.Fds (Index), Mode);
   end Set_Mode;

   procedure Set_Mode (Item : out Pollfd; Mode : Wait_Event_Set) is
   begin
      for J in Mode'Range loop
         Set_Event (Item, J, Mode (J));
      end loop;
   end Set_Mode;

   ------------
   -- Status --
   ------------

   overriding function Status
     (FD_Set : Set; Index : Positive) return Event_Set
   is
      use AWS.OS_Lib;
   begin
      Check_Range (FD_Set, Index);
      return
       (Input  => (FD_Set.Fds (Index).REvents and (POLLIN or POLLPRI))
                    /= 0,
        Error  => (FD_Set.Fds (Index).REvents
                    and (POLLERR or POLLHUP or POLLNVAL)) /= 0,
        Output => (FD_Set.Fds (Index).REvents and POLLOUT) /= 0);
   end Status;

   ----------
   -- Wait --
   ----------

   procedure Wait
     (Fds : in out Set; Timeout : Timeout_Type; Result : out Integer)
   is separate;

   overriding procedure Wait
     (FD_Set : in out Set; Timeout : Duration; Count : out Natural)
   is
      use Ada.Real_Time;
      use type Timeout_Type;

      Result       : Integer;
      Poll_Timeout : Duration := Timeout;
      C_Timeout    : Timeout_Type;
      Errno        : Integer;
      Stamp        : constant Time := Clock;
   begin
      if FD_Set.Length = 0 then
         Count := 0;
         return;
      end if;

      loop
         if Poll_Timeout >= Duration (Timeout_Type'Last - 8) / 1_000 then
            --  Minus 8 is to workaround Linux kernel 2.6.24 bug with close to
            --  Integer'Last poll timeout values.
            --  syscall (SYS_poll, &ufds, 1, 2147483644); // is waiting
            --  syscall (SYS_poll, &ufds, 1, 2147483645); // is not waiting
            --  Timeout values close to maximum could be not safe because of
            --  possible time conversion boundary errors in the kernel.
            --  Use unlimited timeout instead of maximum 24 days timeout for
            --  safety reasons.

            C_Timeout := -1;
         else
            C_Timeout := Timeout_Type (Poll_Timeout * 1_000);
         end if;

         Wait (FD_Set, C_Timeout, Result);

         exit when Result >= 0;

         Errno := AWS.OS_Lib.Socket_Errno;

         --  In case of EINTR error we have to continue waiting for network
         --  events.

         if Errno = OS_Lib.EINTR then
            if C_Timeout >= 0 then
               Poll_Timeout := Timeout - To_Duration (Clock - Stamp);

               if Poll_Timeout < 0.0 then
                  Poll_Timeout := 0.0;
               end if;
            end if;

         else
            --  Call Raise_Socket_Error with dummy created socket and
            --  error code, to raise exception and log error message.

            Raise_Socket_Error
              (AWS.Net.Socket (False),
               "Poll (Size => " & Utils.Image (FD_Set.Length)
                 & ") error code" & Integer'Image (Errno));
         end if;
      end loop;

      Count := Result;
   end Wait;

end AWS.Net.Poll_Events;
