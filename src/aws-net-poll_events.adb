------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2006-2009, AdaCore                     --
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

with AWS.OS_Lib;

package body AWS.Net.Poll_Events is

   procedure Set_Mode (Item : out Thin.Pollfd; Mode : Wait_Event_Set);

   type Poll_Access is access all Set;

   procedure Check_Range (FD_Set : Set; Index : Positive);
   pragma Inline (Check_Range);

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
      use type Thin.Events_Type;
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
   end Replace;

   --------------
   -- Set_Mode --
   --------------

   overriding procedure Set_Mode
     (FD_Set : in out Set; Index : Positive; Mode : Wait_Event_Set) is
   begin
      Check_Range (FD_Set, Index);
      Set_Mode (FD_Set.Fds (Index), Mode);
   end Set_Mode;

   procedure Set_Mode (Item : out Thin.Pollfd; Mode : Wait_Event_Set) is
      use OS_Lib;
   begin
      Item.REvents := 0;

      if Mode (Input) then
         Item.Events := POLLIN or POLLPRI;
      else
         Item.Events := 0;
      end if;

      if Mode (Output) then
         Item.Events := Item.Events or POLLOUT;
      end if;
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

   overriding procedure Wait
     (FD_Set : in out Set; Timeout : Duration; Count : out Natural)
   is
      use type Thin.Timeout_Type;

      Result       : Integer;
      Poll_Timeout : Thin.Timeout_Type;
      Errno        : Integer;
   begin
      if FD_Set.Length = 0 then
         Count := 0;
         return;
      end if;

      if Timeout >= Duration (Thin.Timeout_Type'Last - 8) / 1_000 then
         --  Minus 8 is to workaround Linux kernel 2.6.24 bug with close to
         --  Integer'Last poll timeout values.
         --  syscall (SYS_poll, &ufds, 1, 2147483644); // is waiting
         --  syscall (SYS_poll, &ufds, 1, 2147483645); // is not waiting
         --  Timeout values close to maximum could be not safe because of
         --  possible time conversion boundary errors in the kernel.
         --  Use unlimited timeout instead of maximum 24 days timeout for
         --  safety reasons.

         Poll_Timeout := -1;
      else
         Poll_Timeout := Thin.Timeout_Type (Timeout * 1_000);
      end if;

      loop
         Result := Integer
           (Thin.Poll
              (Fds     => FD_Set.Fds'Address,
               Nfds    => Thin.nfds_t (FD_Set.Length),
               Timeout => Poll_Timeout));

         if Result < 0 then
            Errno := AWS.OS_Lib.Socket_Errno;

            --  In case of EINTR error we have to continue waiting for network
            --  events.

            if Errno /= OS_Lib.EINTR then
               --  Call Raise_Socket_Error with dummy created socket and
               --  error code, to raise exception and log error message.

               Raise_Socket_Error
                 (AWS.Net.Socket (False),
                  "Poll (Size => " & Utils.Image (FD_Set.Length)
                  & ") error code" & Integer'Image (Errno));
            end if;
         else
            exit;
         end if;
      end loop;

      Count := Result;
   end Wait;

end AWS.Net.Poll_Events;
