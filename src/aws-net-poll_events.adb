------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                            Copyright (C) 2006                            --
--                                 AdaCore                                  --
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

with Ada.Unchecked_Deallocation;

with AWS.OS_Lib;

package body AWS.Net.Poll_Events is

   procedure Set_Mode (Item : out Thin.Pollfd; Mode : in Wait_Event_Set);

   procedure Check_Range (FD_Set : in Set; Index : in Positive);
   pragma Inline (Check_Range);

   procedure Free is new
     Unchecked_Deallocation (Poll_Set, Poll_Set_Access);

   ---------
   -- Add --
   ---------

   procedure Add
     (FD_Set : in out Set;
      FD     : in     FD_Type;
      Event  : in     Wait_Event_Set) is
   begin
      if FD_Set.Length = FD_Set.Size then
         if FD_Set.Size < 256 then
            Reallocate (FD_Set, FD_Set.Size * 2);
         else
            Reallocate (FD_Set, FD_Set.Size + 256);
         end if;
      end if;

      FD_Set.Length := FD_Set.Length + 1;
      FD_Set.Fds (FD_Set.Length).FD := AWS.OS_Lib.FD_Type (FD);
      Set_Mode (FD_Set.Fds (FD_Set.Length), Event);
   end Add;

   -----------------
   -- Check_Range --
   -----------------

   procedure Check_Range (FD_Set : in Set; Index : in Positive) is
   begin
      if Index > FD_Set.Length then
         raise Constraint_Error;
      end if;
   end Check_Range;

   ----------
   -- Free --
   ----------

   procedure Free (FD_Set : in out Set) is
   begin
      Free (FD_Set.Fds);
   end Free;

   ------------
   -- Length --
   ------------

   function Length (FD_Set : in Set) return Natural is
   begin
      return FD_Set.Length;
   end Length;

   ----------
   -- Next --
   ----------

   procedure Next (FD_Set : in Set; Index : in out Positive) is
      use type Thin.Events_Type;
   begin
      loop
         exit when Index > FD_Set.Length
           or else FD_Set.Fds (Index).REvents /= 0;

         Index := Index + 1;
      end loop;
   end Next;

   ----------------
   -- Reallocate --
   ----------------

   procedure Reallocate
     (FD_Set : in out Set; Size : in Natural)
   is
      Result : Set (FD_Set.Initial_Size);
      Old    : Poll_Set_Access := FD_Set.Fds;
   begin
      if FD_Set.Size = Size then
         return;
      end if;

      if FD_Set.Size < Size then
         Result.Length := FD_Set.Length;
         Result.Fds := new Poll_Set (1 .. Size);
         Result.Fds (1 .. FD_Set.Size) := FD_Set.Fds.all;
         Free (Old);

      else
         Result.Length := Size;
         Result.Fds (1 .. Size) := FD_Set.Fds (1 .. Size);
      end if;

      Result.Size := Size;

      FD_Set := Result;
   end Reallocate;

   ------------
   -- Remove --
   ------------

   procedure Remove (FD_Set : in out Set; Index : in Positive) is
   begin
      Check_Range (FD_Set, Index);

      if Index < FD_Set.Length then
         FD_Set.Fds (Index) := FD_Set.Fds (FD_Set.Length);

      elsif Index > FD_Set.Length then
         raise Constraint_Error;
      end if;

      FD_Set.Length := FD_Set.Length - 1;
   end Remove;

   --------------
   -- Set_Mode --
   --------------

   procedure Set_Mode
     (FD_Set : in out Set; Index : in Positive; Mode : in Wait_Event_Set) is
   begin
      Check_Range (FD_Set, Index);
      Set_Mode (FD_Set.Fds (Index), Mode);
   end Set_Mode;

   procedure Set_Mode (Item : out Thin.Pollfd; Mode : in Wait_Event_Set) is
      use OS_Lib;
      use type Thin.Events_Type;
   begin
      if Mode (Input) then
         Item.Events := POLLIN or POLLPRI;
      else
         Item.Events := 0;
      end if;

      if Mode (Output) then
         Item.Events := Item.Events or POLLOUT;
      end if;
   end Set_Mode;

   ----------
   -- Size --
   ----------

   function Size (FD_Set : in Set) return Natural is
   begin
      return FD_Set.Size;
   end Size;

   ------------
   -- Status --
   ------------

   function Status
     (FD_Set : in Set; Index : in Positive) return Event_Set
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
     (FD_Set : in out Set; Timeout : in Duration; Count : out Natural)
   is
      use type Thin.Timeout_Type;

      Result       : Integer;
      Poll_Timeout : Thin.Timeout_Type;
   begin
      if FD_Set.Length = 0 then
         Count := 0;
         return;
      end if;

      if Timeout >= Duration (Thin.Timeout_Type'Last / 1_000) then
         Poll_Timeout := Thin.Timeout_Type'Last;
      else
         Poll_Timeout := Thin.Timeout_Type (Timeout * 1_000);
      end if;

      Result := Integer
        (Thin.Poll
           (FDS     => FD_Set.Fds.all'Address,
            Nfds    => Thin.nfds_t (FD_Set.Length),
            Timeout => Poll_Timeout));

      if Result < 0 then
         raise Socket_Error with "Poll error code" & Integer'Image (Errno);
      end if;

      Count := Result;
   end Wait;

end AWS.Net.Poll_Events;
