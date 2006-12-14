with AWS.OS_Lib;

package body AWS.Net.Poll_Events is

   procedure Set_Mode (Item : out Thin.Pollfd; Mode : in Wait_Event_Set);

   type Poll_Access is access all Set;

   procedure Check_Range (Container : in Set; Index : in Positive);
   pragma Inline (Check_Range);

   ---------
   -- Add --
   ---------

   procedure Add
     (Container : in out Set;
      FD        : in     FD_Type;
      Event     : in     Wait_Event_Set) is
   begin
      if Container.Size = Container.Length then
         raise Constraint_Error;
      end if;

      Container.Length := Container.Length + 1;
      Container.Fds (Container.Length).FD := AWS.OS_Lib.FD_Type (FD);
      Set_Mode (Container.Fds (Container.Length), Event);
   end Add;

   -----------------
   -- Check_Range --
   -----------------

   procedure Check_Range (Container : in Set; Index : in Positive) is
   begin
      if Index > Container.Length then
         raise Constraint_Error;
      end if;
   end Check_Range;

   ------------
   -- Length --
   ------------

   function Length (Container : in Set) return Natural is
   begin
      return Container.Length;
   end Length;

   ----------
   -- Next --
   ----------

   procedure Next (Container : in Set; Index : in out Positive) is
      use type Thin.Events_Type;
   begin
      loop
         exit when Index > Container.Length
           or else Container.Fds (Index).REvents /= 0;

         Index := Index + 1;
      end loop;
   end Next;

   ----------------
   -- Reallocate --
   ----------------

   function Reallocate
     (Container : access Set; Size : in Natural) return Set_Access
   is
      Result : Poll_Access;
      Old    : Set_Access := Container.all'Access;
   begin
      if Container.Size = Size then
         return Old;
      end if;

      Result := new Set (Size);

      if Container.Size < Size then
         Result.Length := Container.Length;
         Result.Fds (1 .. Container.Size) := Container.Fds;
      else
         Result.Length := Size;
         Result.Fds    := Container.Fds (1 .. Size);
      end if;

      Free (Old);
      return Result.all'Access;
   end Reallocate;

   ------------
   -- Remove --
   ------------

   procedure Remove (Container : in out Set; Index : in Positive) is
   begin
      Check_Range (Container, Index);

      if Index < Container.Length then
         Container.Fds (Index) := Container.Fds (Container.Length);
      elsif Index > Container.Length then
         raise Constraint_Error;
      end if;

      Container.Length := Container.Length - 1;
   end Remove;

   --------------
   -- Set_Mode --
   --------------

   procedure Set_Mode
     (Container : in out Set; Index : in Positive; Mode : in Wait_Event_Set) is
   begin
      Check_Range (Container, Index);
      Set_Mode (Container.Fds (Index), Mode);
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

   ------------
   -- Status --
   ------------

   function Status
     (Container : in Set; Index : in Positive) return Event_Set
   is
      use AWS.OS_Lib;
   begin
      Check_Range (Container, Index);
      return
       (Input  => (Container.Fds (Index).REvents and (POLLIN or POLLPRI))
                    /= 0,
        Error  => (Container.Fds (Index).REvents
                    and (POLLERR or POLLHUP or POLLNVAL)) /= 0,
        Output => (Container.Fds (Index).REvents and POLLOUT) /= 0);
   end Status;

   ----------
   -- Wait --
   ----------

   procedure Wait
     (Container : in out Set; Timeout : in Duration; Count : out Natural)
   is
      use type Thin.Timeout_Type;

      Result       : Integer;
      Poll_Timeout : Thin.Timeout_Type;
   begin
      if Container.Length = 0 then
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
           (FDS     => Container.Fds'Address,
            Nfds    => Thin.nfds_t (Container.Length),
            Timeout => Poll_Timeout));

      if Result < 0 then
         raise Socket_Error with "Poll error code" & Integer'Image (Errno);
      end if;

      Count := Result;
   end Wait;

end AWS.Net.Poll_Events;
