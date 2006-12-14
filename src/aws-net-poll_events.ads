with AWS.Net.Thin;

package AWS.Net.Poll_Events is

   type Set (Size : Natural) is new FD_Set with private;

   overriding procedure Add
     (Container : in out Set;
      FD        : in     FD_Type;
      Event     : in     Wait_Event_Set);
   --  Add the FD to the end of FD set

   overriding procedure Set_Mode
     (Container : in out Set; Index : in Positive; Mode : in Wait_Event_Set);
   --  Sets what kind of network events would be waiting for in next Wait call

   overriding function Reallocate
     (Container : access Set; Size : in Natural) return Set_Access;
   --  Reallocates the set, remain data unchanged where possible

   overriding procedure Remove (Container : in out Set; Index : in Positive);
   --  Remove socket FD from Index position.
   --  Last socket FD would be placed in place of removed.

   overriding function Length (Container : in Set) return Natural;
   --  Returns number of socket FD elements in set

   overriding procedure Wait
     (Container : in out Set; Timeout : in Duration; Count : out Natural);
   --  Wait for network event on the sockets FD set. Count value would be
   --  the number of socket FDs with non empty event set.

   overriding procedure Next (Container : in Set; Index : in out Positive);
   --  Looking for active socket FD starting from Index and return Index of the
   --  found active socket FD. After search use functions Status to see what
   --  kind of network events happen on this socket FD.

   overriding function Status
     (Container : in Set; Index : in Positive) return Event_Set;
   --  Returns events happening on the socket FD at the specified index

private

   type Poll_Set is array (Positive range <>) of Thin.Pollfd;
   pragma Pack (Poll_Set);

   type Set (Size : Natural) is new FD_Set (Size) with record
      Length : Natural := 0;
      Fds    : Poll_Set (1 .. Size);
   end record;

end AWS.Net.Poll_Events;
