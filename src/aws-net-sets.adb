------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                            Copyright (C) 2004                            --
--                                ACT-Europe                                 --
--                                                                          --
--  Authors: Dmitriy Anisimkov - Pascal Obry                                --
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

--  $Id$

with Ada.Exceptions;
with Ada.Unchecked_Deallocation;

with AWS.Net.Sets.Thin;

package body AWS.Net.Sets is

   type Poll_Set_Type is array (Positive range <>) of Thin.Pollfd;
   pragma Pack (Poll_Set_Type);

   procedure Free is
     new Ada.Unchecked_Deallocation (Poll_Set_Type, Poll_Set_Access);

   procedure Free is
     new Ada.Unchecked_Deallocation (Socket_Array, Socket_Array_Access);

   procedure Free is
     new Ada.Unchecked_Deallocation (Socket_Type'Class, Socket_Access);
      --  We could not use AWS.Net.Free because Socket_Set_Type did not
      --  allocate internal socket data.

   function To_State (Event : in Thin.Events_Type) return Socket_State;
   --  Convert Event to the proper Socket_State

   procedure Add_Private
     (Set    : in out Socket_Set_Type;
      Socket : in     Socket_Access;
      Mode   : in     Waiting_Mode);
   --  Add Socket into Set

   procedure Next_Private (Set : in out Socket_Set_Type);
   --  Looking for next active sockets beginning from current

   ---------
   -- Add --
   ---------

   procedure Add
     (Set    : in out Socket_Set_Type;
      Socket : in     Socket_Type'Class;
      Mode   : in     Waiting_Mode) is
   begin
      Add_Private (Set, new Socket_Type'Class'(Socket), Mode);
      Set.Set (Set.Last).Allocated := True;
   end Add;

   procedure Add
     (Set    : in out Socket_Set_Type;
      Socket : in     Socket_Access;
      Mode   : in     Waiting_Mode) is
   begin
      Add_Private (Set, Socket, Mode);
      Set.Set (Set.Last).Allocated := False;
   end Add;

   -----------------
   -- Add_Private --
   -----------------

   procedure Add_Private
     (Set    : in out Socket_Set_Type;
      Socket : in     Socket_Access;
      Mode   : in     Waiting_Mode)
   is
      use type Thin.Events_Type;
   begin
      if Set.Poll = null then
         if Set.Last /= 0 then
            raise Constraint_Error;
         end if;

         if Set.Set /= null then
            raise Constraint_Error;
         end if;

         --  Allocate only few elements in array first, because this package
         --  often would be used for wait just one socket.

         Set.Poll := new Poll_Set_Type (1 .. 4);
         Set.Set  := new Socket_Array (Set.Poll'Range);

      elsif Set.Last >= Set.Poll'Length then
         declare
            Prev_Set  : Socket_Array_Access := Set.Set;
            Prev_Poll : Poll_Set_Access     := Set.Poll;
            Increment : Positive;

         begin
            if Set.Last < 256 then
               Increment := Set.Last;
            else
               Increment := 256;
            end if;

            Set.Poll := new Poll_Set_Type (1 .. Set.Last + Increment);
            Set.Set  := new Socket_Array (Set.Poll'Range);

            Set.Poll (Prev_Poll'Range) := Prev_Poll.all;
            Set.Set  (Prev_Set'Range)  := Prev_Set.all;

            Free (Prev_Set);
            Free (Prev_Poll);
         end;
      end if;

      Set.Last := Set.Last + 1;

      Set.Set (Set.Last).Socket := Socket;
      Set.Poll (Set.Last).FD := Thin.FD_Type (Get_FD (Socket.all));

      case Mode is
         when Input  =>
            Set.Poll (Set.Last).Events := Thin.Pollin;
         when Output =>
            Set.Poll (Set.Last).Events := Thin.Pollout;
         when Both   =>
            Set.Poll (Set.Last).Events := Thin.Pollin + Thin.Pollout;
      end case;
   end Add_Private;

   -----------
   -- Count --
   -----------

   function Count (Set : Socket_Set_Type) return Natural is
   begin
      return Set.Last;
   end Count;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Set : in out Socket_Set_Type) is
   begin
      Free (Set.Set);
      Free (Set.Poll);
   end Finalize;

   ----------------
   -- Get_Socket --
   ----------------

   function Get_Socket (Set : in Socket_Set_Type) return Socket_Type'Class is
   begin
      if Set.Current > Set.Last then
         raise Constraint_Error;
      else
         return Set.Set (Set.Current).Socket.all;
      end if;
   end Get_Socket;

   ----------------------
   -- Get_Socket_State --
   ----------------------

   function Get_Socket_State (Set : in Socket_Set_Type) return Socket_State is
   begin
      if Set.Current > Set.Last then
         return None;
      else
         return To_State (Set.Poll (Set.Current).REvents);
      end if;
   end Get_Socket_State;

   ----------
   -- Next --
   ----------

   procedure Next (Set : in out Socket_Set_Type) is
      use type Thin.Events_Type;
   begin
      loop
         Set.Current := Set.Current + 1;

         exit when Set.Current > Set.Last
           or else Set.Poll (Set.Current).REvents /= 0;
      end loop;
   end Next;

   ------------------
   -- Next_Private --
   ------------------

   procedure Next_Private (Set : in out Socket_Set_Type) is
      use type Thin.Events_Type;
   begin
      while Set.Current <= Set.Last
        and then Set.Poll (Set.Current).REvents = 0
      loop
         Set.Current := Set.Current + 1;
      end loop;
   end Next_Private;

   -------------------
   -- Remove_Socket --
   -------------------

   procedure Remove_Socket (Set : in out Socket_Set_Type) is
   begin
      if Set.Current > Set.Last then
         raise Constraint_Error;
      end if;

      if Set.Set (Set.Current).Allocated then
         Sets.Free (Set.Set (Set.Current).Socket);
      end if;

      Set.Set (Set.Current)  := Set.Set (Set.Last);
      Set.Poll (Set.Current) := Set.Poll (Set.Last);

      Set.Last := Set.Last - 1;

      Next_Private (Set);
   end Remove_Socket;

   -----------
   -- Reset --
   -----------

   procedure Reset (Set : in out Socket_Set_Type) is
   begin
      for K in 1 .. Set.Last loop
         if Set.Set (K).Allocated then
            Sets.Free (Set.Set (K).Socket);
         end if;
      end loop;

      Set.Last := 0;
   end Reset;

   --------------
   -- To_State --
   --------------

   function To_State (Event : in Thin.Events_Type) return Socket_State is
      use type Thin.Events_Type;
   begin
      if (Event and (Thin.Pollerr
                    or Thin.Pollhup
                    or Thin.Pollnval
                    or Thin.Pollin
                    or Thin.Pollpri
                    or Thin.Pollout)) = 0
      then
         return None;
      end if;

      if (Event and (Thin.Pollerr or Thin.Pollhup or Thin.Pollnval)) /= 0 then
         return Error;
      end if;

      if (Event and (Thin.Pollin or Thin.Pollpri)) /= 0 then
         if (Event and Thin.Pollout) /= 0 then
            return Both;
         else
            return Input;
         end if;
      else
         return Output;
      end if;
   end To_State;

   ----------
   -- Wait --
   ----------

   procedure Wait (Set : in out Socket_Set_Type; Timeout : in  Duration) is
      use type Thin.Timeout_Type;

      Result       : Integer;
      Poll_Timeout : Thin.Timeout_Type;
   begin
      if Timeout >= Duration (Thin.Timeout_Type'Last / 1000) then
         Poll_Timeout := Thin.Timeout_Type'Last;
      else
         Poll_Timeout := Thin.Timeout_Type (Timeout * 1000);
      end if;

      Result := Integer
        (Thin.Poll
           (FDS     => Set.Poll (1)'Address,
            Nfds    => Thin.Length_Type (Set.Last),
            Timeout => Poll_Timeout));

      if Result < 0 then
         Ada.Exceptions.Raise_Exception
           (Socket_Error'Identity, "Poll error code" & Integer'Image (Errno));

      elsif Result > 0 then
         Set.Current := 1;
         Next_Private (Set);
      end if;
   end Wait;

end AWS.Net.Sets;
