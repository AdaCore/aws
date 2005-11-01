------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                            Copyright (C) 2004                            --
--                                ACT-Europe                                --
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

with AWS.Net.Thin;
with AWS.OS_Lib.Definitions;

package body AWS.Net.Generic_Sets is

   type Poll_Set_Type is array (Socket_Index range <>) of Thin.Pollfd;
   pragma Pack (Poll_Set_Type);

   procedure Free is
     new Ada.Unchecked_Deallocation (Poll_Set_Type, Poll_Set_Access);

   procedure Free is
     new Ada.Unchecked_Deallocation (Socket_Array, Socket_Array_Access);

   procedure Free is
     new Ada.Unchecked_Deallocation (Socket_Type'Class, Socket_Access);
      --  We could not use AWS.Net.Free because Socket_Set_Type did not
      --  allocate internal socket data.

   procedure Check_Range (Set : in Socket_Set_Type; Index : in Socket_Index);
   pragma Inline (Check_Range);
   --  Raise Constraint_Error if Index not in Set range.

   procedure Add_Private
     (Set    : in out Socket_Set_Type;
      Socket : in     Socket_Access;
      Mode   : in     Waiting_Mode);
   --  Add Socket into Set

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

   procedure Add
     (Set    : in out Socket_Set_Type;
      Socket : in     Socket_Type'Class;
      Data   : in     Data_Type;
      Mode   : in     Waiting_Mode) is
   begin
      Add_Private (Set, new Socket_Type'Class'(Socket), Mode);
      Set.Set (Set.Last).Allocated := True;
      Set.Set (Set.Last).Data      := Data;
   end Add;

   procedure Add
     (Set    : in out Socket_Set_Type;
      Socket : in     Socket_Access;
      Data   : in     Data_Type;
      Mode   : in     Waiting_Mode) is
   begin
      Add_Private (Set, Socket, Mode);
      Set.Set (Set.Last).Allocated := False;
      Set.Set (Set.Last).Data      := Data;
   end Add;

   -----------------
   -- Add_Private --
   -----------------

   procedure Add_Private
     (Set    : in out Socket_Set_Type;
      Socket : in     Socket_Access;
      Mode   : in     Waiting_Mode) is
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
            Increment : Socket_Index;

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

      Set.Set  (Set.Last).Socket := Socket;
      Set.Poll (Set.Last).FD     := Thin.FD_Type (Get_FD (Socket.all));

      Set_Mode (Set, Set.Last, Mode);
   end Add_Private;

   -----------------
   -- Check_Range --
   -----------------

   procedure Check_Range (Set : in Socket_Set_Type; Index : in Socket_Index) is
   begin
      if Index > Set.Last then
         raise Constraint_Error;
      end if;
   end Check_Range;

   -----------
   -- Count --
   -----------

   function Count (Set : in Socket_Set_Type) return Socket_Count is
   begin
      return Set.Last;
   end Count;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Set : in out Socket_Set_Type) is
   begin
      Reset (Set);
      Free (Set.Set);
      Free (Set.Poll);
   end Finalize;

   --------------
   -- Get_Data --
   --------------

   function Get_Data
     (Set   : in Socket_Set_Type;
      Index : in Socket_Index)
      return Data_Type is
   begin
      Check_Range (Set, Index);

      return Set.Set (Index).Data;
   end Get_Data;

   ----------------
   -- Get_Socket --
   ----------------

   function Get_Socket
     (Set   : in Socket_Set_Type;
      Index : in Socket_Index)
      return Socket_Type'Class is
   begin
      Check_Range (Set, Index);

      return Set.Set (Index).Socket.all;
   end Get_Socket;

   --------------
   -- In_Range --
   --------------

   function In_Range
     (Set   : in Socket_Set_Type;
      Index : in Socket_Index)
      return Boolean is
   begin
      return Index <= Set.Last;
   end In_Range;

   --------------
   -- Is_Error --
   --------------

   function Is_Error
     (Set   : in Socket_Set_Type;
      Index : in Socket_Index)
      return Boolean
   is
      use AWS.OS_Lib.Definitions;
   begin
      Check_Range (Set, Index);

      return (Set.Poll (Index).REvents
              and (POLLERR or POLLHUP or POLLNVAL)) /= 0;
   end Is_Error;

   -------------------
   -- Is_Read_Ready --
   -------------------

   function Is_Read_Ready
     (Set   : in Socket_Set_Type;
      Index : in Socket_Index)
      return Boolean
   is
      use AWS.OS_Lib.Definitions;
   begin
      Check_Range (Set, Index);

      return (Set.Poll (Index).REvents and (POLLIN or POLLPRI)) /= 0;
   end Is_Read_Ready;

   procedure Is_Read_Ready
     (Set   : in     Socket_Set_Type;
      Index : in     Socket_Index;
      Ready :    out Boolean;
      Error :    out Boolean)
   is
      use AWS.OS_Lib.Definitions;
   begin
      Check_Range (Set, Index);

      Ready := (Set.Poll (Index).REvents and (POLLIN or POLLPRI)) /= 0;
      Error := (Set.Poll (Index).REvents
                and (POLLERR or POLLHUP or POLLNVAL)) /= 0;
   end Is_Read_Ready;

   --------------------
   -- Is_Write_Ready --
   --------------------

   function Is_Write_Ready
     (Set   : in Socket_Set_Type;
      Index : in Socket_Index)
      return Boolean
   is
      use AWS.OS_Lib.Definitions;
   begin
      Check_Range (Set, Index);

      return (Set.Poll (Index).REvents and POLLOUT) /= 0;
   end Is_Write_Ready;

   ----------
   -- Next --
   ----------

   procedure Next
     (Set   : in     Socket_Set_Type;
      Index : in out Socket_Index)
   is
      use type Thin.Events_Type;
   begin
      loop
         exit when Index > Set.Last
           or else Set.Poll (Index).REvents /= 0;

         Index := Index + 1;
      end loop;
   end Next;

   -------------------
   -- Remove_Socket --
   -------------------

   procedure Remove_Socket
     (Set   : in out Socket_Set_Type;
      Index : in     Socket_Index) is
   begin
      Check_Range (Set, Index);

      if Set.Set (Index).Allocated then
         Generic_Sets.Free (Set.Set (Index).Socket);
      end if;

      Set.Set (Index)  := Set.Set (Set.Last);
      Set.Poll (Index) := Set.Poll (Set.Last);

      Set.Last := Set.Last - 1;
   end Remove_Socket;

   procedure Remove_Socket
     (Set    : in out Socket_Set_Type;
      Index  : in     Socket_Index;
      Socket :    out Socket_Access) is
   begin
      Check_Range (Set, Index);

      Socket := Set.Set (Index).Socket;

      Set.Set (Index)  := Set.Set (Set.Last);
      Set.Poll (Index) := Set.Poll (Set.Last);

      Set.Last := Set.Last - 1;
   end Remove_Socket;

   -----------
   -- Reset --
   -----------

   procedure Reset (Set : in out Socket_Set_Type) is
   begin
      for K in 1 .. Set.Last loop
         if Set.Set (K).Allocated then
            Generic_Sets.Free (Set.Set (K).Socket);
         end if;
      end loop;

      Set.Last := 0;
   end Reset;

   --------------
   -- Set_Data --
   --------------

   procedure Set_Data
     (Set   : in out Socket_Set_Type;
      Index : in     Socket_Index;
      Data  : in     Data_Type) is
   begin
      Check_Range (Set, Index);

      Set.Set (Index).Data := Data;
   end Set_Data;

   --------------
   -- Set_Mode --
   --------------

   procedure Set_Mode
     (Set    : in out Socket_Set_Type;
      Index  : in     Socket_Index;
      Mode   : in     Waiting_Mode)
   is
      package OSD renames OS_Lib.Definitions;
      use type Thin.Events_Type;
   begin
      if Mode (Net.Input) then
         Set.Poll (Index).Events := OSD.POLLIN or OSD.POLLPRI;
      else
         Set.Poll (Index).Events := 0;
      end if;

      if Mode (Net.Output) then
         Set.Poll (Index).Events
           := Set.Poll (Index).Events or OSD.POLLOUT;
      end if;
   end Set_Mode;

   ----------
   -- Wait --
   ----------

   procedure Wait
     (Set     : in out Socket_Set_Type;
      Timeout : in     Duration;
      Count   :    out Socket_Count)
   is
      use type Thin.Timeout_Type;

      Result       : Socket_Count'Base;
      Poll_Timeout : Thin.Timeout_Type;
   begin
      if Set.Last = 0 then
         Count := 0;
         return;
      end if;

      if Timeout >= Duration (Thin.Timeout_Type'Last / 1_000) then
         Poll_Timeout := Thin.Timeout_Type'Last;
      else
         Poll_Timeout := Thin.Timeout_Type (Timeout * 1_000);
      end if;

      Result := Socket_Count'Base
        (Thin.Poll
           (FDS     => Set.Poll (Set.Poll'First)'Address,
            Nfds    => Thin.nfds_t (Set.Last),
            Timeout => Poll_Timeout));

      if Result < 0 then
         Ada.Exceptions.Raise_Exception
           (Socket_Error'Identity, "Poll error code" & Integer'Image (Errno));
      end if;

      Count := Result;
   end Wait;

   procedure Wait
     (Set     : in out Socket_Set_Type;
      Timeout : in     Duration)
   is
      Dummy : Socket_Count;
   begin
      Wait (Set, Timeout, Dummy);
   end Wait;

end AWS.Net.Generic_Sets;
