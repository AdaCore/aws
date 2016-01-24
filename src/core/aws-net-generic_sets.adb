------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2004-2016, AdaCore                     --
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

with Ada.Unchecked_Deallocation;

package body AWS.Net.Generic_Sets is

   procedure Unchecked_Free is
     new Ada.Unchecked_Deallocation (Socket_Array, Socket_Array_Access);

   procedure Unchecked_Free is
     new Ada.Unchecked_Deallocation (Socket_Type'Class, Socket_Access);
      --  We could not use AWS.Net.Free because Socket_Set_Type did not
      --  allocate internal socket data.

   procedure Add_Private
     (Set    : in out Socket_Set_Type;
      Socket : Socket_Access;
      Mode   : Waiting_Mode;
      Length : out Socket_Count)
   with Pre => Socket /= null;
   --  Add Socket into Set

   ---------
   -- Add --
   ---------

   procedure Add
     (Set    : in out Socket_Set_Type;
      Socket : Socket_Type'Class;
      Mode   : Waiting_Mode)
   is
      Length : Socket_Index;
   begin
      Add_Private (Set, new Socket_Type'Class'(Socket), Mode, Length);
      Set.Set (Length).Allocated := True;
   end Add;

   procedure Add
     (Set    : in out Socket_Set_Type;
      Socket : Socket_Access;
      Mode   : Waiting_Mode)
   is
      Length : Socket_Index;
   begin
      Add_Private (Set, Socket, Mode, Length);
      Set.Set (Length).Allocated := False;
   end Add;

   procedure Add
     (Set    : in out Socket_Set_Type;
      Socket : Socket_Type'Class;
      Data   : Data_Type;
      Mode   : Waiting_Mode)
   is
      Length : Socket_Index;
   begin
      Add_Private (Set, new Socket_Type'Class'(Socket), Mode, Length);
      Set.Set (Length).Allocated := True;
      Set.Set (Length).Data      := Data;
   end Add;

   procedure Add
     (Set    : in out Socket_Set_Type;
      Socket : Socket_Access;
      Data   : Data_Type;
      Mode   : Waiting_Mode)
   is
      Length : Socket_Index;
   begin
      Add_Private (Set, Socket, Mode, Length);
      Set.Set (Length).Allocated := False;
      Set.Set (Length).Data      := Data;
   end Add;

   -----------------
   -- Add_Private --
   -----------------

   procedure Add_Private
     (Set    : in out Socket_Set_Type;
      Socket : Socket_Access;
      Mode   : Waiting_Mode;
      Length : out Socket_Count) is
   begin
      if Socket = null then
         raise Constraint_Error with "Cannot add null Socket_Access.";
      end if;

      if Set.Set = null then
         --  Allocate only few elements in array first, because this package
         --  often would be used for wait just one socket.

         Set.Poll := new FD_Set'Class'(To_FD_Set (Socket.all, Mode, 4));
         Set.Set  := new Socket_Array (1 .. Socket_Count (Set.Poll.Size));

         Length := 1;

      else
         Add (Set.Poll, Socket.Get_FD, Mode);
         Length := Socket_Count (Set.Poll.Length);
      end if;

      if Length > Set.Set'Length then
         declare
            Prev_Set : Socket_Array_Access := Set.Set;
         begin
            Set.Set  := new Socket_Array (1 .. Socket_Count (Set.Poll.Size));

            Set.Set (Prev_Set'Range)  := Prev_Set.all;

            Unchecked_Free (Prev_Set);
         end;
      end if;

      Set.Set (Length).Socket := Socket;

      if Integer (Length) /= Set.Poll.Length then
         raise Constraint_Error with
            Socket_Count'Image (Length) & " <>"
            & Integer'Image (Set.Poll.Length);
      end if;
   end Add_Private;

   -----------
   -- Count --
   -----------

   function Count (Set : Socket_Set_Type) return Socket_Count is
   begin
      if Set.Poll = null then
         return 0;
      else
         return Socket_Count (Set.Poll.Length);
      end if;
   end Count;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Set : in out Socket_Set_Type) is
   begin
      Reset (Set);
      Unchecked_Free (Set.Set);
      Free (Set.Poll);
   end Finalize;

   --------------
   -- Get_Data --
   --------------

   function Get_Data
     (Set   : Socket_Set_Type;
      Index : Socket_Index) return Data_Type is
   begin
      return Set.Set (Index).Data;
   end Get_Data;

   ----------------
   -- Get_Socket --
   ----------------

   function Get_Socket
     (Set   : Socket_Set_Type;
      Index : Socket_Index) return Socket_Type'Class is
   begin
      return Set.Set (Index).Socket.all;
   end Get_Socket;

   --------------
   -- In_Range --
   --------------

   function In_Range
     (Set   : Socket_Set_Type;
      Index : Socket_Index) return Boolean is
   begin
      return Index <= Count (Set) and then Set.Set (Index).Socket /= null;
   end In_Range;

   --------------
   -- Is_Error --
   --------------

   function Is_Error
     (Set : Socket_Set_Type; Index : Socket_Index) return Boolean is
   begin
      return Set.Poll.Status (Positive (Index)) (Error);
   end Is_Error;

   -------------------
   -- Is_Read_Ready --
   -------------------

   procedure Is_Read_Ready
     (Set   : Socket_Set_Type;
      Index : Socket_Index;
      Ready : out Boolean;
      Error : out Boolean)
   is
      Result : constant Event_Set := Set.Poll.Status (Positive (Index));
   begin
      Ready := Result (Net.Input);
      Error := Result (Net.Error);
   end Is_Read_Ready;

   function Is_Read_Ready
     (Set : Socket_Set_Type; Index : Socket_Index) return Boolean is
   begin
      return Set.Poll.Status (Positive (Index)) (Net.Input);
   end Is_Read_Ready;

   --------------------
   -- Is_Write_Ready --
   --------------------

   function Is_Write_Ready
     (Set : Socket_Set_Type; Index : Socket_Index) return Boolean is
   begin
      return Set.Poll.Status (Positive (Index)) (Net.Output);
   end Is_Write_Ready;

   ----------
   -- Next --
   ----------

   procedure Next
     (Set : Socket_Set_Type; Index : in out Socket_Index) is
   begin
      Set.Poll.Next (Positive (Index));
   end Next;

   -------------------
   -- Remove_Socket --
   -------------------

   procedure Remove_Socket
     (Set   : in out Socket_Set_Type;
      Index : Socket_Index)
   is
      Last : constant Socket_Count := Socket_Count (Set.Poll.Length);
   begin
      if Set.Set (Index).Allocated then
         Generic_Sets.Unchecked_Free (Set.Set (Index).Socket);
      end if;

      if Index < Last then
         Set.Set (Index) := Set.Set (Last);
      elsif Index > Last then
         raise Constraint_Error;
      end if;

      --  Ensure that removed socket is not accessible

      Set.Set (Last).Socket := null;
      Set.Set (Last).Allocated := False;

      Set.Poll.Remove (Positive (Index));
   end Remove_Socket;

   procedure Remove_Socket
     (Set    : in out Socket_Set_Type;
      Index  : Socket_Index;
      Socket : out Socket_Access)
   is
      Last : constant Socket_Count := Socket_Count (Set.Poll.Length);
   begin
      Socket := Set.Set (Index).Socket;

      if Index < Last then
         Set.Set (Index) := Set.Set (Last);
      elsif Index > Last then
         raise Constraint_Error;
      end if;

      --  Ensure that removed socket is not accessible

      Set.Set (Last).Socket := null;
      Set.Set (Last).Allocated := False;

      Set.Poll.Remove (Positive (Index));
   end Remove_Socket;

   -----------
   -- Reset --
   -----------

   procedure Reset (Set : in out Socket_Set_Type) is
      Last : Socket_Count;
   begin
      if Set.Poll = null then
         return;
      end if;

      Last := Socket_Count (Set.Poll.Length);

      for K in reverse 1 .. Last loop
         if Set.Set (K).Allocated then
            Generic_Sets.Unchecked_Free (Set.Set (K).Socket);
         end if;

         Set.Poll.Remove (Positive (K));
      end loop;
   end Reset;

   --------------
   -- Set_Data --
   --------------

   procedure Set_Data
     (Set   : in out Socket_Set_Type;
      Index : Socket_Index;
      Data  : Data_Type) is
   begin
      Set.Set (Index).Data := Data;
   end Set_Data;

   ---------------
   -- Set_Event --
   ---------------

   procedure Set_Event
     (Set   : in out Socket_Set_Type;
      Index : Socket_Index;
      Event : Wait_Event_Type;
      Value : Boolean) is
   begin
      Set.Poll.Set_Event (Integer (Index), Event, Value);
   end Set_Event;

   --------------
   -- Set_Mode --
   --------------

   procedure Set_Mode
     (Set    : in out Socket_Set_Type;
      Index  : Socket_Index;
      Mode   : Waiting_Mode) is
   begin
      Set.Poll.Set_Mode (Integer (Index), Mode);
   end Set_Mode;

   -------------------
   -- Update_Socket --
   -------------------

   procedure Update_Socket
     (Set     : in out Socket_Set_Type;
      Index   : Socket_Index;
      Process : not null access procedure
                  (Socket : in out Socket_Type'Class;
                   Data   : in out Data_Type)) is
   begin
      Process (Set.Set (Index).Socket.all, Set.Set (Index).Data);

      --  Socket could be reconnected, we should replace it

      Set.Poll.Replace (Integer (Index), Set.Set (Index).Socket.Get_FD);
   end Update_Socket;

   ----------
   -- Wait --
   ----------

   procedure Wait
     (Set     : in out Socket_Set_Type;
      Timeout : Duration;
      Count   : out Socket_Count) is
   begin
      if Set.Poll = null then
         Count := 0;
         return;
      end if;

      Set.Poll.Wait (Timeout, Integer (Count));
   end Wait;

   procedure Wait
     (Set     : in out Socket_Set_Type;
      Timeout : Duration)
   is
      Dummy : Socket_Count;
   begin
      Wait (Set, Timeout, Dummy);
   end Wait;

end AWS.Net.Generic_Sets;
