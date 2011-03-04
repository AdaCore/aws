------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2004-2011, AdaCore                     --
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

with AWS.Net;

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
      Length : out Socket_Count);
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
      if Set.Set = null then
         --  Allocate only few elements in array first, because this package
         --  often would be used for wait just one socket.

         Set.Poll := new FD_Set'Class'(To_FD_Set (Socket.all, Mode, 4));
         Set.Set  := new Socket_Array (1 .. Socket_Count (Set.Poll.Size));

         Length := 1;

      else
         Add (Set.Poll, Get_FD (Socket.all), Mode);
         Length := Socket_Count (Net.Length (Set.Poll.all));
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

      if Integer (Length) /= Net.Length (Set.Poll.all) then
         raise Constraint_Error with
            Socket_Count'Image (Length) & " <>"
            & Integer'Image (Net.Length (Set.Poll.all));
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
         return Socket_Count (Length (Set.Poll.all));
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
      return Index <= Count (Set);
   end In_Range;

   --------------
   -- Is_Error --
   --------------

   function Is_Error
     (Set : Socket_Set_Type; Index : Socket_Index) return Boolean is
   begin
      return Status (Set.Poll.all, Positive (Index)) (Error);
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
      Result : constant Event_Set := Status (Set.Poll.all, Positive (Index));
   begin
      Ready := Result (Net.Input);
      Error := Result (Net.Error);
   end Is_Read_Ready;

   function Is_Read_Ready
     (Set : Socket_Set_Type; Index : Socket_Index) return Boolean is
   begin
      return Status (Set.Poll.all, Positive (Index)) (Net.Input);
   end Is_Read_Ready;

   --------------------
   -- Is_Write_Ready --
   --------------------

   function Is_Write_Ready
     (Set : Socket_Set_Type; Index : Socket_Index) return Boolean is
   begin
      return Status (Set.Poll.all, Positive (Index)) (Net.Output);
   end Is_Write_Ready;

   ----------
   -- Next --
   ----------

   procedure Next
     (Set : Socket_Set_Type; Index : in out Socket_Index) is
   begin
      Next (Set.Poll.all, Positive (Index));
   end Next;

   -------------------
   -- Remove_Socket --
   -------------------

   procedure Remove_Socket
     (Set   : in out Socket_Set_Type;
      Index : Socket_Index)
   is
      Last : constant Socket_Count := Socket_Count (Length (Set.Poll.all));
   begin
      if Set.Set (Index).Allocated then
         Generic_Sets.Unchecked_Free (Set.Set (Index).Socket);
      end if;

      if Index < Last then
         Set.Set (Index) := Set.Set (Last);
      elsif Index > Last then
         raise Constraint_Error;
      end if;

      Remove (Set.Poll.all, Positive (Index));
   end Remove_Socket;

   procedure Remove_Socket
     (Set    : in out Socket_Set_Type;
      Index  : Socket_Index;
      Socket : out Socket_Access)
   is
      Last : constant Socket_Count := Socket_Count (Length (Set.Poll.all));
   begin
      Socket := Set.Set (Index).Socket;

      if Index < Last then
         Set.Set (Index) := Set.Set (Last);
      elsif Index > Last then
         raise Constraint_Error;
      end if;

      Remove (Set.Poll.all, Positive (Index));
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

      Last := Socket_Count (Length (Set.Poll.all));

      for K in reverse 1 .. Last loop
         if Set.Set (K).Allocated then
            Generic_Sets.Unchecked_Free (Set.Set (K).Socket);
         end if;
         Remove (Set.Poll.all, Positive (K));
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

   --------------
   -- Set_Mode --
   --------------

   procedure Set_Mode
     (Set    : in out Socket_Set_Type;
      Index  : Socket_Index;
      Mode   : Waiting_Mode) is
   begin
      Set_Mode (Set.Poll.all, Integer (Index), Mode);
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

      Wait (Set.Poll.all, Timeout, Integer (Count));
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
