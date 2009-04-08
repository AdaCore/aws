------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2004-2009, AdaCore                     --
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

--  Waiting on group of sockets for input/output availability

with Ada.Finalization;

generic
   type Data_Type is private;
package AWS.Net.Generic_Sets is

   subtype Waiting_Mode is Wait_Event_Set;

   Input  : constant Wait_Event_Set :=
              (Net.Input => True, Net.Output => False);
   --  Would wait for data available for read from socket

   Output : constant Wait_Event_Set :=
              (Net.Input => False, Net.Output => True);
   --  Would wait output buffer availability for write

   Both   : constant Wait_Event_Set :=
              (Net.Input => True, Net.Output => True);
   --  Would wait for both Input and Output

   None   : constant Wait_Event_Set :=
              (Net.Input => False, Net.Output => False);
   --  Would wait only for error state in socket.
   --  Note that all waiting modes would be waiting for error state.

   type Socket_Set_Type is limited private;

   type Socket_Count is new Natural;

   subtype Socket_Index is Socket_Count range 1 .. Socket_Count'Last;

   procedure Add
     (Set    : in out Socket_Set_Type;
      Socket : Socket_Type'Class;
      Mode   : Waiting_Mode);
   pragma Inline (Add);
   --  Add socket to the set. Socket can be retreived from the set using
   --  Get_Socket.

   procedure Add
     (Set    : in out Socket_Set_Type;
      Socket : Socket_Access;
      Mode   : Waiting_Mode);
   pragma Inline (Add);
   --  Add socket to the set

   procedure Add
     (Set    : in out Socket_Set_Type;
      Socket : Socket_Type'Class;
      Data   : Data_Type;
      Mode   : Waiting_Mode);
   pragma Inline (Add);
   --  Add socket and associated data to the set

   procedure Add
     (Set    : in out Socket_Set_Type;
      Socket : Socket_Access;
      Data   : Data_Type;
      Mode   : Waiting_Mode);
   pragma Inline (Add);
   --  Add socket and associated data to the set

   procedure Set_Mode
     (Set    : in out Socket_Set_Type;
      Index  : Socket_Index;
      Mode   : Waiting_Mode);
   --  Change waiting mode for the socket in the set

   function Count (Set : Socket_Set_Type) return Socket_Count;
   pragma Inline (Count);
   --  Returns the number of sockets in the Set

   procedure Wait
     (Set     : in out Socket_Set_Type;
      Timeout : Duration);
   pragma Inline (Wait);
   --  Wait for a socket in the set to be ready for input or output operation.
   --  Raises Socket_Error if an error occurs.

   procedure Wait
     (Set     : in out Socket_Set_Type;
      Timeout : Duration;
      Count   : out Socket_Count);
   --  Wait for a socket in the set to be ready for input or output operation.
   --  Raises Socket_Error if an error occurs. Count is set with the number of
   --  activated sockets.

   function Is_Read_Ready
     (Set   : Socket_Set_Type;
      Index : Socket_Index)
      return Boolean;
   pragma Inline (Is_Read_Ready);
   --  Return True if data could be read from socket and socket was in Input
   --  or Both waiting mode.

   procedure Is_Read_Ready
     (Set   : Socket_Set_Type;
      Index : Socket_Index;
      Ready : out Boolean;
      Error : out Boolean);
   pragma Inline (Is_Read_Ready);
   --  Return True in Ready out parameter if data could be read from socket and
   --  socket was in Input or Both waiting mode. Return True in Error out
   --  parameter if socket is in error state.

   function Is_Write_Ready
     (Set   : Socket_Set_Type;
      Index : Socket_Index) return Boolean;
   pragma Inline (Is_Write_Ready);
   --  Return True if data could be written to socket and socket was in Output
   --  or Both waiting mode.

   function Is_Error
     (Set   : Socket_Set_Type;
      Index : Socket_Index) return Boolean;
   pragma Inline (Is_Error);
   --  Return True if any error occured with socket while waiting

   function In_Range
     (Set   : Socket_Set_Type;
      Index : Socket_Index) return Boolean;
   pragma Inline (In_Range);
   --  Return True if Index is in socket set range

   function Get_Socket
     (Set   : Socket_Set_Type;
      Index : Socket_Index) return Socket_Type'Class;
   pragma Inline (Get_Socket);
   --  Return socket from the Index position or raise Constraint_Error
   --  if index is more than the number of sockets in set.

   function Get_Data
     (Set   : Socket_Set_Type;
      Index : Socket_Index) return Data_Type;
   pragma Inline (Get_Data);

   procedure Set_Data
     (Set   : in out Socket_Set_Type;
      Index : Socket_Index;
      Data  : Data_Type);
   pragma Inline (Set_Data);

   procedure Remove_Socket
     (Set   : in out Socket_Set_Type;
      Index : Socket_Index);
   --  Delete socket from Index position from the Set. If the Index is not
   --  last position in the set, last socket would be placed instead of
   --  deleted one.

   procedure Remove_Socket
     (Set    : in out Socket_Set_Type;
      Index  : Socket_Index;
      Socket : out Socket_Access);
   --  Delete socket from Index position from the Set and return delete socket
   --  access. If the Index is not last position in the set, last socket would
   --  be placed instead of deleted one.

   procedure Update_Socket
     (Set     : in out Socket_Set_Type;
      Index   : Socket_Index;
      Process : not null access procedure
                  (Socket : in out Socket_Type'Class;
                   Data   : in out Data_Type));

   procedure Next (Set : Socket_Set_Type; Index : in out Socket_Index);
   --  Looking for active socket starting from Index and return Index of the
   --  found active socket. After search use functions In_Range,
   --  Is_Write_Ready, Is_Read_Ready and Is_Error to be shure that active
   --  socket is found in the Set.

   procedure Reset (Set : in out Socket_Set_Type);
   --  Clear the set for another use

private

   type Socket_Record is record
      Socket    : Socket_Access;
      Allocated : Boolean;
      --  Set to True if socket was allocated internally in the set (it is the
      --  case when using the Add with Socket_Type'Class parameter).
      --  Needed to control free on delete.

      Data      : Data_Type;
   end record;

   type Socket_Array is array (Socket_Index range <>) of Socket_Record;

   type Socket_Array_Access is access all Socket_Array;

   type Socket_Set_Type is new Ada.Finalization.Limited_Controlled with record
      Poll : FD_Set_Access;
      Set  : Socket_Array_Access;
   end record;

   overriding procedure Finalize (Set : in out Socket_Set_Type);

end AWS.Net.Generic_Sets;
