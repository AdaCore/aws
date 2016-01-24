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

pragma Ada_2012;

--  Waiting on group of sockets for input/output availability

private with Ada.Finalization;

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
      Mode   : Waiting_Mode)
   with Inline;
   --  Add socket to the set. Socket can be retreived from the set using
   --  Get_Socket.

   procedure Add
     (Set    : in out Socket_Set_Type;
      Socket : Socket_Access;
      Mode   : Waiting_Mode)
   with Inline, Pre => Socket /= null;
   --  Add socket to the set

   procedure Add
     (Set    : in out Socket_Set_Type;
      Socket : Socket_Type'Class;
      Data   : Data_Type;
      Mode   : Waiting_Mode)
   with Inline;
   --  Add socket and associated data to the set

   procedure Add
     (Set    : in out Socket_Set_Type;
      Socket : Socket_Access;
      Data   : Data_Type;
      Mode   : Waiting_Mode)
   with Inline, Pre => Socket /= null;
   --  Add socket and associated data to the set

   procedure Set_Mode
     (Set    : in out Socket_Set_Type;
      Index  : Socket_Index;
      Mode   : Waiting_Mode)
   with Inline, Pre => In_Range (Set, Index);
   --  Change waiting mode for the socket in the set

   procedure Set_Event
     (Set   : in out Socket_Set_Type;
      Index : Socket_Index;
      Event : Wait_Event_Type;
      Value : Boolean)
   with Inline, Pre => In_Range (Set, Index);
   --  Set or reset waiting event for the socket in the set

   function Count (Set : Socket_Set_Type) return Socket_Count with Inline;
   --  Returns the number of sockets in the Set

   procedure Wait
     (Set     : in out Socket_Set_Type;
      Timeout : Duration)
   with Inline;
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
      Index : Socket_Index) return Boolean
   with
     Inline,
     Pre => In_Range (Set, Index);
   --  Return True if data could be read from socket and socket was in Input
   --  or Both waiting mode.

   procedure Is_Read_Ready
     (Set   : Socket_Set_Type;
      Index : Socket_Index;
      Ready : out Boolean;
      Error : out Boolean)
   with
     Inline,
     Pre => In_Range (Set, Index);
   --  Return True in Ready out parameter if data could be read from socket and
   --  socket was in Input or Both waiting mode. Return True in Error out
   --  parameter if socket is in error state.

   function Is_Write_Ready
     (Set   : Socket_Set_Type;
      Index : Socket_Index) return Boolean
   with
     Inline,
     Pre => In_Range (Set, Index);
   --  Return True if data could be written to socket and socket was in Output
   --  or Both waiting mode.

   function Is_Error
     (Set   : Socket_Set_Type;
      Index : Socket_Index) return Boolean
   with
     Inline,
     Pre => In_Range (Set, Index);
   --  Return True if any error occured with socket while waiting

   function In_Range
     (Set   : Socket_Set_Type;
      Index : Socket_Index) return Boolean with Inline;
   --  Return True if Index is in socket set range

   function Get_Socket
     (Set   : Socket_Set_Type;
      Index : Socket_Index) return Socket_Type'Class
   with
     Inline,
     Pre => In_Range (Set, Index);
   --  Return socket from the Index position or raise Constraint_Error
   --  if index is more than the number of sockets in set.

   function Get_Data
     (Set   : Socket_Set_Type;
      Index : Socket_Index) return Data_Type
   with
     Inline,
     Pre => In_Range (Set, Index);

   procedure Set_Data
     (Set   : in out Socket_Set_Type;
      Index : Socket_Index;
      Data  : Data_Type)
   with
     Inline,
     Pre => In_Range (Set, Index);

   procedure Remove_Socket
     (Set   : in out Socket_Set_Type;
      Index : Socket_Index)
   with Pre => In_Range (Set, Index);
   --  Delete socket from Index position from the Set. If the Index is not
   --  last position in the set, last socket would be placed instead of
   --  deleted one.

   procedure Remove_Socket
     (Set    : in out Socket_Set_Type;
      Index  : Socket_Index;
      Socket : out Socket_Access)
   with Pre => In_Range (Set, Index);
   --  Delete socket from Index position from the Set and return delete socket
   --  access. If the Index is not last position in the set, last socket would
   --  be placed instead of deleted one.

   procedure Update_Socket
     (Set     : in out Socket_Set_Type;
      Index   : Socket_Index;
      Process : not null access procedure
                  (Socket : in out Socket_Type'Class;
                   Data   : in out Data_Type))
   with Pre => In_Range (Set, Index);

   procedure Next (Set : Socket_Set_Type; Index : in out Socket_Index) with
     Pre  => Index = Count (Set) + 1        -- either past of last item
             or else In_Range (Set, Index), -- or in the range of the set

     Post => not In_Range (Set, Index)
             or else Is_Write_Ready (Set, Index)
             or else Is_Read_Ready (Set, Index)
             or else Is_Error (Set, Index);
   --  Looking for active socket starting from Index and return Index of the
   --  found active socket. After search use functions In_Range,
   --  Is_Write_Ready, Is_Read_Ready and Is_Error to be sure that active
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
