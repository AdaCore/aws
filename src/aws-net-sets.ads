------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                            Copyright (C) 2004                            --
--                               ACT-Europe                                 --
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
--
--  $Id$
--
--  Waiting on group of sockets for input/output availability.
--
package AWS.Net.Sets is

   type Socket_Set_Type is limited private;

   type Socket_State is (None, Error, Input, Output, Both);

   subtype Waiting_Mode is Socket_State range Input .. Both;

   procedure Add
     (Set    : in out Socket_Set_Type;
      Socket : in     Socket_Type'Class;
      Mode   : in     Waiting_Mode);
   --  Add socket to the set.

   procedure Add
     (Set    : in out Socket_Set_Type;
      Socket : in     Socket_Access;
      Mode   : in     Waiting_Mode);
   --  Add socket to the set.

   procedure Wait
     (Set     : in out Socket_Set_Type;
      Timeout : in     Duration);
   --  Wait in a set of sockets when some of them would be ready for
   --  input/output or error occured. And select first ready socket in set
   --  inernally for following operations.

   procedure Take_Socket
     (Set    : in out Socket_Set_Type;
      Socket :    out Socket_Access;
      State  :    out Socket_State);
   --  Return currently selected ready for input/output socket and delete it
   --  from set.

   -----------------------------------------------------
   -- Step by step operations for take ready sockets. --
   -----------------------------------------------------

   function Get_Socket_State (Set : in Socket_Set_Type) return Socket_State;
   --  Return the state of the currently  selected ready for input/output
   --  socket or None if there is no more sockets available for input/output.

   function Get_Socket (Set : in Socket_Set_Type) return Socket_Type'Class;
   --  Return currently selected ready for input/output socket.

   function Get_Socket (Set : in Socket_Set_Type) return Socket_Access;
   --  Return currently selected ready for input/output socket.

   procedure Remove_Socket (Set : in out Socket_Set_Type);
   --  Delete currently selected socket from set
   --  and select the next one.

private

   type Poll_Set_Type;

   type Poll_Set_Access is access all Poll_Set_Type;

   type Socket_Record is record
      Socket    : Socket_Access;

      Allocated : Boolean := False;
      --  Is socket was allocated internally in the set.
      --  Need to control free on delete.
   end record;

   type Socket_Array is array (Positive range <>) of Socket_Record;

   type Socket_Array_Access is access all Socket_Array;

   type Socket_Set_Type is limited record
      Poll    : Poll_Set_Access;
      Set     : Socket_Array_Access;
      Last    : Natural  := 0;
      Current : Positive := 1;
   end record;

end AWS.Net.Sets;