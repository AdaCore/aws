------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2004                          --
--                                ACT-Europe                                --
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
with Interfaces.C;

with AWS.Net.Sets.Thin;
with AWS.Net.Std;
with AWS.Net.SSL;
with AWS.OS_Lib.Definitions;

package body AWS.Net is

   use Ada;

   function Errno return Integer renames Std.Errno;

   ----------
   -- Free --
   ----------

   procedure Free (Socket : in out Socket_Access) is
      procedure Free is
         new Ada.Unchecked_Deallocation (Socket_Type'Class, Socket_Access);
   begin
      if Socket /= null then
         Release_Cache (Socket.all);
         Free (Socket.all);
         Free (Socket);
      end if;
   end Free;

   ---------------
   -- Host_Name --
   ---------------

   function Host_Name return String is
   begin
      return Net.Std.Host_Name;
   end Host_Name;

   -------------
   -- Receive --
   -------------

   function Receive
     (Socket : in Socket_Type'Class;
      Max    : in Stream_Element_Count := 4096)
      return Stream_Element_Array
   is
      Result : Stream_Element_Array (1 .. Max);
      Last   : Stream_Element_Offset;
   begin
      Receive (Socket, Result, Last);

      return Result (1 .. Last);
   end Receive;

   -------------------
   -- Release_Cache --
   -------------------

   procedure Release_Cache (Socket : in out Socket_Type'Class) is
      procedure Free is
         new Ada.Unchecked_Deallocation (RW_Cache, RW_Cache_Access);
   begin
      Free (Socket.C);
   end Release_Cache;

   -----------------------
   -- Set_Blocking_Mode --
   -----------------------

   procedure Set_Blocking_Mode
     (Socket   : in out Socket_Type;
      Blocking : in     Boolean) is
   begin
      if Blocking then
         Socket.Timeout := Forever;
      else
         Socket.Timeout := 0.0;
      end if;
   end Set_Blocking_Mode;

   ---------------
   -- Set_Cache --
   ---------------

   procedure Set_Cache (Socket : in out Socket_Type'Class) is
   begin
      --  Recreate cache if it already exists

      if Socket.C /= null then
         Release_Cache (Socket);
      end if;

      Socket.C := new RW_Cache;
   end Set_Cache;

   -----------------
   -- Set_Timeout --
   -----------------

   procedure Set_Timeout
     (Socket   : in out Socket_Type;
      Timeout  : in     Duration) is
   begin
      Socket.Timeout := Timeout;
   end Set_Timeout;

   ------------
   -- Socket --
   ------------

   function Socket (Security : in Boolean) return Socket_Type'Class is
   begin
      if Security then
         declare
            Result : SSL.Socket_Type;
         begin
            return Result;
         end;

      else
         declare
            Result : Std.Socket_Type;
         begin
            return Result;
         end;
      end if;
   end Socket;

   function Socket (Security : in Boolean) return Socket_Access is
   begin
      return new Socket_Type'Class'(Socket (Security));
   end Socket;

   --------------
   -- Wait_For --
   --------------

   procedure Wait_For (Mode : in Wait_Mode; Socket : in Socket_Type'Class) is
      use Interfaces;
      use AWS.Net.Sets;
      use OS_Lib;

      use type C.int;
      use type Thin.Events_Type;

      To_Poll_Mode : constant array (Wait_Mode) of Thin.Events_Type
        := (Input => Definitions.POLLIN, Output => Definitions.POLLOUT);

      PFD : aliased Thin.Pollfd
        := (Fd      => Thin.FD_Type (Get_FD (Socket)),
            Events  => To_Poll_Mode (Mode),
            REvents => 0);
      RC      : C.int;
      Timeout : C.int;
   begin
      if Socket.Timeout >= Duration (C.int'Last / 1_000) then
         Timeout := C.int'Last;
      else
         Timeout := C.int (Socket.Timeout * 1_000);
      end if;

      RC := Thin.Poll (PFD'Address, 1, Timeout);

      case RC is
         when -1 =>
            Ada.Exceptions.Raise_Exception
              (Socket_Error'Identity,
               "Wait_For_" & Wait_Mode'Image (Mode)
                 & " error code" & Integer'Image (Std.Errno));

         when 0 =>
            Ada.Exceptions.Raise_Exception
              (Socket_Error'Identity,
               Wait_Mode'Image (Mode) & " timeout.");

         when 1 =>
            if PFD.REvents = To_Poll_Mode (Mode) then
               return;
            else
               Ada.Exceptions.Raise_Exception
                 (Socket_Error'Identity,
                  Wait_Mode'Image (Mode) & "_Wait error.");
            end if;

         when others =>
            raise Program_Error;
      end case;
   end Wait_For;

end AWS.Net;
