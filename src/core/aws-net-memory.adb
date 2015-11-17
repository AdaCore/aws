------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                       Copyright (C) 2015, AdaCore                        --
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

with Ada.Containers.Ordered_Maps;
with Ada.Unchecked_Deallocation;

package body AWS.Net.Memory is

   Anonymous_Port : constant Natural := Natural'Last;

   package Sockets_Map is new Containers.Ordered_Maps (Positive, Socket_Type);

   --  Make sure that the socket registry is thread safe

   protected Shared_Socks is

      procedure Get (Socket : out Socket_Type; Found : out Boolean);
      --  Returns the Socket and Found to true if it was on the set

      procedure Include (Socket : Socket_Type);
      --  Include this socket into the set

      procedure Exclude (Socket : Socket_Type);
      --  Excluse this socket from the set

   private
      Mem_Socks : Sockets_Map.Map;
   end Shared_Socks;

   -------------
   -- Connect --
   -------------

   overriding procedure Connect
     (Socket : in out Socket_Type;
      Host   : String;
      Port   : Positive;
      Wait   : Boolean     := True;
      Family : Family_Type := Family_Unspec)
   is
      pragma Unreferenced (Host, Wait, Family);
      Found : Boolean;
   begin
      Socket.Port := Port;

      Shared_Socks.Get (Socket, Found);

      if not Found then
         Socket.S := new Stream_Memory.Stream_Type;
         Shared_Socks.Include (Socket);
      end if;
   end Connect;

   -----------
   -- Errno --
   -----------

   overriding function Errno (Socket : Socket_Type) return Integer is
      pragma Unreferenced (Socket);
   begin
      return 0;
   end Errno;

   ----------
   -- Free --
   ----------

   overriding procedure Free (Socket : in out Socket_Type) is
      procedure Unchecked_Free is new Unchecked_Deallocation
        (Stream_Memory.Stream_Type, Stream_Access);
   begin
      if Socket.S /= null then
         Stream_Memory.Close (Socket.S.all);
         Unchecked_Free (Socket.S);
      end if;
   end Free;

   --------------
   -- Get_Addr --
   --------------

   overriding function Get_Addr (Socket : Socket_Type) return String is
      pragma Unreferenced (Socket);
   begin
      return ":memory:";
   end Get_Addr;

   ------------
   -- Get_FD --
   ------------

   overriding function Get_FD (Socket : Socket_Type) return Integer is
   begin
      return Socket.Port;
   end Get_FD;

   --------------
   -- Get_Port --
   --------------

   overriding function Get_Port (Socket : Socket_Type) return Positive is
   begin
      return Socket.Port;
   end Get_Port;

   -----------------------------
   -- Get_Receive_Buffer_Size --
   -----------------------------

   overriding function Get_Receive_Buffer_Size
     (Socket : Socket_Type) return Natural
   is
      pragma Unreferenced (Socket);
   begin
      return Natural'Last;
   end Get_Receive_Buffer_Size;

   --------------------------
   -- Get_Send_Buffer_Size --
   --------------------------

   overriding function Get_Send_Buffer_Size
     (Socket : Socket_Type) return Natural is
      pragma Unreferenced (Socket);
   begin
      return Natural'Last;
   end Get_Send_Buffer_Size;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (Socket : in out Socket_Type) is
   begin
      Socket.S := new Stream_Memory.Stream_Type;
      Socket.Port := Anonymous_Port;

      Net.Socket_Type (Socket).Initialize;
   end Initialize;

   --------------------
   -- Is_Any_Address --
   --------------------

   overriding function Is_Any_Address (Socket : Socket_Type) return Boolean is
      pragma Unreferenced (Socket);
   begin
      return True;
   end Is_Any_Address;

   ---------------
   -- Peer_Addr --
   ---------------

   overriding function Peer_Addr (Socket : Socket_Type) return String is
      pragma Unreferenced (Socket);
   begin
      return ":memory:";
   end Peer_Addr;

   ---------------
   -- Peer_Port --
   ---------------

   overriding function Peer_Port (Socket : Socket_Type) return Positive is
   begin
      return Socket.Port;
   end Peer_Port;

   -------------
   -- Pending --
   -------------

   overriding function Pending
     (Socket : Socket_Type) return Stream_Element_Count is
   begin
      return Stream_Memory.Pending (Socket.S.all);
   end Pending;

   -------------
   -- Receive --
   -------------

   overriding procedure Receive
     (Socket : Socket_Type;
      Data   : out Stream_Element_Array;
      Last   : out Stream_Element_Offset) is
   begin
      Stream_Memory.Read (Socket.S.all, Data, Last);
   end Receive;

   ----------
   -- Send --
   ----------

   overriding procedure Send
     (Socket : Socket_Type;
      Data   : Stream_Element_Array;
      Last   : out Stream_Element_Offset) is
   begin
      Stream_Memory.Append (Socket.S.all, Data);
      Last := Data'Last;
   end Send;

   ------------------
   -- Shared_Socks --
   ------------------

   protected body Shared_Socks is

      -------------
      -- Exclude --
      -------------

      procedure Exclude (Socket : Socket_Type) is
      begin
         Mem_Socks.Exclude (Socket.Port);
      end Exclude;

      ---------
      -- Get --
      ---------

      procedure Get (Socket : out Socket_Type; Found : out Boolean) is
      begin
         if Mem_Socks.Contains (Socket.Port) then
            Found := True;
            Socket := Mem_Socks (Socket.Port);

         else
            Found := False;
         end if;
      end Get;

      -------------
      -- Include --
      -------------

      procedure Include (Socket : Socket_Type) is
      begin
         Mem_Socks.Include (Socket.Port, Socket);
      end Include;

   end Shared_Socks;

   --------------
   -- Shutdown --
   --------------

   overriding procedure Shutdown
     (Socket : Socket_Type; How : Shutmode_Type := Shut_Read_Write)
   is
      pragma Unreferenced (How);
   begin
      Stream_Memory.Close (Socket.S.all);
      Shared_Socks.Exclude (Socket);
   end Shutdown;

end AWS.Net.Memory;
