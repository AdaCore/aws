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

pragma Ada_2012;

--  This is the socket memory stream. It can be used to route all data to an
--  in-memory buffer before actually sending the data to a real socket for
--  example.
--
--  The server side is done by the AWS runtime, that is there is no needed to
--  Bind, Listen or Accept_Socket. To use such socket both side has to call
--  Connect on a specific port with the hostname set to ":memory:".

with Memory_Streams;

package AWS.Net.Memory is

   type Socket_Type is new Net.Socket_Type with private;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Bind
     (Socket        : in out Socket_Type;
      Port          : Natural;
      Host          : String      := "";
      Reuse_Address : Boolean     := False;
      IPv6_Only     : Boolean     := False;
      Family        : Family_Type := Family_Unspec) is null;
   --  Bind a socket on a given port

   overriding procedure Listen
     (Socket     : Socket_Type;
      Queue_Size : Positive := 5) is null;
   --  Set the queue size of the socket

   overriding procedure Accept_Socket
     (Socket     : Net.Socket_Type'Class;
      New_Socket : in out Socket_Type) is null;
   --  Accept a connection on a socket

   overriding procedure Connect
     (Socket : in out Socket_Type;
      Host   : String;
      Port   : Positive;
      Wait   : Boolean     := True;
      Family : Family_Type := Family_Unspec)
     with Pre => Host = ":memory:";
   --  Connect to any known memory socket or create a new one. The memory
   --  socket identifier is given by the Port number.

   overriding procedure Shutdown
     (Socket : Socket_Type; How : Shutmode_Type := Shut_Read_Write);
   --  Shutdown the read, write or both side of the socket.
   --  If How is Both, close it. Does not raise Socket_Error if the socket is
   --  not connected or already shutdown.

   --------
   -- IO --
   --------

   overriding procedure Send
     (Socket : Socket_Type;
      Data   : Stream_Element_Array;
      Last   : out Stream_Element_Offset);

   overriding procedure Receive
     (Socket : Socket_Type;
      Data   : out Stream_Element_Array;
      Last   : out Stream_Element_Offset)
   with Inline;

   overriding function Pending
     (Socket : Socket_Type) return Stream_Element_Count;
   --  Returns the number of bytes which are available inside socket
   --  for immediate read.

   ------------
   -- Others --
   ------------

   overriding function Get_FD (Socket : Socket_Type) return Integer;
   --  Returns the file descriptor associated with the socket

   overriding function Peer_Addr (Socket : Socket_Type) return String;
   --  Returns the peer name/address

   overriding function Peer_Port (Socket : Socket_Type) return Positive;
   --  Returns the port of the peer socket

   overriding function Get_Addr (Socket : Socket_Type) return String;
   --  Returns the name/address of the socket

   overriding function Get_Port (Socket : Socket_Type) return Positive;
   --  Returns the port of the socket

   overriding function Is_Any_Address (Socket : Socket_Type) return Boolean;
   --  Return true if the socket accepts connections on any of the hosts's
   --  network addresses.

   overriding procedure Set_Send_Buffer_Size
     (Socket : Socket_Type;
      Size   : Natural) is null;
   --  Set the internal socket send buffer size.
   --  Do not confuse with buffers for the AWS.Net.Buffered operations.

   overriding procedure Set_Receive_Buffer_Size
     (Socket : Socket_Type;
      Size   : Natural) is null;
   --  Set the internal socket receive buffer size.
   --  Do not confuse with buffers for the AWS.Net.Buffered operations.

   overriding function Get_Send_Buffer_Size
     (Socket : Socket_Type) return Natural;
   --  Returns the internal socket send buffer size.
   --  Do not confuse with buffers for the AWS.Net.Buffered operations.

   overriding function Get_Receive_Buffer_Size
     (Socket : Socket_Type) return Natural;
   --  Returns the internal socket receive buffer size.
   --  Do not confuse with buffers for the AWS.Net.Buffered operations.

   overriding function Errno (Socket : Socket_Type) return Integer;
   --  Returns and clears error state in socket

private

   type Element_Access is access Stream_Element_Array;
   type Constant_Element_Access is access constant Stream_Element_Array;

   package Stream_Memory is new Memory_Streams
     (Stream_Element, Stream_Element_Offset, Stream_Element_Array,
      Element_Access, Constant_Element_Access);

   type Stream_Access is access Stream_Memory.Stream_Type;

   type Socket_Type is new Net.Socket_Type with record
      Port : Positive;
      S    : Stream_Access;
   end record;

   overriding procedure Free (Socket : in out Socket_Type);
   --  Release memory associated with the socket object

   overriding procedure Initialize (Socket : in out Socket_Type);

end AWS.Net.Memory;
