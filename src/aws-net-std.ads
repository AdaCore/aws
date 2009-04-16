------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2002-2009, AdaCore                     --
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

--  This is the standard (non-SSL) socket support. Only the implementation of
--  this package depends on a specific socket binding. To port AWS to another
--  socket implementation you need only to rewrite the body.

package AWS.Net.Std is

   Socket_Error : exception renames Net.Socket_Error;

   type Socket_Type is new Net.Socket_Type with private;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Bind
     (Socket        : in out Socket_Type;
      Port          : Natural;
      Host          : String  := "";
      Reuse_Address : Boolean := False);
   --  Bind a socket on a given port

   overriding procedure Listen
     (Socket     : Socket_Type;
      Queue_Size : Positive := 5);
   --  Set the queue size of the socket

   overriding procedure Accept_Socket
     (Socket     : Net.Socket_Type'Class;
      New_Socket : in out Socket_Type);
   --  Accept a connection on a socket

   overriding procedure Connect
     (Socket : in out Socket_Type;
      Host   : String;
      Port   : Positive;
      Wait   : Boolean := True);
   --  Connect a socket on a given host/port. If Wait is True Connect will wait
   --  for the connection to be established for timeout seconds, specified by
   --  Set_Timeout routine. If Wait is False Connect will return immediately,
   --  not waiting for the connection to be establised. It is possible to wait
   --  for the Connection completion by calling Wait routine with Output set to
   --  True in Events parameter.

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
      Last   : out Stream_Element_Offset);
   pragma Inline (Receive);

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

   function Host_Name return String;

   overriding procedure Set_Send_Buffer_Size
     (Socket : Socket_Type;
      Size   : Natural);
   --  Set the internal socket send buffer size.
   --  Do not confuse with buffers for the AWS.Net.Buffered operations.

   overriding procedure Set_Receive_Buffer_Size
     (Socket : Socket_Type;
      Size   : Natural);
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

   type Socket_Hidden;

   type Socket_Hidden_Access is access Socket_Hidden;

   type Socket_Type is new Net.Socket_Type with record
      S : Socket_Hidden_Access;
   end record;

   overriding procedure Free (Socket : in out Socket_Type);
   --  Release memory associated with the socket object

end AWS.Net.Std;
