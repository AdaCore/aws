------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2002                          --
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

--  This is two implementations for this spec. One for standard sockets and
--  one for SSL socket. Note that the SSL implementation does support standard
--  socket too, this is controlled with the Security boolean on rountine
--  below. The corresponding implementation will be selected at build time.

with Ada.Streams;

package AWS.Net is

   use Ada.Streams;

   Socket_Error : exception;
   --  Raised by all routines below, a message will indicate the nature of
   --  the error.

   type Socket_Type is abstract tagged private;
   type Socket_Access is access all Socket_Type'Class;

   ----------------
   -- Initialize --
   ----------------

   function Socket (Security : in Boolean) return Socket_Type'Class;
   --  Create an unitialized socket.
   --  This socket could be used as is in the New_Socket parameter
   --  in the Accept_Socket routine.
   --  For the use this socket in the Connect it is necessary to call Create
   --  first.

   function Socket (Security : in Boolean) return Socket_Access;
   --  Create a dynamically allocated unitialized socket.

   procedure Bind
     (Socket : in out Socket_Type;
      Port   : in     Natural;
      Host   : in     String := "")
      is abstract;
   --  Bind a socket on a given port.
   --  Create a server socket if necessary.

   procedure Listen
     (Socket     : in Socket_Type;
      Queue_Size : in Positive := 5)
      is abstract;
   --  Set the queue size of the socket

   procedure Accept_Socket
     (Socket     : in     Socket_Type'Class;
      New_Socket :    out Socket_Type)
      is abstract;
   --  Accept a connection on a socket. If it raises Socket_Error,
   --  all resources used by new_Socket have been released.
   --  There is not need to call Free or Shutdown.

   procedure Connect
     (Socket   :    out Socket_Type;
      Host     : in     String;
      Port     : in     Positive)
      is abstract;
   --  Connect a socket on a given host/port.

   procedure Shutdown (Socket : in Socket_Type) is abstract;
   --  Shutdown both side of the socket and close it. Does not raise
   --  Socket_Error if the socket is not connected.

   procedure Free (Socket : in out Socket_Type) is abstract;
   --  Release memory associated with the socket object

   procedure Free (Socket : in out Socket_Access);
   --  Release memory associated with the socket access and socket object
   --  implemntation.

   --------
   -- IO --
   --------

   procedure Send
     (Socket : in Socket_Type;
      Data   : in Stream_Element_Array)
      is abstract;
   pragma Inline (Send);
   --  Send Data chunk to the socket

   function Receive
     (Socket : in Socket_Type;
      Max    : in Stream_Element_Count := 4096)
      return Stream_Element_Array
      is abstract;
   pragma Inline (Receive);
   --  Read a chunk of data from the socket and returns it. It always return
   --  something and then must wait for available data on the socket.

   ------------
   -- Others --
   ------------

   function Get_FD (Socket : in Socket_Type) return Integer
     is abstract;
   --  Returns the file descriptor associated with the socket

   function Peer_Addr (Socket : in Socket_Type) return String
     is abstract;
   --  Returns the peer name/address

   function Host_Name return String;
   --  Returns the running host name

private
   --  This object is to cache data writed to the stream. It is more efficient
   --  than to write byte by byte on the stream.

   W_Cache_Size  : constant := 2_048;
   W_Cache_Chunk : constant := W_Cache_Size / 2;
   --  This is write the cache size, when the cache is full W_Cache_Chunk
   --  bytes will be sent to the socket. This way we avoid flushing a single
   --  byte as this is not efficient at all with SSL sockets.

   R_Cache_Size : constant := 4_096;
   --  This is the read cache size, all data read on the socket are first put
   --  into a read cache, this makes reading char-by-char the socket more
   --  efficient. Before reading data, the write cache  is flushed.

   type Cache (Max_Size : Stream_Element_Count) is record
      Buffer : Stream_Element_Array (1 .. Max_Size);
      First  : Stream_Element_Offset := 1;
      Last   : Stream_Element_Offset := 0;
      Size   : Stream_Element_Count  := 0;
   end record;

   type RW_Cache is record
      W_Cache : Cache (W_Cache_Size);
      R_Cache : Cache (R_Cache_Size);
   end record;

   type RW_Cache_Access is access RW_Cache;

   type Socket_Type is abstract tagged record
      C : RW_Cache_Access;
   end record;

   procedure Set_Cache (Socket : in out Socket_Type'Class);
   --  Allocate cache object

   procedure Release_Cache (Socket : in out Socket_Type'Class);
   --  Release cache object memory

end AWS.Net;
