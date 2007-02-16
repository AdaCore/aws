------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2007                          --
--                                 AdaCore                                  --
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

--  This is two implementations for this spec. One for standard sockets and
--  one for SSL socket. Note that the SSL implementation does support standard
--  socket too, this is controlled with the Security boolean on rountine
--  below. The corresponding implementation will be selected at build time.

with Ada.Exceptions;
with Ada.Finalization;
with Ada.Streams;

with AWS.Utils;

package AWS.Net is

   use Ada;
   use Ada.Streams;
   use Ada.Exceptions;

   Socket_Error : exception;
   --  Raised by all routines below, a message will indicate the nature of
   --  the error.

   type Socket_Type is abstract tagged private;
   type Socket_Access is access all Socket_Type'Class;

   type Socket_Set is array (Positive range <>) of Socket_Access;

   subtype FD_Type is Integer;
   --  Represents an external socket file descriptor

   type FD_Set (Size : Natural) is abstract tagged private;
   --  Abstract type for waiting of network events on group of sockets FD

   type Event_Type is (Error, Input, Output);
   --  Error  - socket is in error state.
   --  Input  - socket ready for read.
   --  Output - socket available for write.

   type Event_Set is array (Event_Type) of Boolean;
   --  Type for get result of events waiting

   subtype Wait_Event_Type is Event_Type range Input .. Output;
   type Wait_Event_Set is array (Wait_Event_Type) of Boolean;
   --  Type for set events to wait, note that Error event would be waited
   --  anyway.

   Forever : constant Duration;
   --  The longest delay possible on the implementation

   ----------------
   -- Initialize --
   ----------------

   function Socket (Security : in Boolean) return Socket_Type'Class;
   --  Create an uninitialized socket

   function Socket (Security : in Boolean) return Socket_Access;
   --  Create a dynamically allocated uninitialized socket

   procedure Bind
     (Socket : in out Socket_Type;
      Port   : in     Natural;
      Host   : in     String := "") is abstract;
   --  Create the server socket and bind it on the given port.
   --  Using 0 for the port will tell the OS to allocate a non-privileged
   --  free port. The port can be later retrieved using Get_Port on the
   --  bound socket.

   procedure Listen
     (Socket : in Socket_Type; Queue_Size : in Positive := 5) is abstract;
   --  Set the queue size of the socket

   procedure Accept_Socket
     (Socket : in Socket_Type'Class; New_Socket : in out Socket_Type)
      is abstract;
   --  Accept a connection on a socket. If it raises Socket_Error, all
   --  resources used by new_Socket have been released.
   --  There is not need to call Free or Shutdown.

   type Socket_Constructor is access
     function (Security : in Boolean) return Socket_Type'Class;

   procedure Connect
     (Socket : in out Socket_Type;
      Host   : in     String;
      Port   : in     Positive;
      Wait   : in     Boolean := True) is abstract;
   --  Connect a socket on a given host/port. If Wait is True Connect will wait
   --  for the connection to be established for timeout seconds, specified by
   --  Set_Timeout routine. If Wait is False Connect will return immediately,
   --  not waiting for the connection to be establised. It is possible to wait
   --  for the Connection completion by calling Wait routine with Output set to
   --  True in Events parameter.

   procedure Socket_Pair (S1, S2 : out Socket_Type);
   --  Create 2 sockets and connect them together

   procedure Shutdown (Socket : in Socket_Type) is abstract;
   --  Shutdown both side of the socket and close it. Does not raise
   --  Socket_Error if the socket is not connected.

   procedure Free (Socket : in out Socket_Access);
   --  Release memory associated with the socket

   --------
   -- IO --
   --------

   procedure Send
     (Socket : in Socket_Type'Class; Data : in Stream_Element_Array);
   --  Send Data chunk to the socket

   procedure Send
     (Socket : in     Socket_Type;
      Data   : in     Stream_Element_Array;
      Last   :    out Stream_Element_Offset) is abstract;
   --  Try to place data to Socket's output buffer.
   --  If all data cannot be placed to the socket output buffer, Last will
   --  be lower than Data'Last, if no data has been placed into the output
   --  buffer, Last is set to be out of Data'Range, either Data'First - 1
   --  or Stream_Element_Offset'Last is used.

   procedure Receive
     (Socket : in     Socket_Type;
      Data   :    out Stream_Element_Array;
      Last   :    out Stream_Element_Offset) is abstract;
   --  Read a chunk of data from the socket and set appropriate Last value.
   --  This call always returns some data and will wait for incoming data only
   --  if necessary.

   function Receive
     (Socket : in Socket_Type'Class;
      Max    : in Stream_Element_Count := 4096) return Stream_Element_Array;
   --  Read a chunk of data from the socket and returns it. This call always
   --  returns some data and will wait for incoming data only if necessary.

   function Pending (Socket : in Socket_Type) return Stream_Element_Count
      is abstract;
   --  Returns the number of bytes which are available inside socket
   --  for immediate read.

   ------------
   -- Others --
   ------------

   function Get_FD (Socket : in Socket_Type) return FD_Type is abstract;
   --  Returns the file descriptor associated with the socket

   function Peer_Addr (Socket : in Socket_Type) return String is abstract;
   --  Returns the peer name/address

   function Peer_Port (Socket : in Socket_Type) return Positive is abstract;
   --  Returns the port of the peer socket

   function Get_Addr (Socket : in Socket_Type) return String is abstract;
   --  Returns the name/address of the socket

   function Get_Port (Socket : in Socket_Type) return Positive is abstract;
   --  Returns the port of the socket

   function Host_Name return String;
   --  Returns the running host name

   procedure Set_Send_Buffer_Size
     (Socket : in Socket_Type; Size : in Natural) is abstract;
   --  Set the internal socket send buffer size.
   --  Do not confuse with buffers for the AWS.Net.Buffered operations.

   procedure Set_Receive_Buffer_Size
     (Socket : in Socket_Type; Size : in Natural) is abstract;
   --  Set the internal socket receive buffer size.
   --  Do not confuse with buffers for the AWS.Net.Buffered operations.

   function Get_Send_Buffer_Size (Socket : in Socket_Type) return Natural
      is abstract;
   --  Returns the internal socket send buffer size.
   --  Do not confuse with buffers for the AWS.Net.Buffered operations.

   function Get_Receive_Buffer_Size (Socket : in Socket_Type) return Natural
      is abstract;
   --  Returns the internal socket receive buffer size.
   --  Do not confuse with buffers for the AWS.Net.Buffered operations.

   procedure Set_Blocking_Mode
     (Socket : in out Socket_Type; Blocking : in Boolean);
   pragma Obsolescent ("Use Set_Timeout instead");
   --  Set the blocking mode for the socket

   procedure Set_Timeout (Socket : in out Socket_Type; Timeout : in Duration);
   pragma Inline (Set_Timeout);
   --  Sets the timeout for the socket read/write operations

   procedure Set_No_Delay
     (Socket : in Socket_Type; Value : in Boolean := True);
   --  Set/clear TCP_NODELAY option on socket

   function Wait
     (Socket : in Socket_Type'Class;
      Events : in Wait_Event_Set) return Event_Set;
   --  Waiting for Input/Output/Error events.
   --  Waiting time is defined by Set_Timeout.
   --  Empty event set in result mean that timeout occured.

   function Check
     (Socket : in Socket_Type'Class;
      Events : in Wait_Event_Set) return Event_Set;
   --  Check for Input/Output/Error events availability.
   --  No wait for socket timeout.

   function Poll
     (Socket  : in Socket_Type'Class;
      Events  : in Wait_Event_Set;
      Timeout : in Duration) return Event_Set;
   --  Wait events on socket descriptor for specified Timeout

   function Errno (Socket : in Socket_Type) return Integer is abstract;
   --  Returns and clears error state in socket

   function Is_Timeout (E : in Exception_Occurrence) return Boolean;
   --  Returns True if the message associated with the Exception_Occurence for
   --  a Socket_Error is a timeout.

   function To_FD_Set
     (Socket : in Socket_Type;
      Events : in Wait_Event_Set;
      Size   : in Positive := 1) return FD_Set'Class;
   --  Create appropriate socket FD set and put Socket fd there.

   --------------------
   -- Socket FD sets --
   --------------------

   type FD_Set_Access is access all FD_Set'Class;

   procedure Add
     (FD_Set : in out FD_Set_Access;
      FD     : in     FD_Type;
      Event  : in     Wait_Event_Set);
   --  Add FD to the end of FD_Set

   procedure Free (FD_Set : in out FD_Set_Access);
   pragma Inline (Free);
   --  Deallocate the socket FD set

   procedure Add
     (FD_Set : in out Net.FD_Set;
      FD     : in     FD_Type;
      Event  : in     Wait_Event_Set) is abstract;
   --  Add FD to the end of FD_Set

   procedure Set_Mode
     (FD_Set : in out Net.FD_Set;
      Index  : in     Positive;
      Mode   : in     Wait_Event_Set) is abstract;
   --  Sets the kind of network events to wait for

   function Copy
     (FD_Set : access Net.FD_Set;
      Size   : in     Natural) return FD_Set_Access is abstract;
   --  Allocates and copy the given FD_Set with different size

   procedure Remove
     (FD_Set : in out Net.FD_Set; Index : in Positive) is abstract;
   --  Removes socket FD from Index position.
   --  Last socket FD in FD_Set is placed at position Index.

   function Length (FD_Set : in Net.FD_Set) return Natural is abstract;
   --  Returns number of socket FD elements in FD_Set

   procedure Wait
     (FD_Set  : in out Net.FD_Set;
      Timeout : in     Duration;
      Count   :    out Natural) is abstract;
   --  Wait for network events on the sockets FD set. Count value is the
   --  number of socket FDs with non empty event set.

   procedure Next
     (FD_Set : in Net.FD_Set; Index : in out Positive) is abstract;
   --  Looking for an active (for which an event has been detected by routine
   --  Wait above) socket FD starting from Index and return Index of the found
   --  active socket FD. Use functions Status to retreive the kind of network
   --  events for this socket.

   function Status
     (FD_Set : in Net.FD_Set;
      Index  : in Positive) return Event_Set is abstract;
   --  Returns events for the socket FD at position Index

private

   type FD_Set (Size : Natural) is abstract tagged null record;

   procedure Wait_For
     (Mode   : in Wait_Event_Type;
      Socket : in Socket_Type'Class);
   --  Wait for a socket to be ready for input or output operation.
   --  Raises Socket_Error if an error or timeout occurs.

   --  This object is to cache data writed to the stream. It is more efficient
   --  than to write byte by byte on the stream.

   W_Cache_Size  : constant := 2_048;
   --  This is write the cache size, when the cache is full W_Cache_Size
   --  bytes will be sent to the socket. This way we avoid flushing a single
   --  byte as this is not efficient at all with SSL sockets.

   R_Cache_Size : constant := 4_096;
   --  This is the read cache size, all data read on the socket are first put
   --  into a read cache, this makes reading char-by-char the socket more
   --  efficient. Before reading data, the write cache  is flushed.

   Forever : constant Duration := Duration'Last;

   type Read_Cache (Max_Size : Stream_Element_Count) is record
      Buffer : Stream_Element_Array (1 .. Max_Size);
      First  : Stream_Element_Offset := 1;
      Last   : Stream_Element_Offset := 0;
   end record;

   type Write_Cache (Max_Size : Stream_Element_Count) is record
      Buffer : Stream_Element_Array (1 .. Max_Size);
      Last   : Stream_Element_Offset := 0;
   end record;

   type RW_Cache is record
      Ref_Count : Utils.Counter (Initial_Value => 1);
      R_Cache   : Read_Cache (R_Cache_Size);
      W_Cache   : Write_Cache (W_Cache_Size);
   end record;

   type RW_Cache_Access is access RW_Cache;

   type Socket_Type is abstract new Finalization.Controlled with record
      C       : RW_Cache_Access;
      Timeout : Duration        := Forever;
   end record;

   procedure Free (Socket : in out Socket_Type) is null;
   --  Release memory associated with the socket object. This default version
   --  can be overriden to properly release the memory for the derived
   --  implementation. The controlled Finalize routine is in charge of calling
   --  Release.

   function Errno return Integer;
   --  Return error code for the last socket operation

   procedure Raise_Socket_Error
     (Socket : in Socket_Type'Class; Text : in String);
   pragma No_Return (Raise_Socket_Error);

   --  Controlled primitives

   overriding procedure Initialize (Socket : in out Socket_Type);
   overriding procedure Adjust     (Socket : in out Socket_Type);
   overriding procedure Finalize   (Socket : in out Socket_Type);

end AWS.Net;
