------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2016, AdaCore                     --
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

--  There is two implementations for this spec. One for standard sockets and
--  one for SSL socket. Note that the SSL implementation does support standard
--  socket too, this is controlled with the Security boolean on rountine
--  below. The corresponding implementation will be selected at build time.

with Ada.Exceptions;
with Ada.Finalization;
with Ada.Streams;

private with AWS.Utils;
private with Interfaces.C;

package AWS.Net is

   use Ada;
   use Ada.Exceptions;
   use Ada.Streams;

   Socket_Error : exception;
   --  Raised by all routines below, a message will indicate the nature of
   --  the error.

   type Socket_Type is abstract new Finalization.Controlled with private;
   type Socket_Access is access all Socket_Type'Class;

   type Socket_Set is array (Positive range <>) of Socket_Access;

   subtype FD_Type is Integer;
   --  Represents an external socket file descriptor

   No_Socket : constant := -1;
   --  Represents closed socket file descriptor

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

   type Family_Type is (Family_Inet, Family_Inet6, Family_Unspec);

   type Shutmode_Type is (Shut_Read, Shut_Write, Shut_Read_Write);

   Forever : constant Duration;
   --  The longest delay possible on the implementation

   ----------------
   -- Initialize --
   ----------------

   function Socket (Security : Boolean) return Socket_Type'Class;
   --  Create an uninitialized socket

   function Socket
     (Security : Boolean) return not null access Socket_Type'Class;
   --  Create a dynamically allocated uninitialized socket

   procedure Bind
     (Socket        : in out Socket_Type;
      Port          : Natural;
      Host          : String      := "";
      Reuse_Address : Boolean     := False;
      IPv6_Only     : Boolean     := False;
      Family        : Family_Type := Family_Unspec) is abstract;
   --  Create the server socket and bind it on the given port.
   --  Using 0 for the port will tell the OS to allocate a non-privileged
   --  free port. The port can be later retrieved using Get_Port on the
   --  bound socket.
   --  IPv6_Only has meaning only for Family = Family_Inet6 and mean that only
   --  IPv6 clients allowed to connect.

   procedure Listen
     (Socket : Socket_Type; Queue_Size : Positive := 5) is abstract;
   --  Set the queue size of the socket

   procedure Accept_Socket
     (Socket : Socket_Type'Class; New_Socket : in out Socket_Type) is abstract;
   --  Accept a connection on a socket. If it raises Socket_Error, all
   --  resources used by new_Socket have been released.
   --  There is not need to call Free or Shutdown.

   type Socket_Constructor is not null access
     function (Security : Boolean) return Socket_Type'Class;

   procedure Connect
     (Socket : in out Socket_Type;
      Host   : String;
      Port   : Positive;
      Wait   : Boolean     := True;
      Family : Family_Type := Family_Unspec) is abstract
   with Pre'Class => Host'Length > 0;
   --  Connect a socket on a given host/port. If Wait is True Connect will wait
   --  for the connection to be established for timeout seconds, specified by
   --  Set_Timeout routine. If Wait is False Connect will return immediately,
   --  not waiting for the connection to be establised. It is possible to wait
   --  for the Connection completion by calling Wait routine with Output set to
   --  True in Events parameter.

   procedure Socket_Pair (S1, S2 : out Socket_Type);
   --  Create 2 sockets and connect them together

   procedure Shutdown
     (Socket : Socket_Type;
      How    : Shutmode_Type := Shut_Read_Write) is abstract;
   --  Shutdown the read, write or both side of the socket.
   --  If How is Both, close it. Does not raise Socket_Error if the socket is
   --  not connected or already shutdown.

   procedure Free (Socket : in out Socket_Access);
   --  Release memory associated with the socket

   --------
   -- IO --
   --------

   procedure Send
     (Socket : Socket_Type'Class; Data : Stream_Element_Array);
   --  Send Data chunk to the socket

   procedure Send
     (Sockets : Socket_Set; Data : Stream_Element_Array);
   --  Send Data to all sockets from the socket set. This call will ensure that
   --  the data are sent in priority to client waiting for reading. That is,
   --  slow connection for one sokcet should not delay the fast connections.
   --  Yet, this routine will return only when the data is sent to all sockets.

   procedure Send
     (Socket : Socket_Type;
      Data   : Stream_Element_Array;
      Last   : out Stream_Element_Offset) is abstract;
   --  Try to place data to Socket's output buffer. If all data cannot be
   --  placed to the socket output buffer, Last will be lower than Data'Last,
   --  if no data has been placed into the output buffer, Last is set to
   --  Data'First - 1. If Data'First is equal to Stream_Element_Offset'First
   --  then constraint error is raised to follow advice in AI95-227.

   procedure Receive
     (Socket : Socket_Type;
      Data   : out Stream_Element_Array;
      Last   : out Stream_Element_Offset) is abstract;
   --  Read a chunk of data from the socket and set appropriate Last value.
   --  This call always returns some data and will wait for incoming data only
   --  if necessary.

   function Receive
     (Socket : Socket_Type'Class;
      Max    : Stream_Element_Count := 4096) return Stream_Element_Array;
   --  Read a chunk of data from the socket and returns it. This call always
   --  returns some data and will wait for incoming data only if necessary.

   function Pending (Socket : Socket_Type) return Stream_Element_Count
      is abstract;
   --  Returns the number of bytes which are available inside socket
   --  for immediate read.

   function Output_Space (Socket : Socket_Type) return Stream_Element_Offset;
   --  Returns the free space in output buffer in bytes. If OS could not
   --  provide such information, routine returns -1.

   function Output_Busy (Socket : Socket_Type) return Stream_Element_Offset;
   --  How many bytes in the send queue. If OS could not provide such
   --  information, routine returns -1.

   ------------
   -- Others --
   ------------

   function Get_FD (Socket : Socket_Type) return FD_Type is abstract;
   --  Returns the file descriptor associated with the socket

   function Peer_Addr (Socket : Socket_Type) return String is abstract;
   --  Returns the peer name/address

   function Peer_Port (Socket : Socket_Type) return Positive is abstract;
   --  Returns the port of the peer socket

   function Get_Addr (Socket : Socket_Type) return String is abstract;
   --  Returns the name/address of the socket

   function Get_Port (Socket : Socket_Type) return Positive is abstract;
   --  Returns the port of the socket

   function Is_Any_Address (Socket : Socket_Type) return Boolean;
   --  Return true if the socket accepts connections on any of the hosts's
   --  network addresses.

   function Is_IPv6 (Socket : Socket_Type) return Boolean;

   function Is_Listening (Socket : Socket_Type) return Boolean;
   --  Returns true if the socket has been marked to accept connections with
   --  listen.

   function IPv6_Available return Boolean;
   --  Returns True if IPv6 available in OS and in AWS socket implementation

   function Host_Name return String;
   --  Returns the running host name

   procedure Set_Send_Buffer_Size
     (Socket : Socket_Type; Size : Natural) is abstract;
   --  Set the internal socket send buffer size.
   --  Do not confuse with buffers for the AWS.Net.Buffered operations.

   procedure Set_Receive_Buffer_Size
     (Socket : Socket_Type; Size : Natural) is abstract;
   --  Set the internal socket receive buffer size.
   --  Do not confuse with buffers for the AWS.Net.Buffered operations.

   function Get_Send_Buffer_Size (Socket : Socket_Type) return Natural
      is abstract;
   --  Returns the internal socket send buffer size.
   --  Do not confuse with buffers for the AWS.Net.Buffered operations.

   function Get_Receive_Buffer_Size (Socket : Socket_Type) return Natural
      is abstract;
   --  Returns the internal socket receive buffer size.
   --  Do not confuse with buffers for the AWS.Net.Buffered operations.

   function Cipher_Description (Socket : Socket_Type) return String;
   --  Returns cipher description on SSL implementation or empty string on
   --  plain socket.

   procedure Set_Blocking_Mode
     (Socket : in out Socket_Type; Blocking : Boolean);
   pragma Obsolescent ("Use Set_Timeout instead");
   --  Set the blocking mode for the socket

   procedure Set_Timeout (Socket : in out Socket_Type; Timeout : Duration)
     with Inline;
   --  Sets the timeout for the socket read/write operations

   procedure Set_No_Delay
     (Socket : Socket_Type; Value : Boolean := True) is null;
   --  Set/clear TCP_NODELAY option on socket

   function Wait
     (Socket : Socket_Type'Class;
      Events : Wait_Event_Set) return Event_Set;
   --  Waiting for Input/Output/Error events.
   --  Waiting time is defined by Set_Timeout.
   --  Empty event set in result mean that timeout occured.

   function Check
     (Socket : Socket_Type'Class;
      Events : Wait_Event_Set) return Event_Set;
   --  Check for Input/Output/Error events availability.
   --  No wait for socket timeout.

   function Poll
     (Socket  : Socket_Type'Class;
      Events  : Wait_Event_Set;
      Timeout : Duration) return Event_Set;
   --  Wait events on socket descriptor for specified Timeout

   function Errno (Socket : Socket_Type) return Integer is abstract;
   --  Returns and clears error state in socket

   function Is_Timeout
     (Socket : Socket_Type;
      E      : Exception_Occurrence) return Boolean;
   --  Returns True if the message associated with the Exception_Occurence for
   --  a Socket_Error is a timeout.

   function Is_Timeout (E : Exception_Occurrence) return Boolean;
   --  As above but without Socket parameter

   function Is_Peer_Closed
     (Socket : Socket_Type;
      E      : Exception_Occurrence) return Boolean;
   --  Returns True if the message associated with the Exception_Occurence for
   --  a Socket_Error is a "socket closed by peer".

   --------------------
   -- Socket FD sets --
   --------------------

   type FD_Set (Size : Natural) is abstract tagged private;
   --  Abstract type for waiting of network events on group of sockets FD

   type FD_Set_Access is access all FD_Set'Class;

   function To_FD_Set
     (Socket : Socket_Type;
      Events : Wait_Event_Set;
      Size   : Positive := 1) return FD_Set'Class;
   --  Create appropriate socket FD set and put Socket fd there

   procedure Add
     (FD_Set : in out FD_Set_Access;
      FD     : FD_Type;
      Event  : Wait_Event_Set);
   --  Add FD to the end of FD_Set

   procedure Free (FD_Set : in out FD_Set_Access) with Inline;
   --  Deallocate the socket FD set

   procedure Add
     (FD_Set : in out Net.FD_Set;
      FD     : FD_Type;
      Event  : Wait_Event_Set) is abstract;
   --  Add FD to the end of FD_Set

   procedure Replace
     (FD_Set : in out Net.FD_Set;
      Index  : Positive;
      FD     : FD_Type) is abstract
   with Pre'Class => Index <= Length (FD_Set);
   --  Replaces the socket FD in FD_Set

   procedure Set_Mode
     (FD_Set : in out Net.FD_Set;
      Index  : Positive;
      Mode   : Wait_Event_Set) is abstract
   with Pre'Class => Index <= Length (FD_Set);
   --  Sets the kind of network events to wait for

   procedure Set_Event
     (FD_Set : in out Net.FD_Set;
      Index  : Positive;
      Event  : Wait_Event_Type;
      Value  : Boolean) is abstract
   with Pre'Class => Index <= Length (FD_Set);

   function Copy
     (FD_Set : not null access Net.FD_Set;
      Size   : Natural) return FD_Set_Access is abstract;
   --  Allocates and copy the given FD_Set with different size

   procedure Remove
     (FD_Set : in out Net.FD_Set; Index : Positive) is abstract
   with Pre'Class => Index <= Length (FD_Set);
   --  Removes socket FD from Index position.
   --  Last socket FD in FD_Set is placed at position Index.

   function Length (FD_Set : Net.FD_Set) return Natural is abstract;
   --  Returns number of socket FD elements in FD_Set

   procedure Wait
     (FD_Set  : in out Net.FD_Set;
      Timeout : Duration;
      Count   : out Natural) is abstract
   with Post'Class => Count <= Length (FD_Set);
   --  Wait for network events on the sockets FD set. Count value is the
   --  number of socket FDs with non empty event set.

   procedure Next
     (FD_Set : Net.FD_Set; Index : in out Positive) is abstract
   with
     Pre'Class  => Index <= Length (FD_Set) + 1,
     Post'Class => Index <= Length (FD_Set) + 1;
   --  Looking for an active (for which an event has been detected by routine
   --  Wait above) socket FD starting from Index and return Index of the found
   --  active socket FD. Use functions Status to retreive the kind of network
   --  events for this socket.

   function Status
     (FD_Set : Net.FD_Set;
      Index  : Positive) return Event_Set is abstract
   with Pre'Class => Index <= Length (FD_Set);
   --  Returns events for the socket FD at position Index

   procedure Free (Socket : in out Socket_Type) is null;
   --  Release memory associated with the socket object. This default version
   --  can be overriden to properly release the memory for the derived
   --  implementation. The controlled Finalize routine is in charge of calling
   --  Free. We could not have it in the private part because we could not make
   --  AWS.Net.SSL.Free overriding this way.

   function Localhost (IPv6 : Boolean) return String;
   --  Returns "::1" if IPv6 is true or "127.0.0.1" otherwise

private

   type FD_Set (Size : Natural) is abstract tagged null record;

   procedure Wait_For
     (Mode : Wait_Event_Type; Socket : Socket_Type'Class; Timeout : Duration);
   --  Wait for a socket to be ready for input or output operation.
   --  Raises Socket_Error if an error or timeout occurs.

   procedure Wait_For (Mode : Wait_Event_Type; Socket : Socket_Type'Class);
   --  Idem, but use socket timeout

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

   Peer_Closed_Message : constant String := "Receive : Socket closed by peer";

   function Get_Socket_Errno (E : Exception_Occurrence) return Natural;
   --  Returns the errno recorded into the exception message

   type Read_Cache (Max_Size : Stream_Element_Count) is record
      Buffer : Stream_Element_Array (1 .. Max_Size);
      First  : Stream_Element_Offset := 1;
      Last   : Stream_Element_Offset := 0;
   end record;

   type Read_Cache_Access is access Read_Cache;

   type Write_Cache (Max_Size : Stream_Element_Count) is record
      Buffer : Stream_Element_Array (1 .. Max_Size);
      Last   : Stream_Element_Offset := 0;
   end record;

   type Write_Cache_Access is access Write_Cache;

   type RW_Data is record
      Ref_Count : Utils.Counter (Initial_Value => 1);
      Listening : Boolean  := False; -- True if a listening (server) socket
      R_Cache   : Read_Cache_Access;
      W_Cache   : Write_Cache_Access;
      Can_Wait  : Boolean := False; -- Need for OpenSSL send in Mac OS
      Pack_Size : Stream_Element_Count := 2**15; -- Idem
   end record;

   type RW_Data_Access is access RW_Data;

   type Socket_Type is abstract new Finalization.Controlled with record
      C       : RW_Data_Access;
      Timeout : Duration := Forever;
   end record;

   procedure Raise_Socket_Error (Socket : Socket_Type'Class; Text : String)
     with No_Return;

   function Error_Message (Errno : Integer) return String;
   --  Returns the error message string for the error number Errno. If Errno is
   --  not known, returns "Unknown system error".

   --  Controlled primitives

   overriding procedure Initialize (Socket : in out Socket_Type);
   overriding procedure Adjust     (Socket : in out Socket_Type);
   overriding procedure Finalize   (Socket : in out Socket_Type);

   function Last_Index
     (First : Stream_Element_Offset;
      Count : Natural) return Ada.Streams.Stream_Element_Offset;
   --  Compute the Last OUT parameter for the various Send / Receive
   --  subprograms: returns First + Count - 1.
   --  When First = Stream_Element_Offset'First and Res = 0, Constraint_Error
   --  is raised. This is consistent with the semantics of stream operations
   --  as clarified in AI95-227.

   function IO_Control
     (Socket : Socket_Type;
      Code   : Interfaces.C.int) return Stream_Element_Offset;
   --  This routine is necessary for both sockets implementations because
   --  GNAT.Sockets support only 2 control codes (at least in GNAT GPL 2013).

end AWS.Net;
