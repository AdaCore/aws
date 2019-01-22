------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2004-2017, AdaCore                     --
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

with Ada.Unchecked_Deallocation;

with AWS.Net.Log;
with AWS.OS_Lib;
with AWS.Utils;

with Interfaces.C.Strings;
with System.Address_To_Access_Conversions;

package body AWS.Net.Std is

   use Interfaces;

   Failure : constant C.int := C.int (-1);

   type Unsigned_Lock is mod 2**8;
   pragma Atomic (Unsigned_Lock);

   type Socket_Hidden is record
      FD : C.int := No_Socket;
      RL : aliased Unsigned_Lock := 0; -- Flag to detect read competition
   end record;

   subtype In6_Addr is OS_Lib.In6_Addr;

   To_C : constant array (Family_Type) of C.int :=
     (Family_Inet => OS_Lib.AF_INET, Family_Inet6 => OS_Lib.AF_INET6,
      Family_Unspec => OS_Lib.AF_UNSPEC);

   subtype Sockaddr_In6 is OS_Lib.Sockaddr_In6;

   package AC6 is new System.Address_To_Access_Conversions (Sockaddr_In6);

   function Lock_Set
     (Ptr : access Unsigned_Lock; Set : Unsigned_Lock) return Unsigned_Lock
     with Import, Convention => Intrinsic,
          External_Name => "__sync_lock_test_and_set_1";

   procedure Raise_Socket_Error (Error : Integer)
     with No_Return, Inline;

   procedure Raise_Socket_Error (Error : Integer; Socket : Socket_Type)
     with No_Return, Inline;

   procedure Raise_Socket_Error (Errmsg : String)
     with No_Return, Inline;
   --  Log socket error and raise exception

   function Image (Sin6 : Sockaddr_In6; Len : OS_Lib.socklen_t) return String;
   --  Returns image of the socket address

   function Get_Addr_Info
     (Host   : String;
      Port   : Natural;
      Family : Family_Type;
      Flags  : C.int := 0) return not null OS_Lib.Addr_Info_Access;
   --  Returns the inet address information for the given host and port.
   --  Flags should be used from getaddrinfo C routine.

   function Select_IPv6_If_Present
     (Chain : not null OS_Lib.Addr_Info_Access)
      return not null OS_Lib.Addr_Info_Access;
   --  Choose IPv6 address if it exists in the chain. This function is needed
   --  because getaddrinfo returns IPv4 addresses first for Passive flag in
   --  hints and IPv6 addresses first for calls without Passive flag. Without
   --  this call the server is going to be bound to IPv4 address even if IPv6
   --  local address exists.

   function Get_Int_Sock_Opt
     (Socket : Socket_Type; Name : C.int) return Integer;
   --  Return socket option with Integer size

   procedure Set_Int_Sock_Opt
     (Socket : Socket_Type;
      Name   : C.int;
      Value  : Integer;
      Level  : C.int := OS_Lib.SOL_SOCKET);
   --  Return socket option with Integer size

   procedure Set_Non_Blocking_Mode (Socket : Socket_Type);
   --  Set the socket to the non-blocking mode.
   --  AWS is not using blocking sockets internally.

   function Swap_Little_Endian (S : Unsigned_16) return Unsigned_16;

   function C_Bind
     (S : C.int; Name : System.Address; Namelen : C.int) return C.int
     with Import, Convention => Stdcall, External_Name => "bind";

   function C_Socket (Domain, Typ, Protocol : C.int) return C.int
     with Import, Convention => Stdcall, External_Name => "socket";

   function C_Getsockname
     (S       : C.int;
      Name    : System.Address;
      Namelen : not null access OS_Lib.socklen_t) return C.int
     with Import, Convention => Stdcall, External_Name => "getsockname";

   function C_Getsockopt
     (S       : C.int;
      Level   : C.int;
      OptName : C.int;
      OptVal  : System.Address;
      OptLen  : not null access C.int) return C.int
     with Import, Convention => Stdcall, External_Name => "getsockopt";

   function C_Getpeername
     (S       : C.int;
      Name    : System.Address;
      Namelen : not null access OS_Lib.socklen_t) return C.int
     with Import, Convention => Stdcall, External_Name => "getpeername";

   function C_Gethostname
     (Name : System.Address; Namelen : C.int) return C.int
     with Import, Convention => Stdcall, External_Name => "gethostname";

   -------------------
   -- Accept_Socket --
   -------------------

   overriding procedure Accept_Socket
     (Socket : Net.Socket_Type'Class; New_Socket : in out Socket_Type)
   is
      use type C.int;

      function C_Accept
        (S       : Integer;
         Addr    : System.Address;
         Addrlen : not null access C.int) return C.int
        with Import, Convention => Stdcall, External_Name => "accept";

      Dummy : String (1 .. 32);
      Len   : aliased C.int := Dummy'Length;

   begin
      if New_Socket.S /= null then
         New_Socket := Socket_Type'(Net.Socket_Type with others => <>);
      end if;

      New_Socket.S := new Socket_Hidden;

      Wait_For (Input, Socket);

      New_Socket.S.FD := C_Accept (Get_FD (Socket), Dummy'Address, Len'Access);

      if New_Socket.S.FD = Failure then
         Raise_Socket_Error (OS_Lib.Socket_Errno, Socket_Type (Socket));
      end if;

      if Net.Log.Is_Event_Active then
         Net.Log.Event (Net.Log.Accept_Socket, New_Socket);
      end if;

      Set_Non_Blocking_Mode (New_Socket);
   end Accept_Socket;

   ----------
   -- Bind --
   ----------

   overriding procedure Bind
     (Socket        : in out Socket_Type;
      Port          : Natural;
      Host          : String      := "";
      Reuse_Address : Boolean     := False;
      IPv6_Only     : Boolean     := False;
      Family        : Family_Type := Family_Unspec)
   is
      use type C.int;

      Raw   : constant not null OS_Lib.Addr_Info_Access :=
                Get_Addr_Info (Host, Port, Family, OS_Lib.AI_PASSIVE);
      Info  : constant not null OS_Lib.Addr_Info_Access :=
                (if Family = Family_Unspec
                 then Select_IPv6_If_Present (Raw)
                 else Raw);
      FD    : C.int;
      Res   : C.int;
      Errno : Integer;

   begin
      if Socket.S /= null then
         Socket := Socket_Type'(Net.Socket_Type with others => <>);
      end if;

      FD := C_Socket (Info.ai_family, Info.ai_socktype, Info.ai_protocol);

      if FD = Failure then
         OS_Lib.FreeAddrInfo (Raw);
         Raise_Socket_Error (OS_Lib.Socket_Errno);
      end if;

      Socket.S := new Socket_Hidden'(FD => FD, RL => 0);

      if Info.ai_family = OS_Lib.AF_INET6 then
         Set_Int_Sock_Opt
           (Socket, OS_Lib.IPV6_V6ONLY, Boolean'Pos (IPv6_Only),
            Level => OS_Lib.IPPROTO_IPV6);
      end if;

      if Reuse_Address then
         Set_Int_Sock_Opt (Socket, OS_Lib.SO_REUSEADDR, 1);
      end if;

      Res := C_Bind (FD, Info.ai_addr, C.int (Info.ai_addrlen));

      OS_Lib.FreeAddrInfo (Raw);

      if Res = Failure then
         Errno := OS_Lib.Socket_Errno;
         Res   := OS_Lib.C_Close (FD);
         Raise_Socket_Error (Errno, Socket);
      end if;

      Set_Non_Blocking_Mode (Socket);
   end Bind;

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
      use type C.int;

      Info  : constant OS_Lib.Addr_Info_Access :=
                Get_Addr_Info (Host, Port, Family);
      FD    : C.int;
      Res   : C.int;
      Errno : Integer;

      function C_Connect
        (S       : C.int;
         Name    : System.Address;
         Namelen : C.int) return C.int
        with Import, Convention => Stdcall, External_Name => "connect";

   begin
      if Socket.S /= null then
         Socket := Socket_Type'(Net.Socket_Type with others => <>);
      end if;

      FD := C_Socket (Info.ai_family, Info.ai_socktype, Info.ai_protocol);

      if FD = Failure then
         OS_Lib.FreeAddrInfo (Info);
         Raise_Socket_Error (OS_Lib.Socket_Errno);
      end if;

      Socket.S := new Socket_Hidden'(FD => FD, RL => 0);

      Set_Non_Blocking_Mode (Socket);

      Res := C_Connect (FD, Info.ai_addr, C.int (Info.ai_addrlen));

      if Res = Failure then
         Errno := OS_Lib.Socket_Errno;

         if Errno = OS_Lib.EWOULDBLOCK
           or else Errno = OS_Lib.EINPROGRESS
         then
            Errno := 0;

            if Wait then
               declare
                  Events : constant Event_Set
                    := Net.Wait (Socket, (Output => True, Input => False));
               begin
                  if Events (Error) then
                     Errno := Std.Errno (Socket);
                  elsif not Events (Output) then
                     Errno := OS_Lib.ETIMEDOUT;
                  end if;
               end;
            end if;
         end if;

         if Errno /= 0 then
            Res := OS_Lib.C_Close (FD);

            declare
               Errm : constant String := Error_Message (Errno);
               Addr : constant String :=
                 Image (AC6.To_Pointer (Info.ai_addr).all, Info.ai_addrlen);
            begin
               OS_Lib.FreeAddrInfo (Info);
               Raise_Socket_Error
                 (Socket,
                  Error_On_Connect (Errm)
                  & (if Host = Addr then "" else Host & ' ')
                  & (if Strings.Fixed.Index (Addr, ":") > 0
                     then '[' & Addr & ']' else Addr)
                  & ':' & Utils.Image (Port));
            end;
         end if;
      end if;

      OS_Lib.FreeAddrInfo (Info);

      if Net.Log.Is_Event_Active then
         Net.Log.Event (Net.Log.Connect, Socket);
      end if;
   end Connect;

   -----------
   -- Errno --
   -----------

   overriding function Errno (Socket : Socket_Type) return Integer is
   begin
      return Get_Int_Sock_Opt (Socket, OS_Lib.SO_ERROR);
   end Errno;

   ----------
   -- Free --
   ----------

   overriding procedure Free (Socket : in out Socket_Type) is
      procedure Free is
         new Ada.Unchecked_Deallocation (Socket_Hidden, Socket_Hidden_Access);
   begin
      Free (Socket.S);
   end Free;

   --------------
   -- Get_Addr --
   --------------

   overriding function Get_Addr (Socket : Socket_Type) return String is
      use type C.int;
      use type OS_Lib.socklen_t;

      Name : aliased Sockaddr_In6;
      Len  : aliased OS_Lib.socklen_t := Name'Size / 8;

   begin
      if C_Getsockname (Socket.S.FD, Name'Address, Len'Access) = Failure then
         Raise_Socket_Error (OS_Lib.Socket_Errno, Socket);
      end if;

      return Image (Name, Len);
   end Get_Addr;

   -------------------
   -- Get_Addr_Info --
   -------------------

   function Get_Addr_Info
     (Host   : String;
      Port   : Natural;
      Family : Family_Type;
      Flags  : C.int := 0) return not null OS_Lib.Addr_Info_Access
   is
      package CS renames Interfaces.C.Strings;
      use type C.int;

      C_Node : aliased C.char_array := C.To_C (Host);
      P_Node : CS.chars_ptr;
      A_Serv : constant String := AWS.Utils.Image (Port);
      C_Serv : aliased C.char_array := C.To_C (A_Serv);
      Res    : C.int;
      Result : aliased OS_Lib.Addr_Info_Access;
      Hints  : constant OS_Lib.Addr_Info :=
                 (ai_family    => To_C (Family),
                  ai_socktype  => OS_Lib.SOCK_STREAM,
                  ai_protocol  => OS_Lib.IPPROTO_IP,
                  ai_flags     => Flags,
                  ai_addrlen   => 0,
                  ai_canonname => CS.Null_Ptr,
                  ai_addr      => System.Null_Address,
                  ai_next      => null);
   begin
      if Port > Positive (Unsigned_16'Last) then
         raise Constraint_Error with "Port number too big";
      end if;

      if Host = "" then
         P_Node := CS.Null_Ptr;
      else
         P_Node := CS.To_Chars_Ptr (C_Node'Unchecked_Access);
      end if;

      Res := OS_Lib.GetAddrInfo
               (node    => P_Node,
                service => CS.To_Chars_Ptr (C_Serv'Unchecked_Access),
                hints   => Hints,
                res     => Result'Access);

      if Res = OS_Lib.EAI_SYSTEM then
         Raise_Socket_Error (OS_Lib.Socket_Errno);

      elsif Res /= 0 then
         declare
            Errm : constant String := CS.Value (OS_Lib.GAI_StrError (Res));
         begin
            Raise_Socket_Error
              ((if Errm (Errm'Last) = '.'
                then Errm (Errm'First .. Errm'Last - 1) else Errm)
               & ' ' & Host & ':' & A_Serv);
         end;
      end if;

      return Result;
   end Get_Addr_Info;

   ------------
   -- Get_FD --
   ------------

   overriding function Get_FD (Socket : Socket_Type) return Integer is
   begin
      if Socket.S = null then
         return No_Socket;
      else
         return Integer (Socket.S.FD);
      end if;
   end Get_FD;

   ----------------------
   -- Get_Int_Sock_Opt --
   ----------------------

   function Get_Int_Sock_Opt
     (Socket : Socket_Type; Name : C.int) return Integer
   is
      use type C.int;

      Res : aliased C.int := 0;
      Len : aliased C.int := Res'Size / System.Storage_Unit;

      RC  : constant C.int :=
              C_Getsockopt
                (S       => Socket.S.FD,
                 Level   => OS_Lib.SOL_SOCKET,
                 OptName => Name,
                 OptVal  => Res'Address,
                 OptLen  => Len'Access);
   begin
      if RC = Failure then
         Raise_Socket_Error (OS_Lib.Socket_Errno, Socket);
      end if;

      return Integer (Res);
   end Get_Int_Sock_Opt;

   --------------
   -- Get_Port --
   --------------

   overriding function Get_Port (Socket : Socket_Type) return Positive is
      use type C.int;
      use type OS_Lib.socklen_t;

      Name : aliased Sockaddr_In6;
      Len  : aliased OS_Lib.socklen_t := Name'Size / 8;

   begin
      if C_Getsockname (Socket.S.FD, Name'Address, Len'Access) = Failure then
         Raise_Socket_Error (OS_Lib.Socket_Errno, Socket);
      end if;

      return Positive (Swap_Little_Endian (Unsigned_16 (Name.Port)));
   end Get_Port;

   -----------------------------
   -- Get_Receive_Buffer_Size --
   -----------------------------

   overriding function Get_Receive_Buffer_Size
     (Socket : Socket_Type) return Natural is
   begin
      return Get_Int_Sock_Opt (Socket, OS_Lib.SO_RCVBUF);
   end Get_Receive_Buffer_Size;

   --------------------------
   -- Get_Send_Buffer_Size --
   --------------------------

   overriding function Get_Send_Buffer_Size
     (Socket : Socket_Type) return Natural is
   begin
      return Get_Int_Sock_Opt (Socket, OS_Lib.SO_SNDBUF);
   end Get_Send_Buffer_Size;

   ---------------
   -- Host_Name --
   ---------------

   function Host_Name return String is
      use type C.int;
      Name : aliased C.char_array (1 .. 64);
   begin
      if C_Gethostname (Name'Address, Name'Length) = Failure then
         Raise_Socket_Error (OS_Lib.Socket_Errno);
      end if;

      return C.To_Ada (Name);
   end Host_Name;

   -----------
   -- Image --
   -----------

   function Image
     (Sin6 : Sockaddr_In6; Len : OS_Lib.socklen_t) return String
   is
      use type C.int;
      package CS renames Interfaces.C.Strings;

      function getnameinfo
        (sa      : System.Address;
         salen   : OS_Lib.socklen_t;
         host    : CS.chars_ptr;
         hostlen : C.size_t;
         serv    : CS.chars_ptr;
         servlen : C.size_t;
         flags   : C.int) return C.int
        with Import, Convention => Stdcall, External_Name => "getnameinfo";

      Host : aliased C.char_array := (0 .. 128 => C.nul);
      Res  : constant C.int :=
               getnameinfo
                 (sa      => Sin6'Address,
                  salen   => Len,
                  host    => CS.To_Chars_Ptr (Host'Unchecked_Access),
                  hostlen => Host'Length,
                  serv    => CS.Null_Ptr,
                  servlen => 0,
                  flags   => OS_Lib.NI_NUMERICHOST);
   begin
      if Res = OS_Lib.EAI_SYSTEM then
         Raise_Socket_Error (OS_Lib.Socket_Errno);

      elsif Res /= 0 then
         Raise_Socket_Error (CS.Value (OS_Lib.GAI_StrError (Res)));
      end if;

      return C.To_Ada (Host);
   end Image;

   --------------------
   -- IPv6_Available --
   --------------------

   function IPv6_Available return Boolean is
      use type C.int;

      FD    : C.int;
      Res   : C.int;
      Errno : Integer;

      Addr : constant Sockaddr_In6 :=
        (Family => OS_Lib.AF_INET6,
         Addr   => (1 .. 7 => 0, In6_Addr'Last => Swap_Little_Endian (1)),
         others => <>);

   begin
      FD := C_Socket (OS_Lib.AF_INET6, OS_Lib.SOCK_STREAM, 0);

      if FD = Failure then
         --  Windows failed here in case of IPv6 unavailable
         return False;
      end if;

      Res := C_Bind (FD, Addr'Address, Addr'Size / System.Storage_Unit);

      if Res = Failure then
         Errno := OS_Lib.Socket_Errno;
         Res   := OS_Lib.C_Close (FD);

         if Errno = OS_Lib.EADDRNOTAVAIL then
            return False;
         else
            Raise_Socket_Error (Errno);
         end if;
      end if;

      Res := OS_Lib.C_Close (FD);

      return True;
   end IPv6_Available;

   --------------------
   -- Is_Any_Address --
   --------------------

   overriding function Is_Any_Address (Socket : Socket_Type) return Boolean is
      use type C.int;
      use type In6_Addr;
      use type OS_Lib.sa_family_t;
      use type OS_Lib.socklen_t;

      Name : aliased Sockaddr_In6;
      Len  : aliased OS_Lib.socklen_t := Name'Size / 8;

   begin
      if C_Getsockname (Socket.S.FD, Name'Address, Len'Access) = Failure then
         Raise_Socket_Error (OS_Lib.Socket_Errno, Socket);
      end if;

      if Name.Family = OS_Lib.AF_INET6 then
         return Name.Addr = (Name.Addr'Range => 0);
      else
         --  !!! Hack, IPv4 address in sockaddr structure is exactly on
         --  FlowInfo place in IPv6 sockaddr.

         return Name.FlowInfo = 0;
      end if;
   end Is_Any_Address;

   -------------
   -- Is_IPv6 --
   -------------

   overriding function Is_IPv6 (Socket : Socket_Type) return Boolean is
      use type C.int;
      use type OS_Lib.sa_family_t;
      use type OS_Lib.socklen_t;

      Name : aliased Sockaddr_In6;
      Len  : aliased OS_Lib.socklen_t := Name'Size / 8;

   begin
      if C_Getsockname (Socket.S.FD, Name'Address, Len'Access) = Failure then
         Raise_Socket_Error (OS_Lib.Socket_Errno, Socket);
      end if;

      return Name.Family = OS_Lib.AF_INET6;
   end Is_IPv6;

   --------------------
   -- Is_Peer_Closed --
   --------------------

   overriding function Is_Peer_Closed
     (Socket : Socket_Type; E : Exception_Occurrence) return Boolean is
   begin
      return Is_Peer_Closed (Net.Socket_Type (Socket), E);
   end Is_Peer_Closed;

   ----------------
   -- Is_Timeout --
   ----------------

   overriding function Is_Timeout
     (Socket : Socket_Type; E : Exception_Occurrence) return Boolean is
   begin
      return Is_Timeout (Net.Socket_Type (Socket), E)
        or else Get_Socket_Errno (E) = OS_Lib.ETIMEDOUT;
   end Is_Timeout;

   ------------
   -- Listen --
   ------------

   overriding procedure Listen
     (Socket : Socket_Type; Queue_Size : Positive := 5)
   is
      use type C.int;

      function C_Listen (S : C.int; Backlog : C.int) return C.int
        with Import, Convention => Stdcall, External_Name => "listen";

   begin
      if C_Listen (Socket.S.FD, C.int (Queue_Size)) = Failure then
         Raise_Socket_Error (OS_Lib.Socket_Errno, Socket);
      end if;

      Socket.C.Listening := True;
   end Listen;

   ---------------
   -- Peer_Addr --
   ---------------

   overriding function Peer_Addr (Socket : Socket_Type) return String is
      use type C.int;
      use type OS_Lib.socklen_t;

      Sin6 : aliased Sockaddr_In6;
      Len  : aliased OS_Lib.socklen_t := Sin6'Size / 8;

   begin
      if C_Getpeername (Socket.S.FD, Sin6'Address, Len'Access) = Failure then
         Raise_Socket_Error (OS_Lib.Socket_Errno, Socket);
      end if;

      return Image (Sin6, Len);

   end Peer_Addr;

   ---------------
   -- Peer_Port --
   ---------------

   overriding function Peer_Port (Socket : Socket_Type) return Positive is
      use type C.int;
      use type OS_Lib.socklen_t;

      Name : aliased Sockaddr_In6;
      Len  : aliased OS_Lib.socklen_t := Name'Size / 8;

   begin
      if C_Getpeername (Socket.S.FD, Name'Address, Len'Access) = Failure then
         Raise_Socket_Error (OS_Lib.Socket_Errno, Socket);
      end if;

      return Positive (Swap_Little_Endian (Unsigned_16 (Name.Port)));
   end Peer_Port;

   -------------
   -- Pending --
   -------------

   overriding function Pending
     (Socket : Socket_Type) return Stream_Element_Count is
   begin
      return Socket.IO_Control (OS_Lib.FIONREAD);
   end Pending;

   ------------------------
   -- Raise_Socket_Error --
   ------------------------

   procedure Raise_Socket_Error (Error : Integer; Socket : Socket_Type) is
   begin
      Raise_Socket_Error (Socket, Error_Message (Error));
   end Raise_Socket_Error;

   procedure Raise_Socket_Error (Error : Integer) is
   begin
      Raise_Socket_Error (Error_Message (Error));
   end Raise_Socket_Error;

   procedure Raise_Socket_Error (Errmsg : String) is
      Socket : constant Socket_Type :=
                 Socket_Type'(Net.Socket_Type with S => null);
      --  Directly usage of the Socket_Type'(Net.Socket_Type with S => null) in
      --  Raise_Socket_Error call cause GNAT GPL 2011 following warning output
      --
      --  warning: implied return after this statement will raise Program_Error
      --  warning: procedure <empty> is marked as No_Return
   begin
      Raise_Socket_Error (Socket, Errmsg);
   end Raise_Socket_Error;

   -------------
   -- Receive --
   -------------

   overriding procedure Receive
     (Socket : Socket_Type;
      Data   : out Stream_Element_Array;
      Last   : out Stream_Element_Offset)
   is
      use type C.int;

      Res : C.int;

      function C_Recv
        (S     : C.int;
         Msg   : System.Address;
         Len   : C.int;
         Flags : C.int) return C.int
        with Import, Convention => Stdcall, External_Name => "recv";

   begin
      if Lock_Set (Socket.S.RL'Access, 1) /= 0 then
         raise Program_Error with "Simultaneous socket receive";
      end if;

      begin
         Wait_For (Input, Socket);
      exception when others =>
         Socket.S.RL := 0;
         raise;
      end;

      Res := C_Recv (Socket.S.FD, Data (Data'First)'Address, Data'Length, 0);

      Socket.S.RL := 0;

      if Res = Failure then
         Raise_Socket_Error (OS_Lib.Socket_Errno, Socket);

      elsif Res = 0 then
         --  socket closed by peer
         raise Socket_Error with Peer_Closed_Message;
      end if;

      Last := Data'First + Ada.Streams.Stream_Element_Offset (Res - 1);

      if Net.Log.Is_Write_Active then
         Net.Log.Write
           (Direction => Net.Log.Received,
            Socket    => Socket,
            Data      => Data,
            Last      => Last);
      end if;
   end Receive;

   ----------------------------
   -- Select_IPv6_If_Present --
   ----------------------------

   function Select_IPv6_If_Present
     (Chain : not null OS_Lib.Addr_Info_Access)
      return not null OS_Lib.Addr_Info_Access
   is
      use type OS_Lib.Addr_Info_Access, C.int;
      Result : OS_Lib.Addr_Info_Access := Chain;
   begin
      loop
         if Result.ai_family = OS_Lib.AF_INET6 then
            return Result;
         end if;

         Result := Result.ai_next;

         exit when Result = null;
      end loop;

      return Chain;
   end Select_IPv6_If_Present;

   ----------
   -- Send --
   ----------

   overriding procedure Send
     (Socket : Socket_Type;
      Data   : Stream_Element_Array;
      Last   : out Stream_Element_Offset)
   is
      use type C.int;

      Errno : Integer;
      RC    : C.int;

      function C_Send
        (S     : C.int;
         Msg   : System.Address;
         Len   : C.int;
         Flags : C.int) return C.int
        with Import, Convention => Stdcall, External_Name => "send";

   begin
      pragma Warnings (Off, "*condition is always *");

      RC := C_Send
              (Socket.S.FD,
               Data'Address,
               Data'Length,
               (if OS_Lib.MSG_NOSIGNAL = -1 then 0 else OS_Lib.MSG_NOSIGNAL));

      pragma Warnings (On, "*condition is always *");

      if RC = Failure then
         Errno := OS_Lib.Socket_Errno;

         if Errno = OS_Lib.EWOULDBLOCK or else Errno = OS_Lib.EAGAIN then
            Last := Last_Index (Data'First, 0);
            return;

         else
            Raise_Socket_Error (Errno, Socket);
         end if;
      end if;

      Last := Last_Index (Data'First, Natural (RC));

      if Net.Log.Is_Write_Active then
         Net.Log.Write
           (Direction => Net.Log.Sent,
            Socket    => Socket,
            Data      => Data,
            Last      => Last);
      end if;
   end Send;

   ----------------------
   -- Set_Int_Sock_Opt --
   ----------------------

   procedure Set_Int_Sock_Opt
     (Socket : Socket_Type;
      Name   : C.int;
      Value  : Integer;
      Level  : C.int := OS_Lib.SOL_SOCKET)
   is
      use type C.int;

      Res : constant C.int :=
              OS_Lib.Set_Sock_Opt
                (Socket.S.FD,
                 Level,
                 Name,
                 Value'Address,
                 Value'Size / System.Storage_Unit);

   begin
      if Res = Failure then
         Raise_Socket_Error (OS_Lib.Socket_Errno, Socket);
      end if;
   end Set_Int_Sock_Opt;

   ------------------
   -- Set_No_Delay --
   ------------------

   overriding procedure Set_No_Delay
     (Socket : Socket_Type; Value : Boolean := True) is
   begin
      Set_Int_Sock_Opt
        (Socket,
         Name  => OS_Lib.TCP_NODELAY,
         Level => OS_Lib.IPPROTO_TCP,
         Value => Boolean'Pos (Value));
   end Set_No_Delay;

   ---------------------------
   -- Set_Non_Blocking_Mode --
   ---------------------------

   procedure Set_Non_Blocking_Mode (Socket : Socket_Type) is
      use type C.int;
      Enabled : aliased C.int := 1;
   begin
      if OS_Lib.C_Ioctl (Socket.S.FD, OS_Lib.FIONBIO, Enabled'Access) /= 0 then
         Raise_Socket_Error (OS_Lib.Socket_Errno, Socket);
      end if;
   end Set_Non_Blocking_Mode;

   -----------------------------
   -- Set_Receive_Buffer_Size --
   -----------------------------

   overriding procedure Set_Receive_Buffer_Size
     (Socket : Socket_Type; Size : Natural) is
   begin
      Set_Int_Sock_Opt (Socket, OS_Lib.SO_RCVBUF, Size);
   end Set_Receive_Buffer_Size;

   --------------------------
   -- Set_Send_Buffer_Size --
   --------------------------

   overriding procedure Set_Send_Buffer_Size
     (Socket : Socket_Type; Size : Natural) is
   begin
      Set_Int_Sock_Opt (Socket, OS_Lib.SO_SNDBUF, Size);
   end Set_Send_Buffer_Size;

   --------------
   -- Shutdown --
   --------------

   overriding procedure Shutdown
     (Socket : Socket_Type; How : Shutmode_Type := Shut_Read_Write)
   is
      use type C.int;
      FD    : C.int;
      EN    : Integer;
      To_OS : constant array (Shutmode_Type) of C.int :=
                (Shut_Read_Write => OS_Lib.SHUT_RDWR,
                 Shut_Read       => OS_Lib.SHUT_RD,
                 Shut_Write      => OS_Lib.SHUT_WR);

      function C_Shutdown (S : C.int; How : C.int) return C.int
        with Import, Convention => Stdcall, External_Name => "shutdown";

   begin
      if Socket.S = null then
         return;
      end if;

      FD := Socket.S.FD;

      if FD = No_Socket then
         return;
      end if;

      if Net.Log.Is_Event_Active then
         Net.Log.Event (Net.Log.Shutdown, Socket);
      end if;

      if C_Shutdown (FD, To_OS (How)) = Failure then
         EN := OS_Lib.Socket_Errno;

         if EN /= OS_Lib.ENOTCONN then
            Log.Error (Socket, Error_Message (EN));
         end if;
      end if;

      if How /= Shut_Read_Write then
         return;
      end if;

      --  Avoid any activity under closed socket in other threads.
      --  Reduce risk to send/receive data on other new created sockets.

      Socket.S.FD := No_Socket;

      if OS_Lib.C_Close (FD) = Failure then
         --  Use copy of the socket with the original discriptor because
         --  original socket is without descriptor now.

         Log.Error
           (Socket_Type'
              (Net.Socket_Type with new Socket_Hidden'(FD => FD, RL => 0)),
            Error_Message (OS_Lib.Socket_Errno));
      end if;
   end Shutdown;

   ------------------------
   -- Swap_Little_Endian --
   ------------------------

   function Swap_Little_Endian (S : Unsigned_16) return Unsigned_16 is
      use System;
      Big_Endian : constant Boolean := Default_Bit_Order = High_Order_First;
   begin
      if Big_Endian then
         return S;
      else
         return Rotate_Left (S, 8);
      end if;
   end Swap_Little_Endian;

   WSA_Data_Dummy : array (1 .. 512) of C.int;

begin
   OS_Lib.WSA_Startup (16#0202#, WSA_Data_Dummy'Address);
end AWS.Net.Std;
