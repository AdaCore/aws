------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2018, AdaCore                     --
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

with Ada.Strings.Maps;
with Ada.Unchecked_Deallocation;

with GNAT.Sockets;

with AWS.Net.Log;
with AWS.OS_Lib;

package body AWS.Net.Std is

   use GNAT;

   type Socket_Hidden is record
      FD : Sockets.Socket_Type := Sockets.No_Socket;
   end record;

   procedure Free is
      new Ada.Unchecked_Deallocation (Socket_Hidden, Socket_Hidden_Access);

   procedure Raise_Exception
     (E       : Exceptions.Exception_Occurrence;
      Routine : String;
      Socket  : Socket_Type)
     with No_Return;
   --  Raise and log exception Socket_Error with E's message and a reference to
   --  the routine name.

   procedure Set_Non_Blocking_Mode (Socket : Socket_Type);
   --  Set the socket to the non-blocking mode.
   --  AWS is not using blocking sockets internally.

   To_GNAT : constant array (Family_Type) of Sockets.Family_Type :=
               (Family_Inet   => Sockets.Family_Inet,
                Family_Inet6  => Sockets.Family_Inet6,
                Family_Unspec => Sockets.Family_Unspec);

   -------------------
   -- Accept_Socket --
   -------------------

   overriding procedure Accept_Socket
     (Socket     : Net.Socket_Type'Class;
      New_Socket : in out Socket_Type)
   is
      Sock_Addr : Sockets.Sock_Addr_Type;
   begin
      if New_Socket.S /= null then
         New_Socket := Socket_Type'(Net.Socket_Type with others => <>);
      end if;

      New_Socket.S := new Socket_Hidden;

      --  Check for Accept_Socket timeout

      Wait_For (Input, Socket);

      Sockets.Accept_Socket
        (Socket_Type (Socket).S.FD, New_Socket.S.FD, Sock_Addr);

      if Net.Log.Is_Event_Active then
         Net.Log.Event (Net.Log.Accept_Socket, New_Socket);
      end if;

      Set_Non_Blocking_Mode (New_Socket);
   exception
      when E : Sockets.Socket_Error =>
         Raise_Exception (E, "Accept_Socket", New_Socket);
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
      Family        : Family_Type := Family_Unspec) is
   begin
      if Socket.S /= null then
         Socket := Socket_Type'(Net.Socket_Type with others => <>);
      end if;

      Socket.S := new Socket_Hidden;

      declare
         use GNAT.Sockets;
         Keep_Excp : Exceptions.Exception_Occurrence;
         Addresses : Address_Info_Array :=
                       Get_Address_Info
                         (Host, Utils.Image (Port), To_GNAT (Family),
                          Passive => True);
      begin
         if Family = Family_Unspec then
            Sort (Addresses, IPv6_TCP_Preferred'Access);
         end if;

         for Addr_Info of Addresses loop
            Check_Address : begin
               Create_Socket
                 (Socket.S.FD, Addr_Info.Addr.Family, Addr_Info.Mode,
                  Addr_Info.Level);

               if Addr_Info.Addr.Family = Sockets.Family_Inet6 then
                  Set_Socket_Option
                    (Socket.S.FD, IP_Protocol_For_IPv6_Level,
                     (Name => Sockets.IPv6_Only, Enabled => IPv6_Only));
               end if;

               Set_Non_Blocking_Mode (Socket);

               Set_Socket_Option
                 (Socket.S.FD, Socket_Level,
                  Option => (Sockets.Reuse_Address, Enabled => Reuse_Address));

               Bind_Socket (Socket.S.FD, Addr_Info.Addr);

               return;

            exception
               when E : Sockets.Socket_Error | Sockets.Host_Error =>
                  if Socket.S.FD /= Sockets.No_Socket then
                     Sockets.Close_Socket (Socket.S.FD);
                     Socket.S.FD := Sockets.No_Socket;
                  end if;

                  Exceptions.Save_Occurrence (Keep_Excp, E);
            end Check_Address;
         end loop;

         Raise_Exception (Keep_Excp, "Bind", Socket);
      end;

   exception
      when E : Sockets.Socket_Error | Sockets.Host_Error =>
         Raise_Exception (E, "Get_Address_Info", Socket);
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
      Sock_Addr : Sockets.Sock_Addr_Type := Sockets.No_Sock_Addr;

      Close_On_Exception : Boolean := False;

      procedure Raise_Error (Errno : Integer) with Inline;

      procedure Raise_Error (Errm : String);

      -----------------
      -- Raise_Error --
      -----------------

      procedure Raise_Error (Errno : Integer) is
      begin
         Raise_Error (Error_Message (Errno));
      end Raise_Error;

      procedure Raise_Error (Errm : String) is
         use type Sockets.Inet_Addr_Type;

         Addr : constant String := Sockets.Image (Sock_Addr);
         Msg  : constant String :=
                 (if Ada.Strings.Fixed.Index (Errm, Host) > 0 then Errm else
                  Error_On_Connect (Errm)
                  & (if Sock_Addr.Addr = Sockets.No_Inet_Addr
                     then Host & ':' & Utils.Image (Port)
                     elsif Utils.Match (Addr, Host) then Addr
                     else Host & ' ' & Addr));
      begin
         Log.Error (Socket, Msg);

         if Close_On_Exception then
            Sockets.Close_Socket (Socket.S.FD);
         end if;

         raise Socket_Error with Msg;
      end Raise_Error;

   begin
      if Socket.S /= null then
         Socket := Socket_Type'(Net.Socket_Type with others => <>);
      end if;

      Socket.S := new Socket_Hidden;

      declare
         use GNAT.Sockets;
         Keep_Excp : Exceptions.Exception_Occurrence;
         Addresses : constant Address_Info_Array :=
                       Get_Address_Info
                         (Host, "", To_GNAT (Family), Passive => False);
      begin
         for Addr_Info of Addresses loop
            Check_Address : begin
               Sock_Addr := Addr_Info.Addr;
               Sock_Addr.Port := Sockets.Port_Type (Port);

               Sockets.Create_Socket
                 (Socket.S.FD, Addr_Info.Addr.Family, Addr_Info.Mode,
                  Addr_Info.Level);
               Close_On_Exception := True;

               Set_Non_Blocking_Mode (Socket);

               begin
                  Connect_Socket (Socket.S.FD, Sock_Addr);
               exception
                  when E : GNAT.Sockets.Socket_Error =>
                     --  Ignore EWOULDBLOCK and EINPROGRESS errors, because we
                     --  are using none blocking connect.
                     --  ??? Note, we should change this when GNAT will support
                     --  Non-blocking connect.

                     case Resolve_Exception (E) is
                        when Operation_Now_In_Progress
                           | Resource_Temporarily_Unavailable =>
                           null;
                        when others =>
                           raise;
                     end case;
               end;

               if Wait then
                  declare
                     Events : constant Event_Set :=
                                Net.Wait
                                  (Socket, (Output => True, Input => False));
                  begin
                     if Events (Error) then
                        --  Raise original exception to keep trying in next
                        --  address iteration.

                        raise GNAT.Sockets.Socket_Error with
                           Error_Message (Errno (Socket));

                     elsif not Events (Output) then
                        Raise_Error (OS_Lib.ETIMEDOUT);
                     end if;
                  end;
               end if;

               if Net.Log.Is_Event_Active then
                  Net.Log.Event (Net.Log.Connect, Socket);
               end if;

               return;

            exception
               when E : Sockets.Socket_Error | Sockets.Host_Error =>
                  if Close_On_Exception then
                     Sockets.Close_Socket (Socket.S.FD);
                     Close_On_Exception := False;
                  end if;

                  Save_Occurrence (Keep_Excp, E);
            end Check_Address;
         end loop;

         Raise_Error (Exception_Message (Keep_Excp));
      end;

   exception
      when E : Sockets.Socket_Error | Sockets.Host_Error =>
         Raise_Error (Exception_Message (E));
   end Connect;

   -----------
   -- Errno --
   -----------

   overriding function Errno (Socket : Socket_Type) return Integer is
      use Sockets;

      package SC renames OS_Lib;

      Option : constant Option_Type :=
                 Get_Socket_Option (Socket.S.FD, Socket_Level, Name => Error);
   begin
      case Option.Error is
         when Success                                       =>
            return 0;
         when Permission_Denied                             =>
            return SC.EACCES;
         when Address_Already_In_Use                        =>
            return SC.EADDRINUSE;
         when Cannot_Assign_Requested_Address               =>
            return SC.EADDRNOTAVAIL;
         when Address_Family_Not_Supported_By_Protocol      =>
            return SC.EAFNOSUPPORT;
         when Operation_Already_In_Progress                 =>
            return SC.EALREADY;
         when Bad_File_Descriptor                           =>
            return SC.EBADF;
         when Software_Caused_Connection_Abort              =>
            return SC.ECONNABORTED;
         when Connection_Refused                            =>
            return SC.ECONNREFUSED;
         when Connection_Reset_By_Peer                      =>
            return SC.ECONNRESET;
         when Destination_Address_Required                  =>
            return SC.EDESTADDRREQ;
         when Bad_Address                                   =>
            return SC.EFAULT;
         when Host_Is_Down                                  =>
            return SC.EHOSTDOWN;
         when No_Route_To_Host                              =>
            return SC.EHOSTUNREACH;
         when Operation_Now_In_Progress                     =>
            return SC.EINPROGRESS;
         when Interrupted_System_Call                       =>
            return SC.EINTR;
         when Invalid_Argument                              =>
            return SC.EINVAL;
         when Input_Output_Error                            =>
            return SC.EIO;
         when Transport_Endpoint_Already_Connected          =>
            return SC.EISCONN;
         when Too_Many_Symbolic_Links                       =>
            return SC.ELOOP;
         when Too_Many_Open_Files                           =>
            return SC.EMFILE;
         when Message_Too_Long                              =>
            return SC.EMSGSIZE;
         when File_Name_Too_Long                            =>
            return SC.ENAMETOOLONG;
         when Network_Is_Down                               =>
            return SC.ENETDOWN;
         when Network_Dropped_Connection_Because_Of_Reset   =>
            return SC.ENETRESET;
         when Network_Is_Unreachable                        =>
            return SC.ENETUNREACH;
         when No_Buffer_Space_Available                     =>
            return SC.ENOBUFS;
         when Protocol_Not_Available                        =>
            return SC.ENOPROTOOPT;
         when Transport_Endpoint_Not_Connected              =>
            return SC.ENOTCONN;
         when Socket_Operation_On_Non_Socket                =>
            return SC.ENOTSOCK;
         when Operation_Not_Supported                       =>
            return SC.EOPNOTSUPP;
         when Protocol_Family_Not_Supported                 =>
            return SC.EPFNOSUPPORT;
         when Broken_Pipe                                   =>
            return SC.EPIPE;
         when Protocol_Not_Supported                        =>
            return SC.EPROTONOSUPPORT;
         when Protocol_Wrong_Type_For_Socket                =>
            return SC.EPROTOTYPE;
         when Cannot_Send_After_Transport_Endpoint_Shutdown =>
            return SC.ESHUTDOWN;
         when Socket_Type_Not_Supported                     =>
            return SC.ESOCKTNOSUPPORT;
         when Connection_Timed_Out                          =>
            return SC.ETIMEDOUT;
         when Too_Many_References                           =>
            return SC.ETOOMANYREFS;
         when Resource_Temporarily_Unavailable              =>
            return SC.EWOULDBLOCK;
         when Unknown_Host                                  =>
            return SC.HOST_NOT_FOUND;
         when Host_Name_Lookup_Failure                      =>
            return SC.TRY_AGAIN;
         when Non_Recoverable_Error                         =>
            return SC.NO_RECOVERY;
         when Unknown_Server_Error                          =>
            return SC.NO_DATA;
         when others                                        =>
            return Integer'Last;
      end case;
   end Errno;

   ----------
   -- Free --
   ----------

   overriding procedure Free (Socket : in out Socket_Type) is
   begin
      Free (Socket.S);
   end Free;

   --------------
   -- Get_Addr --
   --------------

   overriding function Get_Addr (Socket : Socket_Type) return String is
      use Sockets;
   begin
      return Image (Get_Socket_Name (Socket.S.FD).Addr);
   exception
      when E : Sockets.Socket_Error =>
         Raise_Exception (E, "Get_Addr", Socket);
   end Get_Addr;

   ------------
   -- Get_FD --
   ------------

   overriding function Get_FD (Socket : Socket_Type) return Integer is
   begin
      if Socket.S = null then
         return No_Socket;
      else
         return Sockets.To_C (Socket.S.FD);
      end if;
   end Get_FD;

   --------------
   -- Get_Port --
   --------------

   overriding function Get_Port (Socket : Socket_Type) return Positive is
   begin
      return Positive (Sockets.Get_Socket_Name (Socket.S.FD).Port);
   exception
      when E : Sockets.Socket_Error =>
         Raise_Exception (E, "Get_Port", Socket);
   end Get_Port;

   -----------------------------
   -- Get_Receive_Buffer_Size --
   -----------------------------

   overriding function Get_Receive_Buffer_Size
     (Socket : Socket_Type) return Natural
   is
      use Sockets;
   begin
      return Get_Socket_Option
               (Socket.S.FD, Socket_Level, Name => Receive_Buffer).Size;
   exception
      when E : Sockets.Socket_Error =>
         Raise_Exception (E, "Get_Receive_Buffer_Size", Socket);
   end Get_Receive_Buffer_Size;

   --------------------------
   -- Get_Send_Buffer_Size --
   --------------------------

   overriding function Get_Send_Buffer_Size
     (Socket : Socket_Type) return Natural
   is
      use Sockets;
   begin
      return Get_Socket_Option
               (Socket.S.FD, Socket_Level, Name => Send_Buffer).Size;
   exception
      when E : Sockets.Socket_Error =>
         Raise_Exception (E, "Get_Send_Buffer_Size", Socket);
   end Get_Send_Buffer_Size;

   ---------------
   -- Host_Name --
   ---------------

   function Host_Name return String is
   begin
      return Sockets.Host_Name;
   end Host_Name;

   --------------------
   -- IPv6_Available --
   --------------------

   function IPv6_Available return Boolean is
   begin
      return True;
   end IPv6_Available;

   --------------------
   -- Is_Any_Address --
   --------------------

   overriding function Is_Any_Address (Socket : Socket_Type) return Boolean is
      use Sockets;
   begin
      return Get_Socket_Name (Socket.S.FD).Addr
               in Any_Inet6_Addr | Any_Inet_Addr;
   exception
      when E : Sockets.Socket_Error =>
         Raise_Exception (E, "Is_Any_Address", Socket);
   end Is_Any_Address;

   -------------
   -- Is_IPv6 --
   -------------

   overriding function Is_IPv6 (Socket : Socket_Type) return Boolean is
      use Sockets;
   begin
      return Get_Socket_Name (Socket.S.FD).Family = Family_Inet6;
   exception
      when E : Sockets.Socket_Error =>
         Raise_Exception (E, "Is_IPv6", Socket);
   end Is_IPv6;

   --------------------
   -- Is_Peer_Closed --
   --------------------

   overriding function Is_Peer_Closed
     (Socket : Socket_Type;
      E      : Exception_Occurrence) return Boolean is
   begin
      return Is_Peer_Closed (Net.Socket_Type (Socket), E)
        or else Get_Socket_Errno (E) = OS_Lib.ECONNRESET;
   end Is_Peer_Closed;

   ----------------
   -- Is_Timeout --
   ----------------

   overriding function Is_Timeout
     (Socket : Socket_Type;
      E      : Exception_Occurrence) return Boolean is
   begin
      return Is_Timeout (Net.Socket_Type (Socket), E)
        or else Get_Socket_Errno (E) = OS_Lib.ETIMEDOUT;
   end Is_Timeout;

   ------------
   -- Listen --
   ------------

   overriding procedure Listen
     (Socket     : Socket_Type;
      Queue_Size : Positive := 5) is
   begin
      Sockets.Listen_Socket (Socket.S.FD, Queue_Size);
      Socket.C.Listening := True;
   exception
      when E : Sockets.Socket_Error =>
         Raise_Exception (E, "Listen", Socket);
   end Listen;

   ---------------
   -- Peer_Addr --
   ---------------

   overriding function Peer_Addr (Socket : Socket_Type) return String is
      use Sockets;
   begin
      return Image (Get_Peer_Name (Socket.S.FD).Addr);
   exception
      when E : Sockets.Socket_Error =>
         Raise_Exception (E, "Peer_Addr", Socket);
   end Peer_Addr;

   ---------------
   -- Peer_Port --
   ---------------

   overriding function Peer_Port (Socket : Socket_Type) return Positive is
   begin
      return Positive (Sockets.Get_Peer_Name (Socket.S.FD).Port);
   exception
      when E : Sockets.Socket_Error =>
         Raise_Exception (E, "Peer_Port", Socket);
   end Peer_Port;

   -------------
   -- Pending --
   -------------

   overriding function Pending
     (Socket : Socket_Type) return Stream_Element_Count
   is
      Res : Sockets.Request_Type (Sockets.N_Bytes_To_Read);
   begin
      Sockets.Control_Socket (Socket.S.FD, Res);

      return Stream_Element_Count (Res.Size);
   exception
      when E : Sockets.Socket_Error =>
         Raise_Exception (E, "Pending", Socket);
   end Pending;

   ---------------------
   -- Raise_Exception --
   ---------------------

   procedure Raise_Exception
     (E       : Exceptions.Exception_Occurrence;
      Routine : String;
      Socket  : Socket_Type)
   is
      Msg : constant String := Routine & " : " & Exception_Message (E);
   begin
      Log.Error (Socket, Message => Msg);
      raise Socket_Error with Msg;
   end Raise_Exception;

   -------------
   -- Receive --
   -------------

   overriding procedure Receive
     (Socket : Socket_Type;
      Data   : out Stream_Element_Array;
      Last   : out Stream_Element_Offset) is
   begin
      Wait_For (Input, Socket);

      Sockets.Receive_Socket (Socket.S.FD, Data, Last);

      --  Check if socket closed by peer

      if Last = Data'First - 1 then
         raise Socket_Error with Peer_Closed_Message;
      end if;

      if Net.Log.Is_Write_Active then
         Net.Log.Write
           (Direction => Net.Log.Received,
            Socket    => Socket,
            Data      => Data,
            Last      => Last);
      end if;
   exception
      when E : Sockets.Socket_Error =>
         Raise_Exception (E, "Receive", Socket);
   end Receive;

   ----------
   -- Send --
   ----------

   overriding procedure Send
     (Socket : Socket_Type;
      Data   : Stream_Element_Array;
      Last   : out Stream_Element_Offset)
   is
      use type Sockets.Error_Type;
   begin
      Sockets.Send_Socket (Socket.S.FD, Data, Last, null);

      if Net.Log.Is_Write_Active then
         Net.Log.Write
           (Direction => Net.Log.Sent,
            Socket    => Socket,
            Data      => Data,
            Last      => Last);
      end if;
   exception
      when E : Sockets.Socket_Error =>
         declare
            Error : constant Sockets.Error_Type :=
                      Sockets.Resolve_Exception (E);
         begin
            if Error = Sockets.Resource_Temporarily_Unavailable then
               Last := Last_Index (Data'First, 0);
            else
               Raise_Exception (E, "Send", Socket);
            end if;
         end;
   end Send;

   ------------------
   -- Set_No_Delay --
   ------------------

   overriding procedure Set_No_Delay
     (Socket : Socket_Type; Value : Boolean := True)
   is
      use Sockets;
   begin
      Set_Socket_Option
        (Socket.S.FD,
         Level  => IP_Protocol_For_TCP_Level,
         Option => (No_Delay, Value));
   exception
      when E : Sockets.Socket_Error =>
         Raise_Exception (E, "Set_No_Delay", Socket);
   end Set_No_Delay;

   ---------------------------
   -- Set_Non_Blocking_Mode --
   ---------------------------

   procedure Set_Non_Blocking_Mode (Socket : Socket_Type) is
      use Sockets;
      Mode : Request_Type (Non_Blocking_IO);
   begin
      Mode.Enabled := True;

      Control_Socket (Socket.S.FD, Mode);
   exception
      when E : Sockets.Socket_Error =>
         Raise_Exception (E, "Set_Non_Blocking_Mode", Socket);
   end Set_Non_Blocking_Mode;

   -----------------------------
   -- Set_Receive_Buffer_Size --
   -----------------------------

   overriding procedure Set_Receive_Buffer_Size
     (Socket : Socket_Type;
      Size   : Natural)
   is
      use Sockets;
   begin
      Set_Socket_Option
        (Socket.S.FD, Socket_Level, Option => (Receive_Buffer, Size));
   exception
      when E : Sockets.Socket_Error =>
         Raise_Exception (E, "Set_Receive_Buffer_Size", Socket);
   end Set_Receive_Buffer_Size;

   --------------------------
   -- Set_Send_Buffer_Size --
   --------------------------

   overriding procedure Set_Send_Buffer_Size
     (Socket : Socket_Type;
      Size   : Natural)
   is
      use Sockets;
   begin
      Set_Socket_Option
        (Socket.S.FD, Socket_Level, Option => (Send_Buffer, Size));
   exception
      when E : Sockets.Socket_Error =>
         Raise_Exception (E, "Set_Send_Buffer_Size", Socket);
   end Set_Send_Buffer_Size;

   --------------
   -- Shutdown --
   --------------

   overriding procedure Shutdown
     (Socket : Socket_Type; How : Shutmode_Type := Shut_Read_Write)
   is
      To_GNAT : constant array (Shutmode_Type) of Sockets.Shutmode_Type :=
                  (Shut_Read_Write => Sockets.Shut_Read_Write,
                   Shut_Read       => Sockets.Shut_Read,
                   Shut_Write      => Sockets.Shut_Write);
   begin
      if Socket.S /= null then
         if Net.Log.Is_Event_Active then
            Net.Log.Event (Net.Log.Shutdown, Socket);
         end if;

         declare
            use type Sockets.Error_Type;
         begin
            --  We catch socket exceptions here as we do not want this call to
            --  fail. A shutdown will fail on non connected sockets.

            Sockets.Shutdown_Socket (Socket.S.FD, To_GNAT (How));
         exception
            when E : Sockets.Socket_Error =>
               if Sockets.Resolve_Exception (E)
                  /= Sockets.Transport_Endpoint_Not_Connected
               then
                  Log.Error
                    (Socket,
                     Message => "Shutdown : "
                                & Error'Img & ' ' & Exception_Message (E));
               end if;
         end;

         if How /= Shut_Read_Write then
            return;
         end if;

         declare
            FD : constant Sockets.Socket_Type := Socket.S.FD;
         begin
            --  Avoid any activity under closed socket in other threads.
            --  Reduce risk to send/receive data on other new created sockets.

            Socket.S.FD := Sockets.No_Socket;

            --  ??? In some cases the call above fails because the socket
            --  descriptor is not valid (errno = EABF). This happen on
            --  GNU/Linux only and the problem is not fully understood at this
            --  point. We catch the exception here to hide this problem.

            Sockets.Close_Socket (FD);
         exception
            when E : Sockets.Socket_Error | Constraint_Error =>
               --  Use copy of the socket with the original discriptor
               --  because original socket is without descriptor now.

               Log.Error
                 (Socket_Type'
                    (Net.Socket_Type with new Socket_Hidden'(FD => FD)),
                  Message => "Close : " & Exception_Message (E));
         end;
      end if;
   end Shutdown;

begin
   --  We need No_Socket Integer value for SSL implementation but we could not
   --  use GNAT.Sockets in AWS.Net spec. So we check here to be sure that we
   --  use right value.

   if No_Socket /= Sockets.To_C (Sockets.No_Socket) then
      raise Program_Error with "No_Socket have to be "
        & Integer'Image (Sockets.To_C (Sockets.No_Socket));
   end if;
end AWS.Net.Std;
