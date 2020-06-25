------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2012-2016, AdaCore                     --
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

with Ada.Unchecked_Deallocation;

with AWS.Default;
with AWS.Headers;
with AWS.Messages;
with AWS.Net.WebSocket.Protocol.Draft76;
with AWS.Net.WebSocket.Protocol.RFC6455;
with AWS.Response;
with AWS.Status.Set;
with AWS.Translator;
with AWS.URL;

package body AWS.Net.WebSocket is

   WS_UID : Utils.Counter (0);
   --  Unique Id for the WebSockets

   type Protocol_State is record
      State : Net.WebSocket.Protocol.State_Class;
   end record;

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
      (AWS.Client.HTTP_Connection, AWS.Client.HTTP_Connection_Access);
   procedure Unchecked_Free is new Unchecked_Deallocation
     (Net.WebSocket.Protocol.State'Class,
      Net.WebSocket.Protocol.State_Class);

   procedure Initialize
      (Self     : in out Object'Class;
       Socket   : Socket_Access;
       Protocol : Net.WebSocket.Protocol.State_Class;
       Headers  : AWS.Headers.List);
   --  Initialize the fields of Self

   -----------
   -- Close --
   -----------

   procedure Close
     (Socket  : in out Object;
      Message : String;
      Error   : Error_Type := Normal_Closure) is
   begin
      --  When the user explicitly closes a web socket, we do not call
      --  On_Close (this is only called when the other end closes the socket)

      Socket.P_State.State.Close (Socket, Message, Error_Code (Error));
   exception
      when AWS.Net.Socket_Error =>
         null;
   end Close;

   -------------
   -- Connect --
   -------------

   procedure Connect
     (Socket : in out Object'Class;
      URI    : String)
   is
      Headers  : AWS.Headers.List := AWS.Headers.Empty_List;
      Resp     : AWS.Response.Data;
      Protocol : AWS.Net.WebSocket.Protocol.State_Class;
      URL      : constant AWS.URL.Object := AWS.URL.Parse (URI);
   begin
      --  Initially, the connection is initiated with standard http GET.

      Socket.Connection := new AWS.Client.HTTP_Connection;
      Protocol := new Net.WebSocket.Protocol.RFC6455.State;

      AWS.Client.Create
        (Socket.Connection.all,
         Host        => URI,
         User        => AWS.Client.No_Data,
         Pwd         => AWS.Client.No_Data,
         Proxy       => AWS.Client.No_Data,
         Proxy_User  => AWS.Client.No_Data,
         Proxy_Pwd   => AWS.Client.No_Data,
         Persistent  => False,
         Certificate => AWS.Default.Client_Certificate,
         Timeouts    => AWS.Client.No_Timeout);

      Protocol.Add_Connect_Headers (AWS.URL.Host (URL), Headers);

      AWS.Client.Get
        (Socket.Connection.all,
         Result      => Resp,
         URI         => AWS.Client.No_Data,
         Data_Range  => AWS.Client.No_Range,
         Headers     => Headers);

      if not Protocol.Check_Connect_Response (Headers, Resp) then
         Unchecked_Free (Protocol);
         Unchecked_Free (Socket.Connection);
         raise AWS.Client.Protocol_Error with "Invalid accept from server";
      end if;

      Initialize
         (Socket,
          AWS.Client.Get_Socket (Socket.Connection.all),
          Protocol,
          Headers);
      AWS.Status.Set.Request (Socket.Request, "GET", URI, "1.1");

      Socket.On_Open ("WebSocket connected with " & URI);
   end Connect;

   ------------
   -- Create --
   ------------

   function Create
     (Socket  : Socket_Access;
      Request : AWS.Status.Data) return Object'Class
   is
      Result   : Object;
      Protocol : Net.WebSocket.Protocol.State_Class;
      Headers  : constant AWS.Headers.List :=
                   AWS.Status.Header (Request);
   begin
      if Headers.Exist (Messages.Sec_WebSocket_Key1_Token)
        and then Headers.Exist (Messages.Sec_WebSocket_Key2_Token)
      then
         Protocol := new Net.WebSocket.Protocol.Draft76.State;
      else
         Protocol := new Net.WebSocket.Protocol.RFC6455.State;
      end if;

      Initialize (Result, Socket, Protocol, Headers);
      Result.Request := Request;
      return Result;
   end Create;

   --------------------
   -- End_Of_Message --
   --------------------

   function End_Of_Message (Socket : Object) return Boolean is
   begin
      return Socket.P_State.State.End_Of_Message;
   end End_Of_Message;

   -----------
   -- Errno --
   -----------

   overriding function Errno (Socket : Object) return Integer is
      use type Interfaces.Unsigned_16;
      E : constant Interfaces.Unsigned_16 := Socket.State.Errno;
   begin
      if E = Interfaces.Unsigned_16'Last then
         return Socket.Socket.Errno;
      else
         Socket.State.Errno := Interfaces.Unsigned_16'Last;
         return Integer (E);
      end if;
   end Errno;

   -----------
   -- Error --
   -----------

   function Error (Socket : Object) return Error_Type is
   begin
      case Socket.State.Errno is
         when 1000   => return Normal_Closure;
         when 1001   => return Going_Away;
         when 1002   => return Protocol_Error;
         when 1003   => return Unsupported_Data;
         when 1005   => return No_Status_Received;
         when 1006   => return Abnormal_Closure;
         when 1007   => return Invalid_Frame_Payload_Data;
         when 1008   => return Policy_Violation;
         when 1009   => return Message_Too_Big;
         when 1010   => return Mandatory_Extension;
         when 1011   => return Internal_Server_Error;
         when 1015   => return TLS_Handshake;
         when others => return Cannot_Resolve_Error;
      end case;
   end Error;

   ----------
   -- Free --
   ----------

   overriding procedure Free (Socket : in out Object) is
      use type AWS.Client.HTTP_Connection_Access;
      procedure Unchecked_Free is
         new Unchecked_Deallocation (Internal_State, Internal_State_Access);
      procedure Unchecked_Free is
         new Unchecked_Deallocation (Protocol_State, Protocol_State_Access);
   begin
      Unchecked_Free (Socket.State);

      if Socket.P_State /= null then
         Unchecked_Free (Socket.P_State.State);
         Unchecked_Free (Socket.P_State);
      end if;

      if Socket.Connection /= null then
         --  Also closes Socket.Socket, since it is shared
         Unchecked_Free (Socket.Connection);
      else
         Free (Socket.Socket);
      end if;

      Free (Socket.Mem_Sock);
   end Free;

   --------------
   -- Get_Addr --
   --------------

   overriding function Get_Addr (Socket : Object) return String is
   begin
      return Socket.Socket.Get_Addr;
   end Get_Addr;

   ------------
   -- Get_FD --
   ------------

   overriding function Get_FD (Socket : Object) return FD_Type is
   begin
      return Socket.Socket.Get_FD;
   end Get_FD;

   --------------
   -- Get_Port --
   --------------

   overriding function Get_Port (Socket : Object) return Positive is
   begin
      return Socket.Socket.Get_Port;
   end Get_Port;

   -----------------------------
   -- Get_Receive_Buffer_Size --
   -----------------------------

   overriding function Get_Receive_Buffer_Size
     (Socket : Object) return Natural is
   begin
      return Socket.Socket.Get_Receive_Buffer_Size;
   end Get_Receive_Buffer_Size;

   --------------------------
   -- Get_Send_Buffer_Size --
   --------------------------

   overriding function Get_Send_Buffer_Size (Socket : Object) return Natural is
   begin
      return Socket.Socket.Get_Send_Buffer_Size;
   end Get_Send_Buffer_Size;

   -------------
   -- Get_UID --
   -------------

   function Get_UID (Socket : Object) return UID is
   begin
      return Socket.Id;
   end Get_UID;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
      (Self     : in out Object'Class;
       Socket   : Socket_Access;
       Protocol : Net.WebSocket.Protocol.State_Class;
       Headers  : AWS.Headers.List)
   is
      Version      : Natural := 0;
      WS_UID_Value : Natural := 0;
   begin
      if Headers.Exist (Messages.Sec_WebSocket_Version_Token) then
         declare
            Value : constant String :=
                      Headers.Get (Messages.Sec_WebSocket_Version_Token);
         begin
            if Utils.Is_Number (Value) then
               Version := Natural'Value (Value);
            end if;
         end;
      end if;

      WS_UID.Increment (Value => WS_UID_Value);

      Self.Socket   := Socket;
      Self.Id       := UID (WS_UID_Value);
      Self.Version  := Version;
      Self.State    := new Internal_State'
         (Kind          => Unknown,
          Errno         => Interfaces.Unsigned_16'Last,
          Last_Activity => Calendar.Clock);
      Self.P_State  := new Protocol_State'(State => Protocol);
      Self.Mem_Sock := null;
      Self.In_Mem   := False;
   end Initialize;

   ------------------
   -- Is_Listening --
   ------------------

   overriding function Is_Listening (Socket : Object) return Boolean is
      pragma Unreferenced (Socket);
   begin
      return False;
   end Is_Listening;

   ----------
   -- Kind --
   ----------

   function Kind (Socket : Object) return Kind_Type is
   begin
      return Socket.State.Kind;
   end Kind;

   ----------------
   -- On_Message --
   ----------------

   procedure On_Message (Socket : in out Object; Message : Unbounded_String) is
   begin
      On_Message (Object'Class (Socket), To_String (Message));
   end On_Message;

   ------------
   -- Origin --
   ------------

   function Origin (Socket : Object) return String is
   begin
      return Headers.Get
        (AWS.Status.Header (Socket.Request), Messages.Origin_Token);
   end Origin;

   ---------------
   -- Peer_Addr --
   ---------------

   overriding function Peer_Addr (Socket : Object) return String is
   begin
      return Socket.Socket.Peer_Addr;
   end Peer_Addr;

   ---------------
   -- Peer_Port --
   ---------------

   overriding function Peer_Port (Socket : Object) return Positive is
   begin
      return Socket.Socket.Peer_Port;
   end Peer_Port;

   -------------
   -- Pending --
   -------------

   overriding function Pending
     (Socket : Object) return Stream_Element_Count is
   begin
      return Socket.Socket.Pending;
   end Pending;

   ----------
   -- Poll --
   ----------

   function Poll
     (Socket  : in out Object'Class;
      Timeout : Duration)
     return Boolean
   is
      procedure Do_Receive
         (Socket : not null access Object'Class;
          Data   : out Ada.Streams.Stream_Element_Array;
          Last   : out Ada.Streams.Stream_Element_Offset);
      --  Fetch available data on the socket

      ----------------
      -- Do_Receive --
      ----------------

      procedure Do_Receive
         (Socket : not null access Object'Class;
          Data   : out Ada.Streams.Stream_Element_Array;
          Last   : out Ada.Streams.Stream_Element_Offset) is
      begin
         Socket.Receive (Data, Last);
      end Do_Receive;

      function Read_Message is new AWS.Net.WebSocket.Read_Message
         (Receive => Do_Receive);

      Obj   : Object_Class := Socket'Unrestricted_Access;
      Event : AWS.Net.Event_Set;
      Msg   : Ada.Strings.Unbounded.Unbounded_String;
   begin
      Event := Socket.Poll
         ((AWS.Net.Input => True, others => False), Timeout => Timeout);

      if Event (AWS.Net.Input) then
         --  Block until we have received all chunks of the frame
         while not Read_Message (Obj, Msg) loop
            null;
         end loop;
         return True;

      elsif Event (AWS.Net.Error) then
         Socket.On_Error ("Socket error");
      end if;

      return False;

   exception
      when AWS.Net.Socket_Error =>
         --  Socket has been closed
         return False;
   end Poll;

   ----------------------
   -- Protocol_Version --
   ----------------------

   function Protocol_Version (Socket : Object) return Natural is
   begin
      return Socket.Version;
   end Protocol_Version;

   ------------------
   -- Read_Message --
   ------------------

   function Read_Message
      (WebSocket : in out Object_Class;
       Message   : in out Ada.Strings.Unbounded.Unbounded_String)
      return Boolean
   is
      Data : Stream_Element_Array (1 .. 4_096);
      Last : Stream_Element_Offset;
   begin
      begin
         WebSocket.Receive (Data, Last);
      exception
         when E : Socket_Error =>
            --  FD = No_Socket means Websocket is already closed
            --  and unregistered in other task

            if WebSocket.Get_FD /= Net.No_Socket then
               WebSocket_Exception
                 (WebSocket,
                  Exception_Message (E),
                  Abnormal_Closure);
               On_Error (WebSocket);
            end if;

            return True;
      end;

      case WebSocket.Kind is
         when Text | Binary =>
            Append
              (Message,
               Translator.To_String (Data (Data'First .. Last)));

            if WebSocket.End_Of_Message then
               --  Validate the message as being valid UTF-8 string

               if WebSocket.Kind = Text
                 and then not Utils.Is_Valid_UTF8 (Message)
               then
                  On_Error (WebSocket);
                  WebSocket.On_Close (To_String (Message));
                  WebSocket.Shutdown;
                  On_Free (WebSocket);
               else
                  WebSocket.On_Message (Message);
                  On_Success (WebSocket);
               end if;

               return True;
            end if;

         when Connection_Close =>
            On_Error (WebSocket);
            WebSocket.On_Close (To_String (Message));
            WebSocket.Shutdown;
            On_Free (WebSocket);
            return True;

         when Ping | Pong =>
            if WebSocket.End_Of_Message then
               On_Success (WebSocket);
               return True;
            end if;

         when Connection_Open =>
            --  Note that the On_Open message has been handled at the
            --  time the WebSocket was registered.
            return True;

         when Unknown =>
            On_Error (WebSocket);
            WebSocket.On_Error ("Unknown frame type");
            WebSocket.On_Close ("Unknown frame type");
            WebSocket.Shutdown;
            On_Free (WebSocket);
            return True;
      end case;

      return False;
   end Read_Message;

   -------------
   -- Receive --
   -------------

   overriding procedure Receive
     (Socket : Object;
      Data   : out Stream_Element_Array;
      Last   : out Stream_Element_Offset) is
   begin
      Socket.P_State.State.Receive (Socket, Data, Last);
      Socket.State.Last_Activity := Calendar.Clock;
   end Receive;

   -------------
   -- Request --
   -------------

   function Request (Socket : Object) return AWS.Status.Data is
   begin
      return Socket.Request;
   end Request;

   ----------
   -- Send --
   ----------

   overriding procedure Send
     (Socket : Object;
      Data   : Stream_Element_Array;
      Last   : out Stream_Element_Offset) is
   begin
      if Socket.In_Mem then
         Socket.Mem_Sock.Send (Data, Last);
      else
         Socket.Socket.Send (Data, Last);
      end if;

      Socket.State.Last_Activity := Calendar.Clock;
   end Send;

   procedure Send
     (Socket    : in out Object;
      Message   : String;
      Is_Binary : Boolean := False) is
   begin
      if Is_Binary then
         Socket.State.Kind := Binary;
      else
         Socket.State.Kind := Text;
      end if;

      Socket.P_State.State.Send
        (Socket, Translator.To_Stream_Element_Array (Message));
   end Send;

   procedure Send
     (Socket    : in out Object;
      Message   : Unbounded_String;
      Is_Binary : Boolean := False) is
   begin
      if Is_Binary then
         Socket.State.Kind := Binary;
      else
         Socket.State.Kind := Text;
      end if;

      Socket.P_State.State.Send (Socket, Message);
   end Send;

   procedure Send
     (Socket    : in out Object;
      Message   : Stream_Element_Array;
      Is_Binary : Boolean := True) is
   begin
      if Is_Binary then
         Socket.State.Kind := Binary;
      else
         Socket.State.Kind := Text;
      end if;

      Socket.P_State.State.Send (Socket, Message);
   end Send;

   --------------
   -- Shutdown --
   --------------

   overriding procedure Shutdown
     (Socket : Object;
      How    : Shutmode_Type := Shut_Read_Write) is
   begin
      if Socket.Socket /= null then
         Socket.Socket.Shutdown (How);
      end if;
   end Shutdown;

   ---------
   -- URI --
   ---------

   function URI (Socket : Object) return String is
   begin
      return AWS.Status.URI (Socket.Request);
   end URI;

   -------------------------
   -- WebSocket_Exception --
   -------------------------

   procedure WebSocket_Exception
     (WebSocket : not null access Object'Class;
      Message   : String;
      Error     : Error_Type) is
   begin
      WebSocket.State.Errno := Error_Code (Error);
      WebSocket.On_Error (Message);

      if Error /= Abnormal_Closure then
         WebSocket.On_Close (Message);
      end if;

      WebSocket.Shutdown;
   exception
      when others =>
         --  Never propagate an exception at this point
         null;
   end WebSocket_Exception;

end AWS.Net.WebSocket;
