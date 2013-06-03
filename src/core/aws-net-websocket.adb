------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2012-2013, AdaCore                     --
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

with Ada.Streams;
with Ada.Unchecked_Deallocation;
with Interfaces;

with AWS.Headers;
with AWS.Messages;
with AWS.Translator;
with AWS.Net.WebSocket.Protocol.Draft76;
with AWS.Net.WebSocket.Protocol.RFC6455;

package body AWS.Net.WebSocket is

   use Ada.Streams;

   type Protocol_State is record
      State : Net.WebSocket.Protocol.State_Class;
   end record;

   ------------
   -- Create --
   ------------

   function Create
     (Socket  : Socket_Access;
      Request : AWS.Status.Data) return Object'Class
   is

      Headers  : constant AWS.Headers.List := AWS.Status.Header (Request);
      Version  : Natural := 0;
      Protocol : Net.WebSocket.Protocol.State_Class;

   begin
      if Headers.Exist (Messages.Sec_WebSocket_Key1_Token)
        and then Headers.Exist (Messages.Sec_WebSocket_Key2_Token)
      then
         Protocol := new Net.WebSocket.Protocol.Draft76.State;
      else
         Protocol := new Net.WebSocket.Protocol.RFC6455.State;
      end if;

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

      return Object'
        (Net.Socket_Type with
           Socket  => Socket,
           Request => Request,
           Version => Version,
           State   => new Internal_State'
                           (Kind  => Unknown,
                            Errno => Interfaces.Unsigned_16'Last),
           P_State => new Protocol_State'(State => Protocol));
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
      procedure Unchecked_Free is
         new Unchecked_Deallocation (Internal_State, Internal_State_Access);
      procedure Unchecked_Free is
         new Unchecked_Deallocation (Protocol_State, Protocol_State_Access);
      procedure Unchecked_Free is new Unchecked_Deallocation
        (Net.WebSocket.Protocol.State'Class,
         Net.WebSocket.Protocol.State_Class);
   begin
      Free (Socket.Socket);
      Unchecked_Free (Socket.State);

      if Socket.P_State /= null then
         Unchecked_Free (Socket.P_State.State);
         Unchecked_Free (Socket.P_State);
      end if;
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

   ----------------------
   -- Protocol_Version --
   ----------------------

   function Protocol_Version (Socket : Object) return Natural is
   begin
      return Socket.Version;
   end Protocol_Version;

   -------------
   -- Receive --
   -------------

   overriding procedure Receive
     (Socket : Object;
      Data   : out Stream_Element_Array;
      Last   : out Stream_Element_Offset) is
   begin
      Socket.P_State.State.Receive (Socket, Data, Last);
   end Receive;

   ----------
   -- Send --
   ----------

   overriding procedure Send
     (Socket : Object;
      Data   : Stream_Element_Array;
      Last   : out Stream_Element_Offset) is
   begin
      Socket.Socket.Send (Data, Last);
   end Send;

   procedure Send (Socket : in out Object; Message : String) is
   begin
      Socket.State.Kind := Text;
      Socket.P_State.State.Send
        (Socket, Translator.To_Stream_Element_Array (Message));
   exception
      when E : others =>
         Socket.On_Error (Exception_Message (E));
   end Send;

   --------------
   -- Shutdown --
   --------------

   overriding procedure Shutdown
     (Socket : Object;
      How    : Shutmode_Type := Shut_Read_Write) is
   begin
      Socket.Socket.Shutdown (How);
   end Shutdown;

   ---------
   -- URI --
   ---------

   function URI (Socket : Object) return String is
   begin
      return AWS.Status.URI (Socket.Request);
   end URI;

end AWS.Net.WebSocket;
