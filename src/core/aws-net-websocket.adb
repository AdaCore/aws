------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                       Copyright (C) 2012, AdaCore                        --
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
with System;

with GNAT.Byte_Swapping;

with AWS.Headers;
with AWS.Messages;
with AWS.Net.Buffered;
with AWS.Translator;

package body AWS.Net.WebSocket is

   use Ada.Streams;

   type Bit is range 0 .. 1;
   for Bit'Size use 1;

   type Opcode is mod 15;
   for Opcode'Size use 4;

   O_Continuation     : constant Opcode := 16#0#;
   O_Text             : constant Opcode := 16#1#;
   O_Binary           : constant Opcode := 16#2#;
   O_Connection_Close : constant Opcode := 16#8#;
   O_Ping             : constant Opcode := 16#9#;
   O_Pong             : constant Opcode := 16#A#;

   pragma Warnings (Off, "*reverse bit*");
   --  This is needed to kill warnings on big-endian targets

   type Frame_Header is record
      FIN            : Bit;
      RSV1           : Bit;
      RSV2           : Bit;
      RSV3           : Bit;
      Opcd           : Opcode;
      Mask           : Bit;
      Payload_Length : Integer range 0 .. 127;
   end record;

   for Frame_Header'Size use 16;
   for Frame_Header'Alignment use 1;
   for Frame_Header'Bit_Order use System.High_Order_First;

   pragma Pack (Frame_Header);

   for Frame_Header use record
      --  Byte 1
      FIN            at 0 range 0 .. 0;
      RSV1           at 0 range 1 .. 1;
      RSV2           at 0 range 2 .. 2;
      RSV3           at 0 range 3 .. 3;
      Opcd           at 0 range 4 .. 7;

      --  Byte 2
      Mask           at 1 range 0 .. 0;
      Payload_Length at 1 range 1 .. 7;
   end record;

   pragma Warnings (On, "*reverse bit*");

   type Masking_Key is new Stream_Element_Array (0 .. 3);
   for Masking_Key'Size use 32;

   procedure Send
     (Socket : Object;
      Opcd   : Opcode;
      Data   : Stream_Element_Array);
   --  Internal send procedure which handles control frames

   ------------
   -- Create --
   ------------

   function Create
     (Socket  : Socket_Access;
      Request : AWS.Status.Data) return Object'Class
   is

      function Create_Internal
        (Socket  : Socket_Access;
         Request : AWS.Status.Data) return Object;
      --  Main constructor

      ---------------------
      -- Create_Internal --
      ---------------------

      function Create_Internal
        (Socket  : Socket_Access;
         Request : AWS.Status.Data) return Object
      is
         Headers : constant AWS.Headers.List := AWS.Status.Header (Request);
         Version : Natural := 0;
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

         return Object'
           (Net.Socket_Type with
            Socket    => Socket,
            Request   => Request,
            Version   => Version,
            State     => new Internal_State'
              (Remaining  => -1,
               Kind       => Unknown,
               Close_Sent => False,
               Errno      => Interfaces.Unsigned_16'Last));
      end Create_Internal;

   begin
      return Create_Internal (Socket, Request);
   end Create;

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
   begin
      Free (Socket.Socket);
      Unchecked_Free (Socket.State);
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
      Last   : out Stream_Element_Offset)
   is
      use GNAT;
      use System;

      procedure Read_Payload
        (Length : Stream_Element_Offset; Mask : Boolean; Key : Masking_Key);
      --  Read the Length bytes of the payload

      ------------------
      -- Read_Payload --
      ------------------

      procedure Read_Payload
        (Length : Stream_Element_Offset; Mask : Boolean; Key : Masking_Key) is
      begin
         Last := Data'First + Length - 1;

         if Length > 0 then
            Socket.Socket.Receive (Data (Data'First .. Last), Last);

            --  If the message is masked, apply it

            if Mask then
               for K in Data'First .. Last loop
                  Data (K) := Data (K) xor Key ((K - Data'First) mod 4);
               end loop;
            end if;
         end if;
      end Read_Payload;

      D_Header : Stream_Element_Array (1 .. 2) := (0, 0);
      Header   : Frame_Header;
      for Header'Address use D_Header'Address;

      D_16     : Stream_Element_Array (1 .. 2);
      for D_16'Alignment use Interfaces.Unsigned_16'Alignment;
      L_16     : Interfaces.Unsigned_16;
      for L_16'Address use D_16'Address;

      D_64     : Stream_Element_Array (1 .. 8);
      for D_64'Alignment use Interfaces.Unsigned_64'Alignment;
      L_64     : Interfaces.Unsigned_64;
      for L_64'Address use D_64'Address;

      Mask     : Masking_Key;
      To_Read  : Stream_Element_Offset;
   begin
      pragma Assert (Data'Length > 10);
      --  This is to ease reading frame header data

      --  if a new message is expected, read header

      if Socket.State.Remaining = -1 then
         Socket.Socket.Receive (D_Header, Last);

         if Header.Payload_Length = 126 then
            Socket.Socket.Receive (D_16, Last);

            if Default_Bit_Order = Low_Order_First then
               Byte_Swapping.Swap2 (L_16'Address);
            end if;

            Socket.State.Remaining := Stream_Element_Offset (L_16);

         elsif Header.Payload_Length = 127 then
            Socket.Socket.Receive (D_64, Last);

            if Default_Bit_Order = Low_Order_First then
               Byte_Swapping.Swap8 (L_64'Address);
            end if;

            Socket.State.Remaining := Stream_Element_Offset (L_64);

         else
            Socket.State.Remaining :=
              Stream_Element_Offset (Header.Payload_Length);
         end if;

         if Header.Mask = 1 then
            Socket.Socket.Receive (Stream_Element_Array (Mask), Last);
         end if;
      end if;

      --  Read payload data

      To_Read := Stream_Element_Offset'Min
        (Data'Length, Socket.State.Remaining);

      if Data'Length >= Socket.State.Remaining then
         --  Everything can be read now, next call will handle a new message
         --  frame.
         Socket.State.Remaining := -1;
      else
         Socket.State.Remaining := Socket.State.Remaining - To_Read;
      end if;

      case Header.Opcd is
         when O_Text =>
            Socket.State.Kind := Text;
            Read_Payload (To_Read, Header.Mask = 1, Mask);

         when O_Binary =>
            Socket.State.Kind := Binary;
            Read_Payload (To_Read, Header.Mask = 1, Mask);

         when O_Connection_Close =>
            Read_Payload (To_Read, Header.Mask = 1, Mask);

            --  Check the error code if any

            if Last - Data'First >= 1 then
               --  The first two bytes are the status code
               declare
                  D : Stream_Element_Array (1 .. 2) :=
                        Data (Data'First .. Data'First + 1);
                  E : Interfaces.Unsigned_16;
                  for E'Address use D'Address;
               begin
                  if Default_Bit_Order = Low_Order_First then
                     Byte_Swapping.Swap2 (E'Address);
                  end if;
                  Socket.State.Errno := E;
               end;

               --  Remove the status code from the returned message

               Data (Data'First .. Last - 2) := Data (Data'First + 2 .. Last);
               Last := Last - 2;
            end if;

            --  If needed send a close frame

            if not Socket.State.Close_Sent then
               Socket.State.Close_Sent := True;

               --  Just echo the status code we received as per RFC

               Send (Socket, O_Connection_Close, Data (Data'First .. Last));
            end if;

            Socket.State.Kind := Connection_Close;

         when O_Ping =>
            Read_Payload (To_Read, Header.Mask = 1, Mask);

            --  Just echo with the application data

               Send (Socket, O_Pong, Data (Data'First .. Last));

         when O_Pong =>
            --  Nothing to do, this means we have sent a ping frame
            null;

         when O_Continuation =>
            --  Not yet implemented
            null;

         when others =>
            --  Opcode for future enhancement of the protocol
            Socket.State.Kind := Unknown;
      end case;
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

   procedure Send
     (Socket : Object;
      Opcd   : Opcode;
      Data   : Stream_Element_Array)
   is
      use GNAT;
      use System;

      D_Header : Stream_Element_Array (1 .. 2) := (0, 0);
      Header   : Frame_Header;
      for Header'Address use D_Header'Address;

      D_16     : Stream_Element_Array (1 .. 2);
      for D_16'Alignment use Interfaces.Unsigned_16'Alignment;
      L_16     : Interfaces.Unsigned_16;
      for L_16'Address use D_16'Address;

      D_64     : Stream_Element_Array (1 .. 8);
      for D_64'Alignment use Interfaces.Unsigned_64'Alignment;
      L_64     : Interfaces.Unsigned_64;
      for L_64'Address use D_64'Address;
   begin
      Header.FIN := 1;
      Header.Opcd := Opcd;
      Header.Mask := 0;

      --  Compute proper message length, see RFC-6455 for a full description
      --
      --  <= 125    Payload_Length is the actual length
      --  <= 65536  The actual length is in the 2 following bytes
      --            and set payload length to 126
      --  otherwise The actual length is in the 8 following bytes
      --            and set payload length to 127

      if Data'Length <= 125 then
         Header.Payload_Length := Data'Length;

      elsif Data'Length <= 65536 then
         Header.Payload_Length := 126;
         L_16 := Data'Length;

         if Default_Bit_Order = Low_Order_First then
            Byte_Swapping.Swap2 (L_16'Address);
         end if;

      else
         Header.Payload_Length := 127;
         L_64 := Data'Length;

         if Default_Bit_Order = Low_Order_First then
            Byte_Swapping.Swap8 (L_64'Address);
         end if;
      end if;

      --  Send header

      Net.Buffered.Write (Socket, D_Header);

      --  Send extended length if any

      if Data'Length <= 125 then
         null;
      elsif Data'Length <= 65536 then
         Net.Buffered.Write (Socket, D_16);
      else
         Net.Buffered.Write (Socket, D_64);
      end if;

      --  Send payload

      Net.Buffered.Write (Socket, Data);

      Net.Buffered.Flush (Socket);
   end Send;

   procedure Send (Socket : in out Object; Message : String) is
   begin
      Send (Socket, O_Text, Translator.To_Stream_Element_Array (Message));
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
