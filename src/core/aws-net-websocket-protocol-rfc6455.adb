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

--  This implements the WebSocket protocol as defined in RFC-6455

with Ada.Strings.Fixed;
with System;

with GNAT.Byte_Swapping;
with GNAT.SHA1;

with AWS.Messages;
with AWS.Net.Buffered;
with AWS.Translator;

package body AWS.Net.WebSocket.Protocol.RFC6455 is

   type Bit is range 0 .. 1;
   for Bit'Size use 1;

   O_Continuation     : constant Opcode := 16#0#;
   O_Text             : constant Opcode := 16#1#;
   O_Binary           : constant Opcode := 16#2#;
   O_Connection_Close : constant Opcode := 16#8#;
   O_Ping             : constant Opcode := 16#9#;
   O_Pong             : constant Opcode := 16#A#;

   pragma Warnings (Off);
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

   pragma Warnings (On);

   procedure Send
     (Protocol : in out State;
      Socket   : Object;
      Opcd     : Opcode;
      Data     : Stream_Element_Array);
   --  Internal version

   --------------------
   -- End_Of_Message --
   --------------------

   overriding function End_Of_Message (Protocol : State) return Boolean is
   begin
      return Protocol.Remaining = 0 and then Protocol.Last_Fragment;
   end End_Of_Message;

   -------------
   -- Receive --
   -------------

   overriding procedure Receive
     (Protocol : in out State;
      Socket   : Object;
      Data     : out Stream_Element_Array;
      Last     : out Stream_Element_Offset)
   is
      use GNAT;
      use System;

      procedure Read_Payload
        (Protocol : in out State; Length : Stream_Element_Offset);
      --  Read the Length bytes of the payload

      procedure Read_Data (Data : out Stream_Element_Array);
      --  Read data from the socket to fill Data array

      ---------------
      -- Read_Data --
      ---------------

      procedure Read_Data (Data : out Stream_Element_Array) is
         First : Stream_Element_Offset := Data'First;
         Last  : Stream_Element_Offset;
      begin
         loop
            Socket.Socket.Receive (Data (First .. Data'Last), Last);
            exit when Last = Data'Last;
            First := Last + 1;
         end loop;
      end Read_Data;

      ------------------
      -- Read_Payload --
      ------------------

      procedure Read_Payload
        (Protocol : in out State;
         Length   : Stream_Element_Offset)
      is
         Read_Before : constant Stream_Element_Offset := Protocol.Read;
         Read        : Stream_Element_Offset;
         First       : Stream_Element_Offset := Data'First;
         Max         : Stream_Element_Offset;
      begin
         Last := Data'First + Length - 1;

         Max := Stream_Element_Offset'Min (Data'Last, Last);

         if Length > 0 then
            loop
               Socket.Socket.Receive (Data (First .. Max), Last);

               Read := Last - First + 1;

               Protocol.Read      := Protocol.Read + Read;
               Protocol.Remaining := Protocol.Remaining - Read;

               exit when Protocol.Remaining = 0
                 or else Last = Data'Last;

               First := Last + 1;
            end loop;

            --  If the message is masked, apply it

            if Protocol.Has_Mask then
               for K in Data'First .. Last loop
                  Data (K) := Data (K)
                    xor Protocol.Mask ((Read_Before + K - Data'First) mod 4);
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

      To_Read  : Stream_Element_Offset;

      L_State    : State := Protocol;
      Opcd       : Opcode;

   begin
      pragma Assert (Data'Length > 10);
      --  This is to ease reading frame header data

      --  if a new message is expected, read header

      if L_State.Remaining = 0 then
         Read_Data (D_Header);

         if Header.Payload_Length = 126 then
            Read_Data (D_16);

            if Default_Bit_Order = Low_Order_First then
               Byte_Swapping.Swap2 (L_16'Address);
            end if;

            L_State.Remaining := Stream_Element_Offset (L_16);

         elsif Header.Payload_Length = 127 then
            Read_Data (D_64);

            if Default_Bit_Order = Low_Order_First then
               Byte_Swapping.Swap8 (L_64'Address);
            end if;

            L_State.Remaining := Stream_Element_Offset (L_64);

         else
            L_State.Remaining :=
              Stream_Element_Offset (Header.Payload_Length);
         end if;

         if Header.Mask = 1 then
            Read_Data (Stream_Element_Array (L_State.Mask));
         end if;

         --  Set corresponding data in protocol state.
         --  In case of a continuation frame we reuse the previous code.

         if Header.Opcd = O_Continuation then
            Opcd := L_State.Opcd;
         else
            Opcd := Header.Opcd;
         end if;

         L_State.Has_Mask      := Header.Mask = 1;
         L_State.Read          := 0;
         L_State.Last_Fragment := Header.FIN = 1;
         L_State.Opcd          := Opcd;

      else
         Opcd := L_State.Opcd;
      end if;

      --  Check for wrong headers

      if Header.RSV1 /= 0
        or else Header.RSV2 /= 0
        or else Header.RSV3 /= 0
      then
         Socket.State.Kind := Unknown;
         Last := 0;
         Socket.Shutdown;
         return;
      end if;

      --  Read payload data

      To_Read := Stream_Element_Offset'Min (Data'Length, L_State.Remaining);

      case Opcd is
         when O_Text =>
            Socket.State.Kind := Text;
            Read_Payload (L_State, To_Read);
            Protocol := L_State;

         when O_Binary =>
            Socket.State.Kind := Binary;
            Read_Payload (L_State, To_Read);
            Protocol := L_State;

         when O_Connection_Close =>
            Read_Payload (L_State, To_Read);

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

            if not Protocol.Close_Sent then
               Protocol.Close_Sent := True;

               --  Just echo the status code we received as per RFC

               Send
                 (Protocol,
                  Socket, O_Connection_Close, Data (Data'First .. Last));
            end if;

            Socket.State.Kind := Connection_Close;

         when O_Ping =>
            Socket.State.Kind := Ping;
            Read_Payload (L_State, To_Read);

            --  Just echo with the application data. Note that a control
            --  message must not be fragmented.

            if Header.Payload_Length <= 125 and then Header.FIN = 1 then
               Send (Protocol, Socket, O_Pong, Data (Data'First .. Last));
            else
               Socket.State.Kind := Unknown;
               Socket.Shutdown;
            end if;

         when O_Pong =>
            Socket.State.Kind := Pong;
            Read_Payload (L_State, To_Read);

            --  Note that a control message must not be fragmented

            if Header.Payload_Length > 125 or else Header.FIN = 0 then
               Socket.State.Kind := Unknown;
               Socket.Shutdown;
            end if;

         when O_Continuation =>
            --  Nothing to do in this case. Continuation frames are handled
            --  above by changing the code to the proper one.
            null;

         when others =>
            --  Opcode for future enhancement of the protocol
            Socket.State.Kind := Unknown;
            Socket.Shutdown;
      end case;
   end Receive;

   ----------
   -- Send --
   ----------

   procedure Send
     (Protocol : in out State;
      Socket   : Object;
      Opcd     : Opcode;
      Data     : Stream_Element_Array)
   is
      pragma Unreferenced (Protocol);
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
      Header.RSV1 := 0;
      Header.RSV2 := 0;
      Header.RSV3 := 0;

      --  Compute proper message length, see RFC-6455 for a full description
      --
      --  <= 125    Payload_Length is the actual length
      --  <= 65535  The actual length is in the 2 following bytes
      --            and set payload length to 126
      --  otherwise The actual length is in the 8 following bytes
      --            and set payload length to 127

      if Data'Length <= 125 then
         Header.Payload_Length := Data'Length;

      elsif Data'Length <= 65535 then
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
      elsif Data'Length <= 65535 then
         Net.Buffered.Write (Socket, D_16);
      else
         Net.Buffered.Write (Socket, D_64);
      end if;

      --  Send payload

      Net.Buffered.Write (Socket, Data);

      Net.Buffered.Flush (Socket);
   end Send;

   overriding procedure Send
     (Protocol : in out State;
      Socket   : Object;
      Data     : Stream_Element_Array) is
   begin
      if Socket.State.Kind = Text then
         Send (Protocol, Socket, O_Text, Data);
      else
         Send (Protocol, Socket, O_Binary, Data);
      end if;
   end Send;

   -----------------
   -- Send_Header --
   -----------------

   procedure Send_Header
     (Sock : Net.Socket_Type'Class; Request : AWS.Status.Data)
   is
      use GNAT;

      GUID : constant String :=
               "258EAFA5-E914-47DA-95CA-C5AB0DC85B11";
      --  As specified into the RFC-6455
      Key  : constant String :=
               Strings.Fixed.Trim
                 (AWS.Status.Sec_WebSocket_Key (Request), Strings.Both);
   begin
      declare
         SHA : constant String := SHA1.Digest (Key & GUID);
         --  The SHA-1 as a string
         Hex : Stream_Element_Array (1 .. SHA'Length / 2);
         --  The SHA-1 as an hexadecimal array
         --  ??? Use new service to retrieve the SHA-1 in binary format when
         --  GNAT GPL supports it.
      begin
         for K in 1 .. SHA'Length / 2 loop
            Hex (Stream_Element_Offset (K)) :=
              Stream_Element'Value ("16#" & SHA (K * 2 - 1 .. K * 2) & '#');
         end loop;

         Net.Buffered.Put_Line
           (Sock,
            Messages.Sec_WebSocket_Accept (Translator.Base64_Encode (Hex)));
      end;
   end Send_Header;

end AWS.Net.WebSocket.Protocol.RFC6455;
