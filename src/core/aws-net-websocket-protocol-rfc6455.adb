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

   type Opcode is mod 15;
   for Opcode'Size use 4;

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

   type Masking_Key is new Stream_Element_Array (0 .. 3);
   for Masking_Key'Size use 32;

   procedure Send
     (Socket : Object;
      Opcd   : Opcode;
      Data   : Stream_Element_Array);
   --  Internal version

   -------------
   -- Receive --
   -------------

   procedure Receive
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

   procedure Send
     (Socket : Object;
      Data   : Stream_Element_Array) is
   begin
      if Socket.State.Kind = Text then
         Send (Socket, O_Text, Data);
      else
         Send (Socket, O_Binary, Data);
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
