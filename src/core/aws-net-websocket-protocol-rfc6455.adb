------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2012-2017, AdaCore                     --
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

   No_Masking_Key : constant Masking_Key := (others => 0);
   --  The masking-key when sending from client to server

   type Frame_Header is record
      FIN            : Bit;
      RSV1           : Bit;
      RSV2           : Bit;
      RSV3           : Bit;
      Opcd           : Opcode;
      Mask           : Bit;
      Payload_Length : Integer range 0 .. 127;
   end record with Pack;

   for Frame_Header'Size use 16;
   for Frame_Header'Alignment use 1;
   for Frame_Header'Bit_Order use System.High_Order_First;

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

   procedure Send_Frame_Header
     (Protocol    : in out State;
      Socket      : Object;
      Opcd        : Opcode;
      Data_Length : Stream_Element_Offset;
      Has_Mask    : Boolean := False;
      Mask        : Masking_Key := No_Masking_Key);
   --  Send the frame header only.
   --  Mask and Has_Mask should be set when sending from client to server, and
   --  left to the default from server to client.

   procedure Send_Frame
     (Protocol : in out State;
      Socket   : Object;
      Opcd     : Opcode;
      Data     : Stream_Element_Array;
      Error    : Status_Code := 0);
   --  Send the frame (header + data)

   function Is_Library_Error (Code : Interfaces.Unsigned_16) return Boolean;
   --  Returns True if Code is a valid library error code

   function Is_Valid_Close_Code (Error : Error_Type) return Boolean;
   --  Returns True if the Error code is valid

   function Get_Websocket_Accept (Key : String) return String;
   --  Compute the 'Accept' response based on the key sent by the client

   function Create_Random_Mask return Masking_Key;
   --  Create a random masking key

   -------------------------
   -- Add_Connect_Headers --
   -------------------------

   overriding procedure Add_Connect_Headers
      (Protocol : State;
       Host     : String;
       Headers  : in out AWS.Headers.List)
   is
      pragma Unreferenced (Protocol);
      Ints : array (1 .. 4) of AWS.Utils.Random_Integer :=
              (others => AWS.Utils.Random);
      H : Stream_Element_Array (1 .. 16) with Import, Address => Ints'Address;

   begin
      Headers.Add ("Host", Host);
      Headers.Add ("Upgrade", "WebSocket");
      Headers.Add ("Connection", "Upgrade");
      Headers.Add ("Sec-WebSocket-Key", Translator.Base64_Encode (H));
      Headers.Add ("Sec-WebSocket-Version", "13");
   end Add_Connect_Headers;

   ----------------------------
   -- Check_Connect_Response --
   ----------------------------

   overriding function Check_Connect_Response
      (Protocol : State;
       Request  : AWS.Headers.List;
       Response : AWS.Response.Data)
      return Boolean
   is
      pragma Unreferenced (Protocol);
      Expected : constant String :=
                   Get_Websocket_Accept
                      (AWS.Headers.Get
                         (Request, AWS.Messages.Sec_WebSocket_Key_Token));
      Actual : constant String := AWS.Response.Header
                (Response, AWS.Messages.Sec_WebSocket_Accept_Token);
   begin
      return Expected = Actual;
   end Check_Connect_Response;

   -----------
   -- Close --
   -----------

   overriding procedure Close
     (Protocol : in out State;
      Socket   : Object;
      Data     : String;
      Error    : Status_Code) is
   begin
      Send_Frame
        (Protocol, Socket, O_Connection_Close,
         Translator.To_Stream_Element_Array (Data), Error);
      Protocol.Close_Sent := True;
   end Close;

   ------------------------
   -- Create_Random_Mask --
   ------------------------

   function Create_Random_Mask return Masking_Key is
      Int : constant AWS.Utils.Random_Integer := AWS.Utils.Random;
      Arr : Masking_Key with Import, Address => Int'Address;
   begin
      return Arr;
   end Create_Random_Mask;

   --------------------
   -- End_Of_Message --
   --------------------

   overriding function End_Of_Message (Protocol : State) return Boolean is
   begin
      return Protocol.Remaining = 0 and then Protocol.Last_Fragment;
   end End_Of_Message;

   --------------------------
   -- Get_Websocket_Accept --
   --------------------------

   function Get_Websocket_Accept (Key : String) return String is
      use GNAT;
      GUID     : constant String := "258EAFA5-E914-47DA-95CA-C5AB0DC85B11";
      Trim_Key : constant String := Strings.Fixed.Trim (Key, Strings.Both);
   begin
      declare
         SHA : constant String := SHA1.Digest (Trim_Key & GUID);
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

         return Translator.Base64_Encode (Hex);
      end;
   end Get_Websocket_Accept;

   --------------------
   -- Is_Error_Valid --
   --------------------

   function Is_Library_Error (Code : Interfaces.Unsigned_16) return Boolean is
   begin
      return Code in 3000 .. 4999;
   end Is_Library_Error;

   -------------------------
   -- Is_Valid_Close_Code --
   -------------------------

   function Is_Valid_Close_Code (Error : Error_Type) return Boolean is
   begin
      case Error is
         when Normal_Closure | Going_Away | Protocol_Error | Unsupported_Data
           | Invalid_Frame_Payload_Data .. Internal_Server_Error
           =>
            return True;

         when others =>
            return False;
      end case;
   end Is_Valid_Close_Code;

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
      Header   : Frame_Header with Address => D_Header'Address;
      pragma Import (Ada, Header);  --  Disable default initialization

      D_16     : Stream_Element_Array (1 .. 2);
      for D_16'Alignment use Interfaces.Unsigned_16'Alignment;
      L_16     : Interfaces.Unsigned_16 with Address => D_16'Address;

      D_64     : Stream_Element_Array (1 .. 8);
      for D_64'Alignment use Interfaces.Unsigned_64'Alignment;
      L_64     : Interfaces.Unsigned_64 with Address => D_64'Address;

      To_Read  : Stream_Element_Offset;

      L_State    : State := Protocol;
      Opcd       : Opcode;
      Bad_Header : Boolean := False;

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

         --  Check for wrong headers:
         --     - RSV? must be zero
         --     - continuation frame when there is nothing to continue

         Bad_Header := Header.RSV1 /= 0
           or else Header.RSV2 /= 0
           or else Header.RSV3 /= 0
           or else (Header.Opcd = O_Continuation
                    and then Protocol.Last_Fragment);

         --  Set corresponding data in protocol state.
         --  In case of a continuation frame we reuse the previous code.

         if Header.Opcd = O_Continuation then
            Opcd := L_State.Opcd;
         else
            Opcd := Header.Opcd;

            --  In case we have a O_Text or O_Binary message that is in fact
            --  a continuation of a message we must fail. The protocol requires
            --  that a continuation frame must have O_Continuation.

            Bad_Header := Bad_Header
              or else
                ((Header.Opcd = O_Text or else Header.Opcd = O_Binary)
                 and then not Protocol.Last_Fragment);
         end if;

         if Bad_Header then
            Socket.State.Kind := Unknown;
            Last := 0;
            return;
         end if;

         L_State.Has_Mask      := Header.Mask = 1;
         L_State.Read          := 0;
         L_State.Last_Fragment := Header.FIN = 1;
         L_State.Opcd          := Opcd;

      else
         Opcd := L_State.Opcd;
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

            --  A control frame must not be fragmented and have max 125
            --  bytes payload.

            if Header.Payload_Length <= 125 and then Header.FIN = 1 then

               --  Check the error code if any

               if Last - Data'First >= 1 then
                  --  The first two bytes are the status code
                  declare
                     D : Stream_Element_Array (1 .. 2) :=
                           Data (Data'First .. Data'First + 1);
                     E : Interfaces.Unsigned_16 with Address => D'Address;
                  begin
                     if Default_Bit_Order = Low_Order_First then
                        Byte_Swapping.Swap2 (E'Address);
                     end if;
                     Socket.State.Errno := E;

                     --  If we have a wrong code this is a Protocol_Error

                     if (Is_Library_Error (E)
                         or else Is_Valid_Close_Code (Error (Socket)))
                       and then
                         --  A close message must be a valid UTF-8 string
                         Utils.Is_Valid_UTF8
                           (Translator.To_String
                             (Data (Data'First + 2 .. Last)))
                     then
                        null;
                     else
                        E := Error_Code (Protocol_Error);
                     end if;

                     --  Set back Errno

                     Socket.State.Errno := E;
                  end;

               elsif Last - Data'First = 0 then
                  --  A single byte, we are missing the status code
                  Socket.State.Errno := Error_Code (Protocol_Error);

               else
                  --  Empty payload, this is a normal closure
                  Socket.State.Errno := Error_Code (Normal_Closure);
               end if;

               --  If needed send a close frame

               if not Protocol.Close_Sent then
                  Protocol.Close_Sent := True;

                  --  Just echo the status code we received as per RFC

                  Send_Frame
                    (Protocol,
                     Socket, O_Connection_Close,
                     Data (Data'First + 2 .. Last),
                     Error => Socket.State.Errno);
               end if;

               Socket.State.Kind := Connection_Close;

            else
               Socket.State.Kind := Unknown;
            end if;

         when O_Ping =>
            Socket.State.Kind := Ping;
            Read_Payload (L_State, To_Read);

            --  Just echo with the application data. Note that a control
            --  message must not be fragmented.

            if Header.Payload_Length <= 125 and then Header.FIN = 1 then
               Send_Frame
                 (Protocol, Socket, O_Pong, Data (Data'First .. Last));
            else
               Socket.State.Kind := Unknown;
            end if;

         when O_Pong =>
            Socket.State.Kind := Pong;
            Read_Payload (L_State, To_Read);

            --  Note that a control message must not be fragmented

            if Header.Payload_Length > 125 or else Header.FIN = 0 then
               Socket.State.Kind := Unknown;
            end if;

         when O_Continuation =>
            --  Nothing to do in this case. Continuation frames are handled
            --  above by changing the code to the proper one.
            null;

         when others =>
            --  Opcode for future enhancement of the protocol, they are
            --  illegal at this stage and the connection is required to be
            --  shutdown.
            Socket.State.Kind := Unknown;
      end case;
   end Receive;

   ----------
   -- Send --
   ----------

   overriding procedure Send
     (Protocol    : in out State;
      Socket      : Object;
      Data        : Unbounded_String)
   is
      From_Client : constant Boolean := Socket.Is_Client_Side;
      Len_Data    : constant Natural := Length (Data);
      Chunk_Size  : constant Positive := 4_096;
      First       : Positive := 1;
      Last        : Natural;
      Mask        : Masking_Key;
      Mask_Pos    : Masking_Key_Index := 0;
   begin
      if From_Client then
         Mask := Create_Random_Mask;
      end if;

      if Socket.State.Kind = Text then
         Send_Frame_Header
           (Protocol, Socket, O_Text, Stream_Element_Offset (Len_Data),
            Has_Mask => From_Client,
            Mask     => Mask);
      else
         Send_Frame_Header
           (Protocol, Socket, O_Binary, Stream_Element_Offset (Len_Data),
            Has_Mask => From_Client,
            Mask     => Mask);
      end if;

      Send_Data : loop
         Last := Positive'Min (Len_Data, First + Chunk_Size - 1);

         declare
            S : Stream_Element_Array :=
                 Translator.To_Stream_Element_Array
                    (Slice (Data, First, Last));
         begin
            if From_Client then
               for Idx in S'Range loop
                  S (Idx) := S (Idx)
                     xor Mask (Stream_Element_Offset (Mask_Pos));
                  Mask_Pos := Mask_Pos + 1;
               end loop;
            end if;

            Net.Buffered.Write (Socket, S);
         end;

         exit Send_Data when Last = Len_Data;

         First := Last + 1;
      end loop Send_Data;

      Net.Buffered.Flush (Socket);
   end Send;

   overriding procedure Send
     (Protocol : in out State;
      Socket   : Object;
      Data     : Stream_Element_Array)
   is
      From_Client : constant Boolean := Socket.Is_Client_Side;

      Mask     : Masking_Key;
      Mask_Pos : Masking_Key_Index := 0;
   begin
      if From_Client then
         Mask := Create_Random_Mask;
      end if;

      if Socket.State.Kind = Text then
         Send_Frame_Header (Protocol, Socket, O_Text, Data'Length,
                            Has_Mask => From_Client, Mask => Mask);
      else
         Send_Frame_Header (Protocol, Socket, O_Binary, Data'Length,
                            Has_Mask => From_Client, Mask => Mask);
      end if;

      --  Send payload

      if From_Client then
         declare
            D : Stream_Element_Array (Data'Range);
         begin
            for Idx in Data'Range loop
               D (Idx) := Data (Idx)
                  xor Mask (Stream_Element_Offset (Mask_Pos));
               Mask_Pos := Mask_Pos + 1;
            end loop;

            Net.Buffered.Write (Socket, D);
         end;

      else
         Net.Buffered.Write (Socket, Data);
      end if;

      Net.Buffered.Flush (Socket);
   end Send;

   ----------------
   -- Send_Frame --
   ----------------

   procedure Send_Frame
     (Protocol : in out State;
      Socket   : Object;
      Opcd     : Opcode;
      Data     : Stream_Element_Array;
      Error    : Status_Code := 0)
   is
      use GNAT;
      use System;
      use type Status_Code;

      Error_Code_Needed : constant Boolean :=
                            Opcd = O_Connection_Close and then Error > 0;

      Frame_Length      : constant Stream_Element_Offset :=
                            Data'Length + (if Error_Code_Needed then 2 else 0);

   begin
      Send_Frame_Header (Protocol, Socket, Opcd, Frame_Length);

      --  Send the 2-byte error code for close control frame

      if Error_Code_Needed then
         declare
            D : Stream_Element_Array (1 .. 2);
            for D'Alignment use Interfaces.Unsigned_16'Alignment;
            E : Interfaces.Unsigned_16 := Error with Address => D'Address;
         begin
            if Default_Bit_Order = Low_Order_First then
               Byte_Swapping.Swap2 (E'Address);
            end if;
            Net.Buffered.Write (Socket, D);
         end;
      end if;

      --  Send payload

      Net.Buffered.Write (Socket, Data);

      Net.Buffered.Flush (Socket);
   end Send_Frame;

   -----------------------
   -- Send_Frame_Header --
   -----------------------

   procedure Send_Frame_Header
     (Protocol    : in out State;
      Socket      : Object;
      Opcd        : Opcode;
      Data_Length : Stream_Element_Offset;
      Has_Mask    : Boolean := False;
      Mask        : Masking_Key := No_Masking_Key)
   is
      pragma Unreferenced (Protocol);
      use GNAT;
      use System;

      D_Header : Stream_Element_Array (1 .. 2) := (0, 0);
      Header   : Frame_Header with Address => D_Header'Address;
      pragma Import (Ada, Header);  --  Disable default initialization

      D_16     : Stream_Element_Array (1 .. 2);
      for D_16'Alignment use Interfaces.Unsigned_16'Alignment;
      L_16     : Interfaces.Unsigned_16 with Address => D_16'Address;

      D_64     : Stream_Element_Array (1 .. 8);
      for D_64'Alignment use Interfaces.Unsigned_64'Alignment;
      L_64     : Interfaces.Unsigned_64 with Address => D_64'Address;

   begin
      Header.FIN := 1;
      Header.Opcd := Opcd;
      Header.Mask := (if Has_Mask then 1 else 0);
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

      if Data_Length <= 125 then
         Header.Payload_Length := Integer (Data_Length);

      elsif Data_Length <= 65535 then
         Header.Payload_Length := 126;
         L_16 := Interfaces.Unsigned_16 (Data_Length);

         if Default_Bit_Order = Low_Order_First then
            Byte_Swapping.Swap2 (L_16'Address);
         end if;

      else
         Header.Payload_Length := 127;
         L_64 := Interfaces.Unsigned_64 (Data_Length);

         if Default_Bit_Order = Low_Order_First then
            Byte_Swapping.Swap8 (L_64'Address);
         end if;
      end if;

      --  Send header

      Net.Buffered.Write (Socket, D_Header);

      --  Send extended length if any

      if Data_Length <= 125 then
         null;
      elsif Data_Length <= 65535 then
         Net.Buffered.Write (Socket, D_16);
      else
         Net.Buffered.Write (Socket, D_64);
      end if;

      --  Compute masking-key, when sending from client to server

      if Has_Mask then
         for J in Mask'Range loop
            Net.Buffered.Write (Socket, (1 => Mask (J)));
         end loop;
      end if;
   end Send_Frame_Header;

   -----------------
   -- Send_Header --
   -----------------

   procedure Send_Header
     (Sock : Net.Socket_Type'Class; Request : AWS.Status.Data)
   is
      Acc : constant String :=
              Get_Websocket_Accept
                (AWS.Status.Sec_WebSocket_Key (Request));
   begin
      Net.Buffered.Put_Line (Sock, Messages.Sec_WebSocket_Accept (Acc));
   end Send_Header;

end AWS.Net.WebSocket.Protocol.RFC6455;
