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

with Ada.Strings.Maps.Constants;
with Ada.Text_IO;
with System;

with GNAT.Byte_Swapping;
with GNAT.MD5;

with AWS.Headers;
with AWS.Messages;
with AWS.Net.Buffered;
with AWS.Translator;

package body AWS.Net.WebSocket.Protocol.Draft76 is

   use Ada.Text_IO;

   -------------------------
   -- Add_Connect_Headers --
   -------------------------

   overriding procedure Add_Connect_Headers
     (Protocol : State;
      URI      : String;
      Headers  : in out AWS.Headers.List)
   is
      pragma Unreferenced (Protocol, URI, Headers);
   begin
      raise Program_Error
        with "Connecting with draft76 protocol is not supported";
   end Add_Connect_Headers;

   --------------------
   -- End_Of_Message --
   --------------------

   overriding function End_Of_Message (Protocol : State) return Boolean is
      pragma Unreferenced (Protocol);
   begin
      return True;
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
      pragma Unreferenced (Protocol);
      Byte    : Stream_Element_Array (1 .. 1);
      Ignored : Stream_Element_Offset;
   begin
      Ignored := 1;
      Socket.Socket.Receive (Byte, Ignored);

      if Byte (Byte'First) /= 0 then
         Put_Line ("WebSocket 'received' did not receive a 0x00");
      else
         Last := Data'First;
         loop
            Ignored := 1;
            Socket.Socket.Receive (Byte, Ignored);
            exit when Byte (Byte'First) = 16#FF#;

            Data (Last) := Byte (Byte'First);
            Last := Last + 1;
         end loop;
      end if;

   exception
      when AWS.Net.Socket_Error =>
         null;
      when E : others =>
         Put_Line ("Unexpected exception" & Exception_Information (E));
   end Receive;

   ----------
   -- Send --
   ----------

   overriding procedure Send
     (Protocol : in out State;
      Socket   : Object;
      Data     : Stream_Element_Array)
   is
      pragma Unreferenced (Protocol);
      D_Header : constant Stream_Element_Array (1 .. 1) := (1 => 16#00#);
      D_Footer : constant Stream_Element_Array (1 .. 1) := (1 => 16#FF#);
   begin
      Net.Buffered.Write (Socket, D_Header);

      --  ??? Should encode in UTF-8 (to make sure there is no 16#FF# byte in
      --  the message)
      Net.Buffered.Write (Socket, Data);

      Net.Buffered.Write (Socket, D_Footer);
      Net.Buffered.Flush (Socket);
   end Send;

   overriding procedure Send
     (Protocol : in out State;
      Socket   : Object;
      Data     : Unbounded_String) is
   begin
      Send
        (Protocol, Socket,
         Translator.To_Stream_Element_Array (To_String (Data)));
   end Send;

   -----------------
   -- Send_Header --
   -----------------

   procedure Send_Header
     (Sock : Net.Socket_Type'Class; Request : AWS.Status.Data)
   is
      use GNAT;
      use System;

      Headers : constant AWS.Headers.List := AWS.Status.Header (Request);

      function WS_Key_Value (Key : String) return Interfaces.Unsigned_32;
      --  Returns a WebSocket key as an Unsigned_32

      ------------------
      -- WS_Key_Value --
      ------------------

      function WS_Key_Value (Key : String) return Interfaces.Unsigned_32 is
         use type Interfaces.Unsigned_32;
         Spaces : Interfaces.Unsigned_32 := 0;
         Value  : Interfaces.Unsigned_32;
         Result : Interfaces.Unsigned_32;
         N      : String (1 .. Key'Length);
         I      : Natural := 0;

      begin
         --  Get all digits

         for K in Key'Range loop
            if Strings.Maps.Is_In
              (Key (K), Strings.Maps.Constants.Decimal_Digit_Set)
            then
               I := I + 1;
               N (I) := Key (K);

            elsif Key (K) = ' ' then
               Spaces := Spaces + 1;
            end if;
         end loop;

         Value := Interfaces.Unsigned_32'Value (N (N'First .. I));

         --  ??? Should check that Spaces /= 0
         --  ??? Should check that Value is an integral multiple of Spaces

         Result := Value / Spaces;
         return Result;
      end WS_Key_Value;

      --  Key 1

      K1 : constant String :=
             Headers.Get_Values (Messages.Sec_WebSocket_Key1_Token);
      V1 : constant Interfaces.Unsigned_32 := WS_Key_Value (K1);
      B1 : Stream_Element_Array (1 .. 4);
      for B1'Address use V1'Address;
      pragma Import (Ada, B1);  --  disable default initialization

      --  Key 2

      K2 : constant String :=
             Headers.Get_Values (Messages.Sec_WebSocket_Key2_Token);
      V2 : constant Interfaces.Unsigned_32 := WS_Key_Value (K2);
      B2 : Stream_Element_Array (1 .. 4);
      for B2'Address use V2'Address;
      pragma Import (Ada, B2);  --  disable default initialization

      --  Body

      B  : constant Stream_Element_Array :=
             AWS.Status.Binary_Data (Request);

      C  : MD5.Context;
      D  : MD5.Message_Digest;
      S  : Stream_Element_Array (1 .. D'Length);
      for S'Address use D'Address;
      pragma Import (Ada, S);  --  disable default initialization

   begin
      if B'Length /= 8 then
         Put_Line ("Error, could not read binary_data from request");
      end if;

      --  Send protocol-specific headers

      Net.Buffered.Put_Line
        (Sock,
         AWS.Messages.Sec_WebSocket_Location_Token
         & ": ws://"
         & AWS.Status.Host (Request)
         & AWS.Status.URI (Request));

      Net.Buffered.Put_Line
        (Sock,
         AWS.Messages.Sec_WebSocket_Origin_Token
         & ": " & AWS.Status.Origin (Request));

      --  End of header

      Net.Buffered.New_Line (Sock);

      --  Send body of the handshake (ie answer the challenge)

      --  This is only steps "4." and later of the draft protocol, the rest of
      --  the handshake has already been conducted.

      if Default_Bit_Order = Low_Order_First then
         Byte_Swapping.Swap4 (B1'Address);
         Byte_Swapping.Swap4 (B2'Address);
      end if;

      --  Compute MD5 of B1 & B2 & B

      MD5.Update (C, B1);
      MD5.Update (C, B2);
      MD5.Update (C, B);

      D := MD5.Digest (C);

      --  The following output is the body for the response

      declare
         V : Natural;
         S : Natural;
      begin
         for K in 1 .. D'Length / 2 loop
            S := K * 2 - 1;
            V := Natural'Value ("16#" & D (S .. S + 1) & "#");

            Net.Buffered.Put (Sock, String'(1 => Character'Val (V)));
         end loop;
      end;

      Net.Buffered.Flush (Sock);
   end Send_Header;

end AWS.Net.WebSocket.Protocol.Draft76;
