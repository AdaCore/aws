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

with Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants;
with System;

with GNAT.Byte_Swapping;
with GNAT.MD5;

with AWS.Messages;
with AWS.Net.Buffered;
with AWS.Headers;

package body AWS.Net.WebSocket.Protocol.Draft76 is

   -------------
   -- Receive --
   -------------

   procedure Receive
     (Socket : Object;
      Data   : out Stream_Element_Array;
      Last   : out Stream_Element_Offset) is
   begin
      null;
   end Receive;

   ----------
   -- Send --
   ----------

   procedure Send
     (Socket : Object;
      Data   : Stream_Element_Array) is
   begin
      null;
   end Send;

   ---------------
   -- Send_Body --
   ---------------

   procedure Send_Body
     (Sock : Net.Socket_Type'Class; Request : AWS.Status.Data)
   is
      use System;
      use GNAT;

      Headers : constant AWS.Headers.List := AWS.Status.Header (Request);

      function WS_Key_Value (Key : String) return Interfaces.Unsigned_32;
      --  Returns a WebSocket key as an Unsigned_32

      ------------------
      -- WS_Key_Value --
      ------------------

      function WS_Key_Value (Key : String) return Interfaces.Unsigned_32 is
         use type Interfaces.Unsigned_32;

         Spaces : constant Interfaces.Unsigned_32 :=
                    Interfaces.Unsigned_32 (Strings.Fixed.Count (Key, " "));
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
            end if;
         end loop;

         return Interfaces.Unsigned_32'Value (N (N'First .. I)) / Spaces;
      end WS_Key_Value;

      --  Key 1

      K1 : constant String :=
             Headers.Get_Values (Messages.Sec_WebSocket_Key1_Token);
      V1 : constant Interfaces.Unsigned_32 := WS_Key_Value (K1);
      B1 : Stream_Element_Array (1 .. 4);
      for B1'Address use V1'Address;

      --  Key 2

      K2 : constant String :=
             Headers.Get_Values (Messages.Sec_WebSocket_Key2_Token);
      V2 : constant Interfaces.Unsigned_32 := WS_Key_Value (K2);
      B2 : Stream_Element_Array (1 .. 4);
      for B2'Address use V2'Address;

      --  Body

      B  : constant Stream_Element_Array := AWS.Status.Binary_Data (Request);

      C  : MD5.Context;
      D  : MD5.Message_Digest;
      S  : Stream_Element_Array (1 .. D'Length);
      for S'Address use D'Address;

   begin
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
   end Send_Body;

end AWS.Net.WebSocket.Protocol.Draft76;
