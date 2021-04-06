------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                      Copyright (C) 2021, AdaCore                         --
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

with Ada.Integer_Text_IO;
with Ada.Text_IO;

with AWS.HTTP2.Frame.Continuation;
with AWS.HTTP2.Frame.Data;
with AWS.HTTP2.Frame.GoAway;
with AWS.HTTP2.Frame.Headers;
with AWS.HTTP2.Frame.Ping;
with AWS.HTTP2.Frame.Priority;
with AWS.HTTP2.Frame.Push_Promise;
with AWS.HTTP2.Frame.RST_Stream;
with AWS.HTTP2.Frame.Settings;
with AWS.HTTP2.Frame.Window_Update;
with AWS.Net.Buffered;

package body AWS.HTTP2.Frame is

   use Ada.Text_IO;

   use System;

   ------------
   -- Adjust --
   ------------

   overriding procedure Adjust (Self : in out Object) is
   begin
      Self.Counter.all := Self.Counter.all + 1;
   end Adjust;

   ----------
   -- Dump --
   ----------

   procedure Dump (Self : Object'Class; Message : String) is
   begin
      Put (Message & " - FRAME: Id:" & Integer (Self.Header.H.Stream_Id)'Img);
      Put ("  L:" & Integer (Self.Header.H.Length)'Img);
      Integer_Text_IO.Put
        (Integer (Self.Header.H.Length), Width => 8, Base => 16);
      Put_Line ("  Kind: " & Self.Header.H.Kind'Img
                & "   Flags:" & Self.Header.H.Flags'Img);

      Self.Dump_Payload;
   end Dump;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out Object) is
      Ref_Counter : Utils.Counter_Access := Self.Counter;
   begin
      --  Ensure call is idempotent

      Self.Counter := null;

      if Ref_Counter /= null then
         Ref_Counter.all := Ref_Counter.all - 1;

         if Ref_Counter.all = 0 then
            Utils.Unchecked_Free (Ref_Counter);
            Release (Self);
         end if;
      end if;
   end Finalize;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (Self : in out Object) is
   begin
      Self.Counter := new Natural'(1);
   end Initialize;

   ----------
   -- Read --
   ----------

   function Read (Sock : Net.Socket_Type'Class) return Object'Class is
      H : Object;
   begin
      --  Get the frame header

      Net.Buffered.Read (Sock, H.Header.S);

      --  Get the frame payload

      case H.Header.H.Kind is
         when K_Data =>
            return Frame.Data.Read (Sock, H);

         when K_Settings =>
            return Frame.Settings.Read (Sock, H);

         when K_Headers =>
            return Frame.Headers.Read (Sock, H);

         when K_Window_Update =>
            return Frame.Window_Update.Read (Sock, H);

         when K_RST_Stream =>
            return Frame.RST_Stream.Read (Sock, H);

         when K_GoAway =>
            return Frame.GoAway.Read (Sock, H);

         when K_Continuation =>
            return Frame.Continuation.Read (Sock, H);

         when K_Ping =>
            return Frame.Ping.Read (Sock, H);

         when K_Priority =>
            return Frame.Priority.Read (Sock, H);

         when K_Push_Promise =>
            return Frame.Push_Promise.Read (Sock, H);
      end case;
   end Read;

   ----------
   -- Send --
   ----------

   procedure Send (Self : Object'Class; Sock : Net.Socket_Type'Class) is
   begin
      --  Send header

      Net.Buffered.Write (Sock, Self.Header.S);

      --  Send payload if any

      if Self.Header.H.Length > 0 then
         Self.Send_Payload (Sock);
      end if;

      Net.Buffered.Flush (Sock);
   end Send;

   ---------------
   -- Set_Flags --
   ---------------

   procedure Set_Flags (Self : in out Object; Flags : Flags_Type) is
   begin
      Self.Header.H.Flags := Flags;
   end Set_Flags;

end AWS.HTTP2.Frame;
