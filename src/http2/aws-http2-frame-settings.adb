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

with Ada.Text_IO;

with AWS.Net.Buffered;

package body AWS.HTTP2.Frame.Settings is

   ---------
   -- Ack --
   ---------

   function Ack return Object is
   begin
      return Create (Empty_Set, Ack => True);
   end Ack;

   ------------
   -- Create --
   ------------

   function Create
     (Settings : Set;
      Ack      : Boolean := False) return Object
   is
      Len : constant Stream_Element_Count :=
              Settings'Length * Payload'Size / Stream_Element'Size;
   begin
      return O : Object do
         O.Header.H.Stream_Id := 0;
         O.Header.H.Length    := Length_Type (Len);
         O.Header.H.Kind      := K_Settings;
         O.Header.H.R         := 0;
         O.Header.H.Flags     := (if Ack then Ack_Flag else 0);

         O.Size := Settings'Length;

         if Settings'Length > 0 then
            O.Data.S := new Stream_Element_Array (1 .. Len);
            O.Data.P (1 .. Settings'Length) := Settings;
         end if;
      end return;
   end Create;

   function Create (Payload : Stream_Element_Array) return Object is
   begin
      return O : Object do
         O.Header.H.Stream_Id := 0;
         O.Header.H.Length    := Payload'Length;
         O.Header.H.Kind      := K_Settings;
         O.Header.H.R         := 0;
         O.Header.H.Flags     := 0;

         O.Size := Payload'Length / (Payload'Size / Stream_Element'Size);

         O.Data.S := new Stream_Element_Array'(Payload);
      end return;
   end Create;

   ------------------
   -- Dump_Payload --
   ------------------

   overriding procedure Dump_Payload (Self : Object) is
   begin
      if Self.Is_Ignored then
         Text_IO.Put_Line ("Ignored");

      else
         for K in 1 .. Self.Size loop
            Text_IO.Put_Line
              ("   S: " & Self.Data.P (K).Id'Img
               & " = " & Self.Data.P (K).Value'Img);
         end loop;
      end if;
   end Dump_Payload;

   ----------
   -- Read --
   ----------

   function Read
     (Sock   : Net.Socket_Type'Class;
      Header : Frame.Object) return Object
   is
      Len  : Length_Type := Header.Header.H.Length;
      Size : Stream_Element_Count :=
               Stream_Element_Count (Len) / (Payload'Size / 8);
      Idx  : Stream_Element_Count := 1;
      Data : Stream_Element_Array (1 .. Stream_Element_Count (Len));
      Vect : Set (1 .. Size);
      for Vect'Address use Data'Address;
      Head : Frame.Object;
   begin
      if Size = 0 then
         return (Header with Size => 0, Data => <>);
      end if;

      Net.Buffered.Read (Sock, Data);

      Head := Header;

      --  Ignore settings with unknown identifier (RFC 7540, 6.5.2.)

      while Idx <= Size loop
         if Vect (Idx).Id'Valid then
            Idx := Idx + 1;

         else
            if Idx < Size then
               Vect (Idx) := Vect (Size);
            end if;

            Size := Size - 1;
            Len  := Len  - (Payload'Size / 8);
         end if;
      end loop;

      if Size = 0 then
         --  All setting is invalid, ignore this settings frame, see Is_Ignore

         return (Header with Size => 1, Data => <>);
      end if;

      Head.Header.H.Length := Len;

      return (Head with Size => Size,
              Data =>
                (Flat => True,
                 S    => new Stream_Element_Array'
                               (Data (1 .. Stream_Element_Count (Len)))));
   end Read;

   -------------
   -- Release --
   -------------

   overriding procedure Release (Self : in out Object) is
   begin
      Utils.Unchecked_Free (Self.Data.S);
   end Release;

   ------------------
   -- Send_Payload --
   ------------------

   overriding procedure Send_Payload
     (Self : Object; Sock : Net.Socket_Type'Class) is
   begin
      Net.Buffered.Write (Sock, Self.Data.S.all);
   end Send_Payload;

   ------------
   -- To_Set --
   ------------

   function To_Set (Config : AWS.Config.Object) return Set is
      Result : Set (Settings_Kind'Pos (Settings_Kind'First)
                    .. Settings_Kind'Pos (Settings_Kind'Last));

      procedure Put (Kind : Settings_Kind; Value : Natural) with Inline;
      --  Put config parameter into result

      ---------
      -- Put --
      ---------

      procedure Put (Kind : Settings_Kind; Value : Natural) is
      begin
         Result (Settings_Kind'Pos (Kind)) := (Kind, Byte_4 (Value));
      end Put;

   begin
      Put (HEADER_TABLE_SIZE,      Config.HTTP2_Header_Table_Size);
      Put (MAX_CONCURRENT_STREAMS, Config.HTTP2_Max_Concurrent_Streams);
      Put (INITIAL_WINDOW_SIZE,    Config.HTTP2_Initial_Window_Size);
      Put (MAX_FRAME_SIZE,         Config.HTTP2_Max_Frame_Size);
      Put (MAX_HEADER_LIST_SIZE,   Config.HTTP2_Max_Header_List_Size);
      Put (ENABLE_PUSH,           (if Config.HTTP2_Enable_Push then 1 else 0));

      --  To be sure that all settings were filled

      pragma Assert
        (for all J in Result'Range => Result (J).Id = Settings_Kind'Val (J));

      return Result;
   end To_Set;

   --------------
   -- Validate --
   --------------

   overriding function Validate
     (Self : Object; Settings : Connection.Object) return Error_Codes is
   begin
      if Self.Header.H.Stream_Id /= 0 then
         return C_Protocol_Error;

      elsif Self.Has_Flag (Ack_Flag) and then Self.Header.H.Length /= 0 then
         return C_Frame_Size_Error;

      elsif Self.Header.H.Length mod 6 /= 0 then
         return C_Frame_Size_Error;

      else
         for K in 1 .. Self.Size loop
            if Self.Data.P (K).Id = MAX_FRAME_SIZE
              and then Self.Data.P (K).Value not in 2**14 .. 2**24 - 1
            then
               return C_Protocol_Error;

            elsif Self.Data.P (K).Id = ENABLE_PUSH
              and then Self.Data.P (K).Value > 1
            then
               return C_Protocol_Error;

            elsif Self.Data.P (K).Id = INITIAL_WINDOW_SIZE
              and then Self.Data.P (K).Value > 2 ** 31 - 1
            then
               return C_Flow_Control_Error;
            end if;
         end loop;

         return HTTP2.Frame.Object (Self).Validate (Settings);
      end if;
   end Validate;

   ------------
   -- Values --
   ------------

   function Values (Self : Object) return Set is
      Result : Set (1 .. Self.Size);
   begin
      for K in 1 .. Self.Size loop
         Result (K) := Self.Data.P (K);
      end loop;

      return Result;
   end Values;

end AWS.HTTP2.Frame.Settings;
