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
              Stream_Element_Count (Settings'Length * Payload'Size / 8);
   begin
      return O : Object do
         O.Header.H.Stream_Id := 0;
         O.Header.H.Length    := Length_Type (Len);
         O.Header.H.Kind      := K_Settings;
         O.Header.H.R         := 0;
         O.Header.H.Flags     := (if Ack then Ack_Flag else 0);

         O.Size := Stream_Element_Count (Len / (Payload'Size / 8));

         if Settings'Length > 0 then
            O.Data.S := new Stream_Element_Array
                          (1 .. Stream_Element_Offset (Len));
            O.Data.P (1 .. Settings'Length) := Settings;
         end if;
      end return;
   end Create;

   function Create (Payload : Stream_Element_Array) return Object is
      Len : constant Stream_Element_Count :=
              Stream_Element_Count (Payload'Length);
   begin
      return O : Object do
         O.Header.H.Stream_Id := 0;
         O.Header.H.Length    := Length_Type (Len);
         O.Header.H.Kind      := K_Settings;
         O.Header.H.R         := 0;
         O.Header.H.Flags     := 0;

         O.Size := Stream_Element_Count (Len / (Payload'Size / 8));

         O.Data.S := new Stream_Element_Array'(Payload);
      end return;
   end Create;

   ------------------
   -- Dump_Payload --
   ------------------

   overriding procedure Dump_Payload (Self : Object) is
   begin
      for K in 1 .. Self.Size loop
         Text_IO.Put_Line ("   S: " & Self.Data.P (K).Id'Img
                           & " = " & Self.Data.P (K).Value'Img);
      end loop;
   end Dump_Payload;

   ----------
   -- Read --
   ----------

   function Read
     (Sock   : Net.Socket_Type'Class;
      Header : Frame.Object) return Object
   is
      Len  : constant Stream_Element_Count :=
               Stream_Element_Count (Header.Header.H.Length);
      Size : constant Stream_Element_Count :=
               Len / (Payload'Size / 8);
   begin
      return O : Object := (Header with Size => Size, Data => <>) do
         if Len > 0 then
            O.Data.S := new Stream_Element_Array (1 .. Len);
            Net.Buffered.Read (Sock, O.Data.S.all);
         end if;
      end return;
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

   --------------
   -- Validate --
   --------------

   overriding function Validate (Self : Object) return Error_Codes is
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

         return C_No_Error;
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
