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

with AWS.HTTP2.Frame.Window_Update;

package body AWS.HTTP2.Connection is

   -------------------------------
   -- Flow_Control_Window_Valid --
   -------------------------------

   function Flow_Control_Window_Valid
     (Current, Increment : Integer) return Boolean
   is
      Max : constant Natural :=
              Natural (Frame.Window_Update.Size_Increment_Type'Last);
   begin
      return Increment <= 0 or else Current <= Max - Increment;
   end Flow_Control_Window_Valid;

   --------------------------
   -- Handle_Control_Frame --
   --------------------------

   procedure Handle_Control_Frame
     (Self             : in out Object;
      Frame            : HTTP2.Frame.Object'Class;
      Answers          : in out HTTP2.Frame.List.Object;
      Add_Flow_Control : out Integer;
      Error            : out HTTP2.Error_Codes)
   is

      procedure Handle (Frame : HTTP2.Frame.Settings.Object);
      --  Handle settings frame values

      procedure Handle (Frame : HTTP2.Frame.Window_Update.Object);
      --  Handle window update frame value

      package S renames HTTP2.Frame.Settings;

      ------------
      -- Handle --
      ------------

      procedure Handle (Frame : HTTP2.Frame.Settings.Object) is
         use type S.Settings_Kind;

         Initial_Window_Size : constant Integer :=
                                 Self.Initial_Window_Size;
      begin
         Self.Set (Frame.Values);

         --  Update the control flow window size of all streams

         for V of Frame.Values loop
            if V.Id = S.INITIAL_WINDOW_SIZE then
               Add_Flow_Control := Integer (V.Value) - Initial_Window_Size;
            end if;
         end loop;
      end Handle;

      procedure Handle (Frame : HTTP2.Frame.Window_Update.Object) is
         Incr : constant Natural := Natural (Frame.Size_Increment);
      begin
         if Flow_Control_Window_Valid (Self.Flow_Control_Window, Incr) then
            Self.Update_Flow_Control_Window (Incr);
         else
            Error := HTTP2.C_Flow_Control_Error;
            return;
         end if;
      end Handle;

      use AWS.HTTP2.Frame;

   begin
      Error            := HTTP2.C_No_Error;
      Add_Flow_Control := 0;

      if Frame.Kind = K_Ping
        and then not Frame.Has_Flag (HTTP2.Frame.Ack_Flag)
      then
         --  A probing ping frame, respond now with the same
         --  payload (see RFC-7540 / 6.7).

         declare
            R_Ping : HTTP2.Frame.Object'Class := Frame;
         begin
            R_Ping.Set_Flags (HTTP2.Frame.Ack_Flag);
            Answers.Prepend (R_Ping);
         end;

      elsif Frame.Kind = K_Settings
        and then not Frame.Has_Flag (HTTP2.Frame.Ack_Flag)
      then
         Handle (HTTP2.Frame.Settings.Object (Frame));
         Answers.Prepend (HTTP2.Frame.Settings.Ack);

      elsif Frame.Kind = K_Settings
        and then Frame.Has_Flag (HTTP2.Frame.Ack_Flag)
      then
         Answers.Prepend (HTTP2.Frame.Settings.Ack);

      elsif Frame.Kind = K_Window_Update then
         Handle (HTTP2.Frame.Window_Update.Object (Frame));
      end if;
   end Handle_Control_Frame;

   ---------
   -- Set --
   ---------

   procedure Set
     (Self   : in out Object;
      Config : AWS.Config.Object) is
   begin
      Self.Set (S.To_Set (Config));
   end Set;

   procedure Set
     (Self   : in out Object;
      Values : Frame.Settings.Set) is
   begin
      for V of Values loop
         Self.Values (V.Id) := Natural (V.Value);
      end loop;

      Self.Dynamic_Header_Table_Size := Self.Values (S.HEADER_TABLE_SIZE);
   end Set;

   procedure Set
     (Self : in out Object; Socket : Net.Socket_Type'Class)
   is
      F : constant Frame.Object'Class := Frame.Read (Socket, Self);
   begin
      if F in Frame.Settings.Object'Class then
         Self.Set (Frame.Settings.Object (F).Values);
      else
         raise Protocol_Error with "Wrong frame type " & F.Kind'Img;
      end if;
   end Set;

   ---------------------------
   -- Set_Header_Table_Size --
   ---------------------------

   procedure Set_Dynamic_Header_Table_Size
     (Self : in out Object; Size : Natural) is
   begin
      Self.Dynamic_Header_Table_Size := Size;
   end Set_Dynamic_Header_Table_Size;

   -----------------------------
   -- Set_Initial_Window_Size --
   -----------------------------

   procedure Set_Initial_Window_Size
     (Self  : in out Object;
      Value : Natural)
   is
      Prev : constant Natural := Self.Values (S.INITIAL_WINDOW_SIZE);
   begin
      --  Self.Values (S.INITIAL_WINDOW_SIZE) :=
      --    Self.Values (S.INITIAL_WINDOW_SIZE) + Increment;
      Self.Values (S.INITIAL_WINDOW_SIZE) := Value;

      Self.Flow_Send_Window := Self.Flow_Send_Window + (Value - Prev);
   end Set_Initial_Window_Size;

   --------------------------------
   -- Update_Flow_Control_Window --
   --------------------------------

   procedure Update_Flow_Control_Window
     (Self      : in out Object;
      Increment : Integer) is
   begin
      Self.Flow_Send_Window := Self.Flow_Send_Window + Increment;
   end Update_Flow_Control_Window;

   --------------------------------
   -- Update_Flow_Receive_Window --
   --------------------------------

   procedure Update_Flow_Receive_Window
     (Self      : in out Object;
      Increment : Integer) is
   begin
      Self.Flow_Receive_Window := Self.Flow_Receive_Window + Increment;
   end Update_Flow_Receive_Window;

end AWS.HTTP2.Connection;
