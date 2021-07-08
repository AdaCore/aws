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
      Max  : constant Natural :=
               Natural (HTTP2.Frame.Window_Update.Size_Increment_Type'Last);
   begin
      return Current < 0 or else Increment <= Max - Current;
   end Flow_Control_Window_Valid;

   ---------
   -- Set --
   ---------

   procedure  Set
     (Self   : in out Object;
      Config : AWS.Config.Object) is
   begin
      Self.Values (S.HEADER_TABLE_SIZE) :=
        Config.HTTP2_Header_Table_Size;

      Self.Values (S.ENABLE_PUSH) :=
        (if Config.HTTP2_Enable_Push then 1 else 0);

      Self.Values (S.MAX_CONCURRENT_STREAMS) :=
        Config.HTTP2_Max_Concurrent_Streams;

      Self.Values (S.INITIAL_WINDOW_SIZE) :=
        Config.HTTP2_Initial_Window_Size;

      Self.Values (S.MAX_FRAME_SIZE) :=
        Config.HTTP2_Max_Frame_Size;

      Self.Values (S.MAX_HEADER_LIST_SIZE) :=
        Config.HTTP2_Max_Header_List_Size;
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
     (Self : in out Object; Socket : Net.Socket_Type'Class) is
   begin
      Self.Set (Frame.Settings.Object (Frame.Read (Socket, Self)).Values);
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

      Self.Flow_Control_Window :=
        Self.Flow_Control_Window + (Value - Prev);
   end Set_Initial_Window_Size;

   --------------------------------
   -- Update_Flow_Control_Window --
   --------------------------------

   procedure Update_Flow_Control_Window
     (Self      : in out Object;
      Increment : Integer) is
   begin
      Self.Flow_Control_Window := Self.Flow_Control_Window + Increment;
   end Update_Flow_Control_Window;

end AWS.HTTP2.Connection;
