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

--  Support connection settings from settings & windows update frames

with AWS.HTTP2.Frame.Settings;
with AWS.HTTP2.Frame.Window_Update;

package AWS.HTTP2.Connection is

   type Object is tagged private;

   procedure Set
     (Self   : in out Object;
      Values : Frame.Settings.Set);
   --  Record all settings from Values extracted from a settings frames

   procedure Set
     (Self                  : in out Object;
      Window_Size_Increment : Frame.Window_Update.Size_Increment_Type);
   --  Record the setting from window update frame

   function Header_Table_Size (Self : Object) return Natural;

   function Enable_Push (Self : Object) return Boolean;
   --  Wether the push is supported

   function Max_Concurrent_Streams (Self : Object) return Natural;
   --  The maximum number of stream opened simultaneously

   function Initial_Window_Size (Self : Object) return Natural;
   --  The window size

   function Max_Frame_Size (Self : Object) return Natural;
   --  The maximum number of bytes for a frame payload

   function Max_Header_List_Size (Self : Object) return Natural;
   --  The size in bytes of the uncompressed headers list

   function Window_Size_Increment
     (Self : Object) return Frame.Window_Update.Size_Increment_Type;
   --   Size increment to add to the current initial window size

private

   type Settings_Set is array (Frame.Settings.Settings_Kind) of Natural;

   package S renames Frame.Settings;

   Default_Values : constant Settings_Set :=
                      (S.HEADER_TABLE_SIZE      =>     4_096,
                       S.ENABLE_PUSH            =>         0,
                       S.MAX_CONCURRENT_STREAMS =>       250,
                       S.INITIAL_WINDOW_SIZE    =>    65_535,
                       S.MAX_FRAME_SIZE         =>    16_384,
                       S.MAX_HEADER_LIST_SIZE   => 1_048_576);

   type Object is tagged record
      Values                : Settings_Set := Default_Values;
      Window_Size_Increment : Frame.Window_Update.Size_Increment_Type := 0;
   end record;

   function Header_Table_Size (Self : Object) return Natural is
     (Self.Values (Frame.Settings.HEADER_TABLE_SIZE));

   function Enable_Push (Self : Object) return Boolean is
     (if Self.Values (Frame.Settings.ENABLE_PUSH) = 0 then False else True);

   function Max_Concurrent_Streams (Self : Object) return Natural is
     (Self.Values (Frame.Settings.MAX_CONCURRENT_STREAMS));

   function Initial_Window_Size (Self : Object) return Natural is
     (Self.Values (Frame.Settings.INITIAL_WINDOW_SIZE));

   function Max_Frame_Size (Self : Object) return Natural is
     (Self.Values (Frame.Settings.MAX_FRAME_SIZE));

   function Max_Header_List_Size (Self : Object) return Natural is
     (Self.Values (Frame.Settings.MAX_HEADER_LIST_SIZE));

   function Window_Size_Increment
     (Self : Object)
      return Frame.Window_Update.Size_Increment_Type
   is (Self.Window_Size_Increment);

end AWS.HTTP2.Connection;
