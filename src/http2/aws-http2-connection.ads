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

with AWS.Config;
with AWS.Default;

with AWS.HTTP2.Frame.List;
with AWS.HTTP2.Frame.Settings;
with AWS.Net;

package AWS.HTTP2.Connection is

   type Object is tagged private;

   procedure Set
     (Self   : in out Object;
      Config : AWS.Config.Object);
   --  Set values using the configuration variables

   procedure Set
     (Self   : in out Object;
      Values : Frame.Settings.Set);
   --  Record all settings from Values extracted from a settings frames

   procedure Set
     (Self   : in out Object;
      Socket : Net.Socket_Type'Class);
   --  Record all settings from a frame read from Socket

   procedure Set_Initial_Window_Size
     (Self  : in out Object;
      Value : Natural);

   procedure Update_Flow_Control_Window
     (Self      : in out Object;
      Increment : Integer);
   --  Record the setting from window update frame

   procedure Update_Flow_Receive_Window
     (Self      : in out Object;
      Increment : Integer);
   --  Increment Flow_Receive_Window

   function Header_Table_Size (Self : Object) return Natural;
   --  The current header table size. Set above as part of initial settings
   --  handshake.

   function Dynamic_Header_Table_Size (Self : Object) return Natural;
   --  The dynmaic table size, can be set while parsing HPACK with Dynamic
   --  Table Size Update block. Default to header table size.

   procedure Set_Dynamic_Header_Table_Size
     (Self : in out Object; Size : Natural);
   --  Set the dynmaic table size, can be set while parsing HPACK with Dynamic
   --  Table Size Update block. Default to header table size.

   function Enable_Push (Self : Object) return Boolean;
   --  Whether the push is supported

   function Max_Concurrent_Streams (Self : Object) return Natural;
   --  The maximum number of stream opened simultaneously

   function Initial_Window_Size (Self : Object) return Integer;
   --  The window size

   function Max_Frame_Size (Self : Object) return Frame.Length_Type;
   --  The maximum number of bytes for a frame payload

   function Max_Header_List_Size (Self : Object) return Natural;
   --  The size in bytes of the uncompressed headers list

   function Flow_Control_Window_Valid
     (Current, Increment : Integer) return Boolean with Inline;
   --  Returns True if an update of the control flow window by the size of
   --  Increment is still in range (not above max).

   function Flow_Control_Window (Self : Object) return Integer;

   function Flow_Receive_Window (Self : Object) return Integer;

   procedure Handle_Control_Frame
     (Self             : in out Object;
      Frame            : HTTP2.Frame.Object'Class;
      Answers          : in out HTTP2.Frame.List.Object;
      Add_Flow_Control : out Integer;
      Error            : out HTTP2.Error_Codes)
     with Pre => Frame.Stream_Id = 0;
   --  Handle a control frame (Frame id = 0)

private

   type Settings_Set is array (Frame.Settings.Settings_Kind) of Integer;

   package S renames Frame.Settings;

   Default_Values : constant Settings_Set :=
                      (S.HEADER_TABLE_SIZE      =>
                         Default.HTTP2_Header_Table_Size,
                       S.ENABLE_PUSH            =>
                         (if Default.HTTP2_Enable_Push then 1 else 0),
                       S.MAX_CONCURRENT_STREAMS =>
                         Default.HTTP2_Max_Concurrent_Streams,
                       S.INITIAL_WINDOW_SIZE    =>
                         Default.HTTP2_Initial_Window_Size,
                       S.MAX_FRAME_SIZE         =>
                         Default.HTTP2_Max_Frame_Size,
                       S.MAX_HEADER_LIST_SIZE   =>
                         Default.HTTP2_Max_Header_List_Size);

   type Object is tagged record
      Values                    : Settings_Set := Default_Values;
      Dynamic_Header_Table_Size : Natural :=
                                    Default_Values (S.HEADER_TABLE_SIZE);
      Flow_Send_Window          : Integer :=
                                    Default_Values (S.INITIAL_WINDOW_SIZE);
      Flow_Receive_Window       : Integer :=
                                    Default_Values (S.INITIAL_WINDOW_SIZE);
   end record;

   function Header_Table_Size (Self : Object) return Natural is
     (Self.Values (S.HEADER_TABLE_SIZE));

   function Enable_Push (Self : Object) return Boolean is
     (if Self.Values (S.ENABLE_PUSH) = 0 then False else True);

   function Max_Concurrent_Streams (Self : Object) return Natural is
     (Self.Values (S.MAX_CONCURRENT_STREAMS));

   function Initial_Window_Size (Self : Object) return Integer is
     (Self.Values (S.INITIAL_WINDOW_SIZE));

   function Max_Frame_Size (Self : Object) return Frame.Length_Type is
     (Frame.Length_Type (Self.Values (S.MAX_FRAME_SIZE)));

   function Max_Header_List_Size (Self : Object) return Natural is
     (Self.Values (S.MAX_HEADER_LIST_SIZE));

   function Flow_Control_Window (Self : Object) return Integer is
     (Self.Flow_Send_Window);

   function Flow_Receive_Window (Self : Object) return Integer is
     (Self.Flow_Receive_Window);

   function Dynamic_Header_Table_Size (Self : Object) return Natural is
     (Self.Dynamic_Header_Table_Size);

end AWS.HTTP2.Connection;
