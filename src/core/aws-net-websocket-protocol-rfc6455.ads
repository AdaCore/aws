------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2012-2014, AdaCore                     --
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

--  This implements the WebSocket protocol as defined in RFC-6455

package AWS.Net.WebSocket.Protocol.RFC6455 is

   type State is new Protocol.State with private;

   overriding procedure Send
     (Protocol : in out State;
      Socket   : Object;
      Data     : Stream_Element_Array);
   --  Encode and send data to the WebSocket.

   overriding procedure Send
     (Protocol : in out State;
      Socket   : Object;
      Data     : Unbounded_String);
   --  Same as above but for an Unbounded_String. This version supports large
   --  messages possibly sent fragmented.

   overriding procedure Close
     (Protocol : in out State;
      Socket   : Object;
      Data     : String;
      Error    : Status_Code);
   --  Send a close frame

   overriding procedure Receive
     (Protocol : in out State;
      Socket   : Object;
      Data     : out Stream_Element_Array;
      Last     : out Stream_Element_Offset);
   --  Receive and decode WebSocket data

   overriding function End_Of_Message (Protocol : State) return Boolean;
   --  Returns True if we have read a whole message

   procedure Send_Header
     (Sock : Net.Socket_Type'Class; Request : AWS.Status.Data);
   --  Send specific header for this protocol

   overriding procedure Add_Connect_Headers
     (Protocol : State;
      Host     : String;
      Headers  : in out AWS.Headers.List);
   overriding function Check_Connect_Response
     (Protocol : State;
      Request  : AWS.Headers.List;
      Response : AWS.Response.Data) return Boolean;
   --  See inherited documentation

private

   --  Protocol specific status

   type Masking_Key_Index is mod 4;
   type Masking_Key is new Stream_Element_Array (0 .. 3);
   for Masking_Key'Size use 32;

   type Opcode is mod 16;
   for Opcode'Size use 4;

   type State is new Protocol.State with record
      Remaining     : Stream_Element_Offset := 0;
      Read          : Stream_Element_Offset := 0;
      Opcd          : Opcode := 0;
      Has_Mask      : Boolean;
      Mask          : Masking_Key;
      Close_Sent    : Boolean := False;
      Last_Fragment : Boolean := True;
   end record;

end AWS.Net.WebSocket.Protocol.RFC6455;
