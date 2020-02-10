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

--  Parent of all implemented protocols which are for internal use only

with AWS.Headers;
with AWS.Response;
with Interfaces;

package AWS.Net.WebSocket.Protocol is

   type State is abstract tagged null record;
   type State_Class is access all State'Class;

   subtype Status_Code is Interfaces.Unsigned_16 range 0 .. 4_999;

   procedure Send
     (Protocol : in out State;
      Socket   : Object;
      Data     : Stream_Element_Array) is abstract;
   --  Encode and send data to the WebSocket.
   --  From_Client is true for messages sent from client to server.

   procedure Send
     (Protocol : in out State;
      Socket   : Object;
      Data     : Unbounded_String) is abstract;
   --  Same as above but for an Unbounded_String. This version supports large
   --  messages possibly sent fragmented.

   procedure Close
     (Protocol : in out State;
      Socket   : Object;
      Data     : String;
      Error    : Status_Code) is abstract;
   --  Send a close frame to the given socket

   procedure Receive
     (Protocol : in out State;
      Socket   : Object;
      Data     : out Stream_Element_Array;
      Last     : out Stream_Element_Offset) is abstract;
   --  Receive and decode WebSocket data

   function End_Of_Message (Protocol : State) return Boolean is abstract;
   --  Returns True if we have read a whole message

   procedure Add_Connect_Headers
     (Protocol : State;
      Host     : String;
      Headers  : in out AWS.Headers.List) is abstract;
   --  Add all required headers to establish a websocket connection to a server

   function Check_Connect_Response
     (Protocol_Ignored : State;
      Request_Ignored  : AWS.Headers.List;
      Response_Ignored : AWS.Response.Data) return Boolean is (True);
   --  Check whether the server's response matches the headers we sent to
   --  establish a connection.

end AWS.Net.WebSocket.Protocol;
