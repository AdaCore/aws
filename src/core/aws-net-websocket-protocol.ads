------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2012-2013, AdaCore                     --
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

package AWS.Net.WebSocket.Protocol is

   type State is abstract tagged null record;
   type State_Class is access all State'Class;

   procedure Send
     (Protocol : in out State;
      Socket   : Object;
      Data     : Stream_Element_Array) is abstract;
   --  Encode and send data to the WebSocket

   procedure Send
     (Protocol : in out State;
      Socket   : Object;
      Data     : Unbounded_String) is abstract;
   --  Same as above but for an Unbounded_String. This version supports large
   --  messages possibly sent fragmented.

   procedure Receive
     (Protocol : in out State;
      Socket   : Object;
      Data     : out Stream_Element_Array;
      Last     : out Stream_Element_Offset) is abstract;
   --  Receive and decode WebSocket data

   function End_Of_Message (Protocol : State) return Boolean is abstract;
   --  Returns True if we have read a whole message

end AWS.Net.WebSocket.Protocol;
