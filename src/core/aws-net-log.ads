------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2004-2013, AdaCore                     --
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

pragma Ada_2012;

--  This package handles the Net logging facility for AWS.
--
--  AWS calls the Write procedure which in turn calls the callback routine
--  provided by the user when starting the logging. This feature can help
--  greatly to debug an application.
--
--  This package is thread safe. There will never be two simultaneous calls
--  to the callback routine.

package AWS.Net.Log is

   type Data_Direction is (Sent, Received);
   --  The direction of the data, sent or received to/from the socket

   type Event_Type is (Connect, Accept_Socket, Shutdown);

   type Write_Callback is access procedure
     (Direction : Data_Direction;
      Socket    : Socket_Type'Class;
      Data      : Stream_Element_Array;
      Last      : Stream_Element_Offset);
   --  The callback procedure which is called for each incoming/outgoing data

   type Event_Callback is access procedure
     (Action : Event_Type; Socket : Socket_Type'Class);
   --  The callback procedure which is called for every socket creation,
   --  connect and accept.

   type Error_Callback is access procedure
     (Socket : Socket_Type'Class; Message : String);
   --  The callback procedure which is called for every socket error

   procedure Start
     (Write : Write_Callback;
      Event : Event_Callback := null;
      Error : Error_Callback := null);
   --  Activate the logging

   function Is_Active return Boolean with Inline;
   --  Returns True if Log is activated and False otherwise

   function Is_Write_Active return Boolean with Inline;
   --  Returns True if Write Log is activated and False otherwise

   function Is_Event_Active return Boolean with Inline;
   --  Returns True if Event Log is activated and False otherwise

   procedure Write
     (Direction : Data_Direction;
      Socket    : Socket_Type'Class;
      Data      : Stream_Element_Array;
      Last      : Stream_Element_Offset);
   --  Write sent/received data indirectly through the callback routine,
   --  if activated (i.e. Start routine above has been called). Otherwise this
   --  call does nothing.

   procedure Event (Action : Event_Type; Socket : Socket_Type'Class);
   --  Call Event callback if activated (i.e. Start routine above has been
   --  called). Otherwise this call does nothing.

   procedure Error (Socket : Socket_Type'Class; Message : String);
   --  Call Error callback if activated (i.e. Start routine above has been
   --  called). Otherwise this call does nothing.

   procedure Stop;
   --  Stop logging activity

end AWS.Net.Log;
