------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                            Copyright (C) 2004                            --
--                                ACT-Europe                                --
--                                                                          --
--  Authors: Dmitriy Anisimkov - Pascal Obry                                --
--                                                                          --
--  This library is free software; you can redistribute it and/or modify    --
--  it under the terms of the GNU General Public License as published by    --
--  the Free Software Foundation; either version 2 of the License, or (at   --
--  your option) any later version.                                         --
--                                                                          --
--  This library is distributed in the hope that it will be useful, but     --
--  WITHOUT ANY WARRANTY; without even the implied warranty of              --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU       --
--  General Public License for more details.                                --
--                                                                          --
--  You should have received a copy of the GNU General Public License       --
--  along with this library; if not, write to the Free Software Foundation, --
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.          --
--                                                                          --
--  As a special exception, if other files instantiate generics from this   --
--  unit, or you link this unit with other files to produce an executable,  --
--  this  unit  does not  by itself cause  the resulting executable to be   --
--  covered by the GNU General Public License. This exception does not      --
--  however invalidate any other reasons why the executable file  might be  --
--  covered by the  GNU Public License.                                     --
------------------------------------------------------------------------------

--  $Id$

--  This package handles the Net logging facility for AWS.
--
--  AWS calls the Write procedure which in turn calls the callback routine
--  provided by the user when starting the logging. This feature can help
--  greatly to debug an application.
--
--  This package is thread safe. There will never be two simultaneous calls
--  to the callback routine.

with Ada.Streams;

package AWS.Net.Log is

   type Data_Direction is (Sent, Received);
   --  The direction of the data, sent or received to/from the socket

   type Write_Callback is access procedure
     (Direction : in Data_Direction;
      FD        : in Integer;
      Data      : in Ada.Streams.Stream_Element_Array;
      Last      : in Ada.Streams.Stream_Element_Offset);
   --  The callback procedure which is called for each incoming/outgoing data

   procedure Start (Write : in Write_Callback);
   --  Activate the logging

   function Is_Active return Boolean;
   pragma Inline (Is_Active);
   --  Returns True if Log is activated and False otherwise

   procedure Write
     (Direction : in Data_Direction;
      FD        : in Integer;
      Data      : in Ada.Streams.Stream_Element_Array;
      Last      : in Ada.Streams.Stream_Element_Offset);
   --  Write sent/received data indirectly through the callback routine,
   --  if activated (i.e. Start routine above has been called). Otherwise this
   --  call does nothing.

   procedure Stop;
   --  Stop logging activity

end AWS.Net.Log;
