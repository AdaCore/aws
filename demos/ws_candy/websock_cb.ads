------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                       Copyright (C) 2013, AdaCore                        --
--                                                                          --
--  This is free software;  you can redistribute it  and/or modify it       --
--  under terms of the  GNU General Public License as published  by the     --
--  Free Software  Foundation;  either version 3,  or (at your option) any  --
--  later version.  This software is distributed in the hope  that it will  --
--  be useful, but WITHOUT ANY WARRANTY;  without even the implied warranty --
--  of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU     --
--  General Public License for  more details.                               --
--                                                                          --
--  You should have  received  a copy of the GNU General  Public  License   --
--  distributed  with  this  software;   see  file COPYING3.  If not, go    --
--  to http://www.gnu.org/licenses for a complete copy of the license.      --
------------------------------------------------------------------------------

with Ada.Streams;

with AWS.Net.Log;
with AWS.Response;
with AWS.Status;

with AWS.Net.WebSocket;

package WebSock_CB is

   use Ada.Streams;
   use AWS;

   function HW_CB (Request : Status.Data) return Response.Data;

   procedure W_log
     (Direction : Net.Log.Data_Direction;
      Socket    : Net.Socket_Type'Class;
      Data      : Stream_Element_Array;
      Last      : Stream_Element_Offset);

   --  My WebSocket, just display the messages

   type Object is new AWS.Net.WebSocket.Object with private;

   function Create
     (Socket  : Net.Socket_Access;
      Request : Status.Data) return Net.WebSocket.Object'Class;

   overriding procedure On_Message (Socket : in out Object; Message : String);
   --  Message received from the server

   overriding procedure On_Open (Socket : in out Object; Message : String);
   --  Open event received from the server

   overriding procedure On_Close (Socket : in out Object; Message : String);
   --  Close event received from the server

   overriding procedure On_Error (Socket : in out Object; Message : String);

private

   type Object is new Net.WebSocket.Object with record
      C : Natural := 0;
   end record;

end WebSock_CB;
