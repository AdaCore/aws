------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                            Copyright (C) 2000                            --
--                      Dmitriy Anisimov - Pascal Obry                      --
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

--  routines here are wrappers around standard sockets and SSL.

with Sockets;

package AWS.Net is

   function Accept_Socket
     (Socket   : in Sockets.Socket_FD;
      Security :    Boolean)
     return Sockets.Socket_FD'Class;
   --  Accept a connection on a socket. If Security is true an secure socket
   --  will be used.

   function Connect
     (Host     : String;
      Port     : Positive;
      Security : Boolean)
     return Sockets.Socket_FD'Class;
   --  Connect a socket on a given host/port. Raise Connection_Refused if
   --  the connection has not been accepted by the other end. If Security is
   --  true an secure socket will be used.

end AWS.Net;
