------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2001                          --
--                               ACT-Europe                                 --
--                                                                          --
--  Authors: Dmitriy Anisimov - Pascal Obry                                 --
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

with AWS.Response;

generic

   type T (<>) is limited private;  --  data type received by this server
   type T_Access is access T;

   with function Callback
     (Server     : in String;
      Name       : in String;
      Context    : in T_Access;
      Parameters : in Parameter_Set := Null_Parameter_Set)
     return Response.Data;

package AWS.Communication.Server is

   --  Each instantiation of this package will create an HTTP server waiting
   --  for incoming request at the Port specified by the Start formal
   --  parameter. This communication server must be started with the Start
   --  procedure and can be stopped with the procedure Shutdown below.

   procedure Start (Port : in Positive; Context : in T_Access);
   --  Start communication HTTP server listening at the given port.

   procedure Shutdown;
   --  Shutdown the communication HTTP server.

end AWS.Communication.Server;
