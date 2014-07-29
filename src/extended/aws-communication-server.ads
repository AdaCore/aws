------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2014, AdaCore                     --
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

with AWS.Response;

generic

   type T (<>) is limited private;  --  Data type received by this server
   type T_Access is access T;

   with function Callback
     (Server     : String; -- Host name
      Name       : String; -- Message name
      Context    : not null access T;
      Parameters : Parameter_Set := Null_Parameter_Set)
      return Response.Data;

package AWS.Communication.Server is

   --  Each instantiation of this package will create an HTTP server waiting
   --  for incoming requests at the Port specified in the Start formal
   --  parameter. This communication server must be started with the Start
   --  procedure and can be stopped with the procedure Shutdown below.

   procedure Start (Port : Positive; Context : T_Access; Host : String := "");
   --  Start communication HTTP server listening at the given port

   procedure Shutdown;
   --  Shutdown the communication HTTP server

end AWS.Communication.Server;
