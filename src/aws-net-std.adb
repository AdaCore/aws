------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2001                          --
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

--  Routines here are wrappers around standard sockets when building with no
--  SSL support.

with Ada.Exceptions;

package body AWS.Net is

   procedure Check_SSL (Security : Boolean);
   --  Check that Security is False as there is no support for SSL provied by
   --  this implementation.

   -------------------
   -- Accept_Socket --
   -------------------

   function Accept_Socket (Socket     : in Sockets.Socket_FD;
                           Security   : Boolean)
                          return Sockets.Socket_FD'Class
   is
      New_Socket : Sockets.Socket_FD;
   begin
      Check_SSL (Security);
      Sockets.Accept_Socket (Socket, New_Socket);
      return New_Socket;
   end Accept_Socket;

   ---------------
   -- Check_SSL --
   ---------------

   procedure Check_SSL (Security : Boolean) is
   begin
      if Security then
         Ada.Exceptions.Raise_Exception
           (Program_Error'Identity,
            "You should compile AWS with SSL support.");
      end if;
   end Check_SSL;

   -------------
   -- Connect --
   -------------

   function Connect (Host     : String;
                     Port     : Positive;
                     Security : Boolean)
                    return Sockets.Socket_FD'Class
   is
      Sock : Sockets.Socket_FD;
   begin
      Check_SSL (Security);
      Sockets.Socket (Sock, Sockets.AF_INET, Sockets.SOCK_STREAM);
      Sockets.Connect (Sock, Host, Port);
      return Sock;
   end Connect;

   ----------
   -- Free --
   ----------

   procedure Free (Socket : in out Sockets.Socket_FD'Class) is
   begin
      null;
   end Free;

end AWS.Net;
