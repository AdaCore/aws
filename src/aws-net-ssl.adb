------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                            Copyright (C) 2000                            --
--                     Dmitriy Anisimkov - Pascal Obry                      --
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

with SSL;

package body AWS.Net is

   SSL_Initialized : Boolean := False;

   procedure Init;
   --  Initializa OpenSSL library

   ----------
   -- Init --
   ----------

   procedure Init is
   begin
      if not SSL_Initialized then
         SSL.Init (SSL.SSLv23);
         SSL.Set_Certificate ("cert.pem");
         SSL.Set_Quiet_Shutdown;
         SSL.Set_Sess_Cache_Size (16);
         SSL_Initialized := True;
      end if;
   end Init;

   -------------------
   -- Accept_Socket --
   -------------------

   function Accept_Socket (Socket   : in Sockets.Socket_FD;
                           Security : Boolean)
                          return Sockets.Socket_FD'Class is
   begin
      if Security then
         declare
            New_Socket : SSL.Handle;
         begin
            SSL.Accept_Socket (Socket, New_Socket);
            return New_Socket;
         end;
      else
         declare
            New_Socket : Sockets.Socket_FD;
         begin
            Sockets.Accept_Socket (Socket, New_Socket);
            return New_Socket;
         end;
      end if;
   end Accept_Socket;

   -------------
   -- Connect --
   -------------

   function Connect (Host     : String;
                     Port     : Positive;
                     Security : Boolean) return Sockets.Socket_FD'Class is
   begin
      if Security then
         declare
            Sock : SSL.Handle;
         begin
            SSL.Socket (Sock, Sockets.AF_INET, Sockets.SOCK_STREAM);
            SSL.Connect (Sock, Host, Port);
            return Sock;
         end;
      else
         declare
            Sock : Sockets.Socket_FD;
         begin
            Sockets.Socket (Sock, Sockets.AF_INET, Sockets.SOCK_STREAM);
            Sockets.Connect (Sock, Host, Port);
            return Sock;
         end;
      end if;
   end Connect;

begin
   Init;
end AWS.Net;
