------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                            Copyright (C) 2004                            --
--                                ACT-Europe                                 --
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

--  Package with constant and type declarations for the Poll operation. This
--  API is used to implement AWS.Net.Sets.

with Interfaces.C;
with System;

package AWS.Net.Sets.Thin is

   use Interfaces;

   Pollin   : constant := 1;
   Pollpri  : constant := 2;
   Pollout  : constant := 4;
   Pollerr  : constant := 8;
   Pollhup  : constant := 16;
   Pollnval : constant := 32;

   subtype FD_Type is C.int;
   subtype Length_Type is C.unsigned_long;
   subtype Timeout_Type is C.int;

   type Events_Type is mod 2 ** C.short'Size;
   for Events_Type'Size use C.short'Size;

   type Pollfd is record
      FD      : FD_Type;
      Events  : Events_Type := 0;
      REvents : Events_Type := 0;
   end record;
   pragma Convention (C, Pollfd);

   function Poll
     (Fds     : System.Address;
      Nfds    : Length_Type;
      Timeout : Timeout_Type)
      return C.Int;
   pragma Import (C, Poll, "poll");
   --  Don't forget to link application with win32/poll.adb under the windows
   --  ??? This must be fixed, poll.o should be part of libaws.a on Windows.

end AWS.Net.Sets.Thin;
