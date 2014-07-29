------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2003-2014, AdaCore                     --
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

--  This package provides routine to retrieve server's internal status

with Ada.Calendar;

with AWS.Net.Acceptors;
with AWS.Templates;

package AWS.Server.Status is

   function Translations (Server : HTTP) return Templates.Translate_Set;
   --  Returns a translate table to be used with a template file. This table
   --  contains all internal server's data. This table is used by the server
   --  internal status page for example.

   function Translations (Server : HTTP) return Templates.Translate_Table;
   pragma Obsolescent ("Use Translate_Set return value instead");
   --  The same as above but obsolete and keept for backward compartibility

   function Start_Time (Server : HTTP) return Ada.Calendar.Time;
   --  Returns the server's start time

   function Resources_Served (Server : HTTP) return Natural;
   --  Returns the total number of resources (static file, templates,
   --  in-memory string) served by the server.

   function Socket (Server : HTTP) return Net.Socket_Type'Class;
   --  Returns the main server's socket

   function Sockets (Server : HTTP) return Net.Acceptors.Socket_List;
   --  Returns all server's sockets

   function Port (Server : HTTP) return Positive;
   --  Returns the server's socket port

   function Host (Server : HTTP) return String;
   --  Returns the server's socket host

   function Is_Any_Address (Server : HTTP) return Boolean;
   --  Returns True if the server accepts connections on any of the host's
   --  network addresses.

   function Is_IPv6 (Server : HTTP) return Boolean;
   --  Returns True if Server is using IPv6

   function Local_URL (Server : HTTP) return String;
   --  Local URL of the server

   function Current_Connections (Server : HTTP) return Natural;
   --  Returns the current number of connections

   function Is_Session_Activated (Server : HTTP) return Boolean;
   --  Returns True if the session feature has been activated

   function Is_Security_Activated (Server : HTTP) return Boolean;
   --  Returns True if the HTTPS protocol is used

   function Is_Shutdown (Server : HTTP) return Boolean;
   --  Returns True if server has been stopped (the server could still be in
   --  the shutdown phase).

end AWS.Server.Status;
