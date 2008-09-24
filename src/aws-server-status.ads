------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2003-2008, AdaCore                     --
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

--  This package provides routine to retrieve server's internal status

with Ada.Calendar;

with AWS.Templates;

package AWS.Server.Status is

   function Translations (Server : in HTTP) return Templates.Translate_Set;
   --  Returns a translate table to be used with a template file. This table
   --  contains all internal server's data. This table is used by the server
   --  internal status page for example.

   function Translations (Server : in HTTP) return Templates.Translate_Table;
   pragma Obsolescent ("Use Translate_Set return value instead");
   --  The same as above but obsolete and keept for backward compartibility

   function Start_Time (Server : in HTTP) return Ada.Calendar.Time;
   --  Returns the server's start time

   function Resources_Served (Server : in HTTP) return Natural;
   --  Returns the total number of resources (static file, templates,
   --  in-memory string) served by the server.

   function Socket (Server : in HTTP) return Net.Socket_Type'Class;
   --  Returns the server's socket

   function Port (Server : in HTTP) return Positive;
   --  Returns the server's socket port

   function Current_Connections (Server : in HTTP) return Natural;
   --  Returns the current number of connections

   function Is_Session_Activated (Server : in HTTP) return Boolean;
   --  Returns True if the session feature has been activated

   function Is_Security_Activated (Server : in HTTP) return Boolean;
   --  Returns True if the HTTPS protocol is used

   function Is_Shutdown (Server : in HTTP) return Boolean;
   --  Returns True if server has been stopped (the server could still be in
   --  the shutdown phase).

end AWS.Server.Status;
