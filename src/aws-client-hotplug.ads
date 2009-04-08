------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2004-2009, AdaCore                     --
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

with AWS.Response;

package AWS.Client.Hotplug is

   --  Below are two routines to register/unregister hotplug modules into
   --  server. Note that such server must be configured to accept hotplug
   --  modules. Password parameter is the clear text paswword, it will be sent
   --  encoded. An authorization entry for module Name with Password (and the
   --  given URL host for registration) must be found in the server's
   --  authorization file. See AWS.Server.Hotplug.Activate.

   function Register
     (Name     : String;
      Password : String;
      Server   : String;
      Regexp   : String;
      URL      : String) return Response.Data;
   --  Register hotplug module Name into Server with address URL to respond to
   --  requests matching Regexp. Server must be a valid URL, http://host:port.
   --  If port is not specified the default HTTP port is used.

   function Unregister
     (Name     : String;
      Password : String;
      Server   : String;
      Regexp   : String) return Response.Data;
   --  Unregister hotplug module Name responding to Regexp requests from
   --  Server. See comment above about Password.

end AWS.Client.Hotplug;
