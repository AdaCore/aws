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

with AWS.Response;

package AWS.Services.Virtual_Host is

   procedure Register
     (Virtual_Hostname : in String;
      Hostname         : in String);
   --  Register Virtual_Hostname to be a redirection to the specified
   --  hostname.

   procedure Register
     (Virtual_Hostname : in String;
      Callback         : in Response.Callback);
   --  Register Virtual_Hostname to use the specified callback.

   procedure Unregister (Virtual_Hostname : in String);
   --  Removes Virtual_Hostname from the list of virtual hostnames to handle.

   procedure Register_Default_Callback (Callback : in Response.Callback);
   --  Register the default callback. This will be used if no Virtual_Hostname
   --  match the request.

   function Callback return Response.Callback;
   --  Returns the callback to use the virtual hostname facilities. This must
   --  be the callback passed to the AWS.Server.Start procedure. This
   --  procedure will redirect to one of the virtual host registered. If none
   --  of the virtual host match then the default callback will be used. If no
   --  default callback is registered, an error message will be issued.

end AWS.Services.Virtual_Host;
