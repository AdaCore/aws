------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                      Copyright (C) 2000-2009, AdaCore                    --
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

with AWS.Hotplug;

package AWS.Server.Hotplug is

   --  Messages used to register/unregister hotplug modules

   Register_Message      : constant String := "REGISTER";
   Unregister_Message    : constant String := "UNREGISTER";
   Request_Nonce_Message : constant String := "REQUEST_NONCE";

   --  The Authorization_File below is a file that contains authorizations
   --  for the hotplug modules. Only modules that have an entry into this
   --  file will be able to register to server. Each line on this fille must
   --  have the following format:
   --
   --  <module_name>:<md5_password>:<host>:<port>
   --
   --  module_name  : The name of the module that will register
   --  md5_password : The corresponding password, use aws_password
   --                 tool to generate such password
   --  host         : The host name where requests will be redirected
   --  port         : and the corresponding port

   procedure Activate
     (Web_Server         : HTTP_Access;
      Port               : Positive;
      Authorization_File : String;
      Register_Mode      : AWS.Hotplug.Register_Mode := AWS.Hotplug.Add);
   --  Start hotplug server listening at the specified Port for the Web_Server.
   --  Only client modules listed in the authorization file will be able to
   --  connect to this server. For better securite the host of redictection
   --  must also be specified.

   procedure Shutdown;
   --  Shutdown hotplug server

end AWS.Server.Hotplug;
