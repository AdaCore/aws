------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2001                          --
--                     Dmitriy Anisimkov & Pascal Obry                      --
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

with AWS.Communication;
with AWS.Communication.Server;
with AWS.Messages;

package body AWS.Server.Hotplug is

   function Message
     (Server     : in String;
      Name       : in String;
      Web_Server : in HTTP_Access;
      Parameters : in Communication.Parameter_Set
        := Communication.Null_Parameter_Set)
     return Response.Data;
   --  Handle incoming message to register/unregister a module.

   package Hotplug_Server is
      new Communication.Server (HTTP, HTTP_Access, Message);

   ----------------------
   -- Activate_Hotplug --
   ----------------------

   procedure Activate (Web_Server : in HTTP_Access;
                       Port       : in Positive) is
   begin
      Hotplug_Server.Start (Port, Web_Server);
   end Activate;

   -------------
   -- Message --
   -------------

   function Message
     (Server     : in String;
      Name       : in String;
      Web_Server : in HTTP_Access;
      Parameters : in Communication.Parameter_Set
        := Communication.Null_Parameter_Set)
     return Response.Data is
   begin
      --  There is two kind of message REGISTER and UNREGISTER. The format
      --  are (parameters are between <>):
      --
      --  REGISTER <regexp> <URL>
      --  UNREGISTER <regexp>

      if Name = Register_Message then
         AWS.Hotplug.Register (Web_Server.Filters,
                               To_String (Parameters (1)),
                               To_String (Parameters (2)));

      elsif Name = Unregister_Message then
         AWS.Hotplug.Unregister (Web_Server.Filters,
                                 To_String (Parameters (1)));

      end if;

      return Response.Acknowledge (Messages.S200);
   end Message;

end AWS.Server.Hotplug;
