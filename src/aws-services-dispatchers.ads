------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2005                          --
--                                 AdaCore                                  --
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

package AWS.Services.Dispatchers is

   pragma Pure;

   --  Services on the Dispatcher tree are to help building big servers.
   --  Experiences shows that a lot of user's code is to check the value of a
   --  specific URI or request method to call the right callback that will
   --  handle the request. This code is a big "if/elsif/end if" that just hide
   --  the real job. A dispatcher is to replace this code. Currently there is
   --  five of them:
   --
   --  URI (AWS.Services.Dispatchers.URI)
   --     to dispatch to a callback depending of the ressource name.
   --
   --  Method (AWS.Services.Dispatchers.Method)
   --     to dispatch to a callback depending of the request method.
   --
   --  Virtual_Host (AWS.Services.Dispatchers.Virtual_Host)
   --     to dispatch to a callback depending on the host name. This is known
   --     as virtual hosting and permit to have multiple servers on the same
   --     machine using the same port.
   --
   --  Transient_Pages (AWS.Services.Dispatchers.Transient_Pages)
   --     to handle transient pages, if the default user's callback returns
   --     404 this dispatcher checks if the requested resource is a transient
   --     page.
   --
   --  Timer (AWS.Services.Dispatchers.Timer)
   --     to dispatch to a specific callback depending on the current time.
   --
   --  Linker (AWS.Services.Dispatchers.Linker)
   --     to link two dispatchers together, if the first one retruns 404 tries
   --     the second one.

end AWS.Services.Dispatchers;
