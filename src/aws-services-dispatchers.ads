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

with Ada.Unchecked_Deallocation;
with Ada.Finalization;

with AWS.Response;
with AWS.Status;

package AWS.Services.Dispatchers is

   --  Services on the Dispatcher tree are to help building big servers.
   --  Experiences shows that a lot of user's code is to check the value of a
   --  specific URI or request method to call the right callback that will
   --  handle the request. This code is a big "if/elsif/end if" that just hide
   --  the real job. A dispatcher is to replace this code. Currently there is
   --  four of them:
   --
   --  Callback (AWS.Services.Dispatchers.Callback)
   --     to dispatch to a specific callback. This is a wrapper object around
   --     standard callback to integrate this feature into the dispatchers.
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

   type Handler is abstract new Ada.Finalization.Controlled with private;

   procedure Initialize (Dispatcher : in out Handler);
   procedure Adjust     (Dispatcher : in out Handler);
   procedure Finalize   (Dispatcher : in out Handler);

   function Dispatch
     (Dispatcher : in Handler;
      Request    : in Status.Data)
     return Response.Data is abstract;
   --  Call the appropriate inherited dispatcher

   type Handler_Class_Access is access all Handler'Class;

   procedure Free (Dispatcher : in out Handler_Class_Access);
   pragma Inline (Free);

private

   type Handler is abstract new Ada.Finalization.Controlled with record
      Ref_Counter : Natural := 1;
   end record;

end AWS.Services.Dispatchers;
