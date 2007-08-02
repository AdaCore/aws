------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2007                          --
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

--  This package provides a service to build Callbacks which can support
--  user's data. It is possible to build a new dispatcher by inheriting the
--  handler type and to provides the Dispatch routine.

with Ada.Finalization;

with AWS.Response;
with AWS.Status;
with AWS.Utils;

package AWS.Dispatchers is

   type Handler is abstract new Ada.Finalization.Controlled
     and AWS.Utils.Clonable with private;

   overriding procedure Initialize (Dispatcher : in out Handler);
   overriding procedure Adjust     (Dispatcher : in out Handler);
   overriding procedure Finalize   (Dispatcher : in out Handler);
   --  Initialize/Adjust/Finalize is doing the reference counting, children
   --  should just call these routines if possible. It is possible to know if
   --  no more object are referenced by calling Ref_Counter below.

   function Dispatch
     (Dispatcher : in Handler;
      Request    : in Status.Data) return Response.Data is abstract;
   --  Call the appropriate inherited dispatcher

   function Ref_Counter (Dispatcher : in Handler) return Natural;
   --  Returns the reference counter for Handler. If 0 is returned then this
   --  object is not referenced anymore, it is safe to deallocate ressources.

   type Handler_Class_Access is access all Handler'Class;

   procedure Free (Dispatcher : in out Handler_Class_Access);
   pragma Inline (Free);
   --  Release memory associated with the dispatcher

private

   type Handler is abstract new Ada.Finalization.Controlled
     and AWS.Utils.Clonable
   with record
      Ref_Counter : Utils.Counter_Access;
   end record;

end AWS.Dispatchers;
