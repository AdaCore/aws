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

--  $RCSfile$
--  $Revision$ $Date$ $Author$

with AWS.MIME;
with AWS.Dispatchers.Callback;

package body AWS.Services.Dispatchers.Method is

   use AWS.Dispatchers;

   --------------
   -- Dispatch --
   --------------

   function Dispatch
     (Dispatcher : in Handler;
      Request    : in Status.Data)
      return Response.Data
   is
      Method : constant Status.Request_Method := Status.Method (Request);
   begin
      if Dispatcher.Table (Method) = null then
         return Response.Build
           (MIME.Text_HTML,
            "<p>AWS " & Version
            & "<p>No rule found in dispatch for "
            & Status.Request_Method'Image (Method) & " method call.");

      else
         return Dispatch (Dispatcher.Table (Method).all, Request);
      end if;
   end Dispatch;

   --------------
   -- Register --
   --------------

   procedure Register
     (Dispatcher : in out Handler;
      Method     : in     Status.Request_Method;
      Action     : in     AWS.Dispatchers.Handler'Class) is
   begin
      if Dispatcher.Table (Method) /= null then
         Free (Dispatcher.Table (Method));
      end if;

      Dispatcher.Table (Method) := new AWS.Dispatchers.Handler'Class'(Action);
   end Register;

   procedure Register
     (Dispatcher : in out Handler;
      Method     : in     Status.Request_Method;
      Action     : in     Response.Callback) is
   begin
      Register (Dispatcher, Method, AWS.Dispatchers.Callback.Create (Action));
   end Register;

   -------------------------------
   -- Register_Default_Callback --
   -------------------------------

   procedure Register_Default_Callback
     (Dispatcher : in out Handler;
      Action     : in     AWS.Dispatchers.Handler'Class) is
   begin
      if Dispatcher.Action /= null then
         Free (Dispatcher.Action);
      end if;

      Dispatcher.Action := new AWS.Dispatchers.Handler'Class'(Action);
   end Register_Default_Callback;

   ----------------
   -- Unregister --
   ----------------

   procedure Unregister
     (Dispatcher : in out Handler;
      Method     : in     Status.Request_Method) is
   begin
      Free (Dispatcher.Table (Method));
   end Unregister;

end AWS.Services.Dispatchers.Method;
