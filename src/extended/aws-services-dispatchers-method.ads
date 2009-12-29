------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2009, AdaCore                     --
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

--  Dispatch a specific request to a callback depending on the request method

with AWS.Dispatchers;
with AWS.Response;
with AWS.Status;

package AWS.Services.Dispatchers.Method is

   type Handler is new AWS.Dispatchers.Handler with private;

   procedure Register
     (Dispatcher : in out Handler;
      Method     : Status.Request_Method;
      Action     : AWS.Dispatchers.Handler'Class);
   --  Register callback to use for a specific request method

   procedure Register
     (Dispatcher : in out Handler;
      Method     : Status.Request_Method;
      Action     : Response.Callback);
   --  Idem as above but take a callback procedure as parameter

   procedure Unregister
     (Dispatcher : in out Handler;
      Method     : Status.Request_Method);
   --  Removes Method from the list of request method to handle

   procedure Register_Default_Callback
     (Dispatcher : in out Handler;
      Action     : AWS.Dispatchers.Handler'Class);
   --  Register the default callback. This will be used if no request method
   --  have been activated.

private

   overriding procedure Initialize (Dispatcher : in out Handler);
   overriding procedure Finalize   (Dispatcher : in out Handler);

   overriding function Dispatch
     (Dispatcher : Handler;
      Request    : Status.Data) return Response.Data;
   --  Dispatch to the corresponding method callback, if no such callback
   --  registered it dispatches to the default callback. If there is no default
   --  callback it returns an error message (code 404).

   overriding function Clone (Dispatcher : Handler) return Handler;
   --  Returns a deep copy of the dispatcher

   type Method_Table is
     array (Status.Request_Method) of AWS.Dispatchers.Handler_Class_Access;

   type Handler is new AWS.Dispatchers.Handler with record
      Action : AWS.Dispatchers.Handler_Class_Access;
      Table  : Method_Table;
   end record;

end AWS.Services.Dispatchers.Method;
