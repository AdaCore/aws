------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2003-2009, AdaCore                     --
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

--  Dispatch to the user's callback, if a 404 (Page Not Found) is returned it
--  checks if the resource is a transient page and return it if found.

with AWS.Dispatchers;
with AWS.Response;
with AWS.Status;

package AWS.Services.Dispatchers.Transient_Pages is

   type Handler is new AWS.Dispatchers.Handler with private;

   procedure Register
     (Dispatcher : in out Handler;
      Action     : AWS.Dispatchers.Handler'Class);
   --  Register callback to use for a specific request method

   procedure Register
     (Dispatcher : in out Handler;
      Action     : Response.Callback);
   --  Idem as above but takes a callback procedure as parameter

private

   overriding function Dispatch
     (Dispatcher : Handler;
      Request    : Status.Data) return Response.Data;
   --  Returns an error message (code 404) if no transient page were found

   overriding function Clone
     (Dispatcher : Handler) return Handler;
   --  Returns a deep copy of the dispatcher

   type Handler is new AWS.Dispatchers.Handler with record
      Action : AWS.Dispatchers.Handler_Class_Access;
   end record;

end AWS.Services.Dispatchers.Transient_Pages;
