------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2014, AdaCore                     --
--                                                                          --
--  This library is free software;  you can redistribute it and/or modify   --
--  it under terms of the  GNU General Public License  as published by the  --
--  Free Software  Foundation;  either version 3,  or (at your  option) any --
--  later version. This library is distributed in the hope that it will be  --
--  useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                    --
--                                                                          --
--  As a special exception under Section 7 of GPL version 3, you are        --
--  granted additional permissions described in the GCC Runtime Library     --
--  Exception, version 3.1, as published by the Free Software Foundation.   --
--                                                                          --
--  You should have received a copy of the GNU General Public License and   --
--  a copy of the GCC Runtime Library Exception along with this program;    --
--  see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see   --
--  <http://www.gnu.org/licenses/>.                                         --
--                                                                          --
--  As a special exception, if other files instantiate generics from this   --
--  unit, or you link this unit with other files to produce an executable,  --
--  this  unit  does not  by itself cause  the resulting executable to be   --
--  covered by the GNU General Public License. This exception does not      --
--  however invalidate any other reasons why the executable file  might be  --
--  covered by the  GNU Public License.                                     --
------------------------------------------------------------------------------

--  Dispatch a specific request to a callback depending on the URI

with AWS.Dispatchers;
with AWS.Response;
with AWS.Status;
with AWS.Utils;

private with Ada.Containers.Vectors;
private with Ada.Strings.Unbounded;

package AWS.Services.Dispatchers.URI is

   type Handler is new AWS.Dispatchers.Handler with private;

   procedure Register
     (Dispatcher : in out Handler;
      URI        : String;
      Action     : AWS.Dispatchers.Handler'Class;
      Prefix     : Boolean := False);
   --  Register URI to use the specified dispatcher. URI is the full string
   --  that must match the resource requested (with the leading /). If Prefix
   --  is True, only the URI prefix is checked.

   procedure Register
     (Dispatcher : in out Handler;
      URI        : String;
      Action     : Response.Callback;
      Prefix     : Boolean := False);
   --  Idem as above but take a callback procedure as parameter

   procedure Register_Regexp
     (Dispatcher : in out Handler;
      URI        : String;
      Action     : AWS.Dispatchers.Handler'Class);
   --  Register URI to use the specified dispatcher. URI is a regular
   --  expression that must match the resource requested (with the leading /).

   procedure Register_Regexp
     (Dispatcher : in out Handler;
      URI        : String;
      Action     : Response.Callback);
   --  Idem as above but take a callback procedure as parameter

   procedure Unregister
     (Dispatcher : in out Handler;
      URI        : String);
   --  Removes URI from the list. URI is either a name or a regexp and must
   --  have exactly the value used with Register.

   procedure Register_Default_Callback
     (Dispatcher : in out Handler;
      Action     : AWS.Dispatchers.Handler'Class);
   --  Register the default callback. This will be used if no URI match
   --  the request.

private

   use Ada.Strings.Unbounded;

   overriding procedure Finalize (Dispatcher : in out Handler);

   overriding function Dispatch
     (Dispatcher : Handler;
      Request    : Status.Data) return Response.Data;
   --  Dispatch will return the value returned by the first callback matching
   --  the request. Note that if a callback returns the Response.Empty message,
   --  Dispatch will just continue to the next matching callback. In any case,
   --  if no handler matches it will call the default callback. If no default
   --  callback is registered an error (code 404) HTML message is returned.

   overriding function Clone (Dispatcher : Handler) return Handler;
   --  Returns a deep copy of the dispatcher

   type Std_URI is new Utils.Clonable with record
      Action : AWS.Dispatchers.Handler_Class_Access;
      URI    : Unbounded_String;
      Prefix : Boolean;
   end record;

   overriding function Clone (URI : Std_URI) return Std_URI;

   function Match (URI : Std_URI; Value : String) return Boolean;

   type URI_Class_Access is access all Std_URI'Class;

   package URI_Table is
     new Ada.Containers.Vectors (Positive, URI_Class_Access, "=");

   type Handler is new AWS.Dispatchers.Handler with record
      Action : AWS.Dispatchers.Handler_Class_Access;
      Table  : URI_Table.Vector;
   end record;

end AWS.Services.Dispatchers.URI;
