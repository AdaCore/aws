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

with AWS.Dispatchers;
with AWS.Response;
with AWS.Status;

private with Ada.Containers.Indefinite_Hashed_Maps;
private with Ada.Strings.Hash;
private with Ada.Strings.Unbounded;

package AWS.Services.Dispatchers.Virtual_Host is

   type Handler is new AWS.Dispatchers.Handler with private;

   procedure Register
     (Dispatcher       : in out Handler;
      Virtual_Hostname : String;
      Hostname         : String);
   --  Register Virtual_Hostname to be a redirection to the specified
   --  hostname.

   procedure Register
     (Dispatcher       : in out Handler;
      Virtual_Hostname : String;
      Action           : AWS.Dispatchers.Handler'Class);
   --  Register Virtual_Hostname to use the specified callback

   procedure Register
     (Dispatcher       : in out Handler;
      Virtual_Hostname : String;
      Action           : Response.Callback);
   --  Idem as above but take a callback procedure as parameter

   procedure Unregister
     (Dispatcher       : in out Handler;
      Virtual_Hostname : String);
   --  Removes Virtual_Hostname from the list of virtual hostnames to handle

   procedure Register_Default_Callback
     (Dispatcher : in out Handler;
      Action     : AWS.Dispatchers.Handler'Class);
   --  Register the default callback. This will be used if no Virtual_Hostname
   --  match the request.

private

   use Ada.Strings.Unbounded;

   overriding procedure Initialize (Dispatcher : in out Handler);
   overriding procedure Finalize   (Dispatcher : in out Handler);

   overriding function Dispatch
     (Dispatcher : Handler;
      Request    : Status.Data) return Response.Data;
   --  Returns an error message (code 404) if there is no match for the request

   overriding function Clone (Dispatcher : Handler) return Handler;
   --  Returns a deep copy of the dispatcher

   type VH_Mode is (Host, Callback);

   type VH_Node (Mode : VH_Mode := Host) is record
      case Mode is
         when Host =>
            Hostname : Unbounded_String;
         when Callback =>
            Action   : AWS.Dispatchers.Handler_Class_Access;
      end case;
   end record;

   package Virtual_Host_Table is new Ada.Containers.Indefinite_Hashed_Maps
       (String, VH_Node, Ada.Strings.Hash, "=", "=");

   type Handler is new AWS.Dispatchers.Handler with record
      Action : AWS.Dispatchers.Handler_Class_Access;
      Table  : Virtual_Host_Table.Map;
   end record;

end AWS.Services.Dispatchers.Virtual_Host;
