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

with Ada.Strings.Unbounded;

with Strings_Maps;

with AWS.Dispatchers;
with AWS.Response;
with AWS.Status;

package AWS.Services.Dispatchers.Virtual_Host is

   type Handler is new AWS.Dispatchers.Handler with private;

   function Dispatch
     (Dispatcher : in Handler;
      Request    : in Status.Data)
      return Response.Data;

   procedure Register
     (Dispatcher       : in out Handler;
      Virtual_Hostname : in     String;
      Hostname         : in     String);
   --  Register Virtual_Hostname to be a redirection to the specified
   --  hostname.

   procedure Register
     (Dispatcher       : in out Handler;
      Virtual_Hostname : in     String;
      Action           : in     AWS.Dispatchers.Handler'Class);
   --  Register Virtual_Hostname to use the specified callback.

   procedure Register
     (Dispatcher       : in out Handler;
      Virtual_Hostname : in     String;
      Action           : in     Response.Callback);
   --  Idem as above but take a callback procedure as parameter.

   procedure Unregister
     (Dispatcher       : in out Handler;
      Virtual_Hostname : in     String);
   --  Removes Virtual_Hostname from the list of virtual hostnames to handle.

   procedure Register_Default_Callback
     (Dispatcher : in out Handler;
      Action     : in     AWS.Dispatchers.Handler'Class);
   --  Register the default callback. This will be used if no Virtual_Hostname
   --  match the request.

private

   use Ada.Strings.Unbounded;

   procedure Initialize (Dispatcher : in out Handler);
   procedure Finalize   (Dispatcher : in out Handler);

   type VH_Mode is (Host, Callback);

   type VH_Node (Mode : VH_Mode := Host) is record
      case Mode is
         when Host =>
            Hostname : Unbounded_String;
         when Callback =>
            Action   : AWS.Dispatchers.Handler_Class_Access;
      end case;
   end record;

   package Virtual_Host_Table is new Strings_Maps (VH_Node, "=");

   type Handler is new AWS.Dispatchers.Handler with record
      Action : AWS.Dispatchers.Handler_Class_Access;
      Table  : Virtual_Host_Table.Map;
   end record;

end AWS.Services.Dispatchers.Virtual_Host;
