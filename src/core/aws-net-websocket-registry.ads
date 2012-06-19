------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                       Copyright (C) 2012, AdaCore                        --
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

--  This package is used to build and register the active WebSockets. Some
--  services to send or broadcast messages are also provided.

private with GNAT.Regexp;

package AWS.Net.WebSocket.Registry is

   type Factory is access function
     (Socket  : Socket_Access;
      Request : AWS.Status.Data) return Object'Class;

   --  Creating and Registering WebSockets

   function Constructor (URI : String) return Registry.Factory;
   --  Get the WebObject's constructor for a specific URI

   procedure Register (URI : String; Factory : Registry.Factory);
   --  Register a WebObject's constructor for a specific URI

   procedure Register (WebSocket : Object'Class);
   --  Register a new WebSocket which must be watched for incoming data

   --  Sending messages

   type Recipient is private;

   function Create (URI : String; Origin : String := "") return Recipient;
   --  A recipient with only an URI is called a broadcast as it designate all
   --  registered WebSocket for this specific URI. If Origin is specified then
   --  it designates a single client.
   --
   --  Note that both URI and Origin can be regular expressions.

   procedure Send (To : Recipient; Message : String);
   --  Send a message to the WebSocket designated by Origin and URI

private

   use GNAT.Regexp;

   type Recipient is record
      URI_Set    : Boolean := False;
      URI        : Regexp  := Compile ("");
      Origin_Set : Boolean := False;
      Origin     : Regexp  := Compile ("");
   end record;

   procedure Start;
   --  Start the WebServer's servers

   procedure Shutdown;
   --  Stop the WebServer's servers

end AWS.Net.WebSocket.Registry;
