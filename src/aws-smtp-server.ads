------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                 S M T P - Simple Mail Transfer Protocol                  --
--                                                                          --
--                     Copyright (C) 2008-2009, AdaCore                     --
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

with AWS.SMTP.Messages;

package AWS.SMTP.Server is

   type Callback is access procedure (Message : in Messages.Data);

   type Handle is limited private;

   procedure Start
     (Server : in out Handle;
      Host   : in     Receiver;
      Action : in     Callback);
   --  Start the server. This must be called once

   procedure Shutdown (Server : in out Handle);
   --  Stop the server and release all associated memory

private

   task type Mail_Handler (Server : not null access Handle) is
      entry Start;
   end Mail_Handler;

   type Handle is limited record
      Server_Handler : Mail_Handler (Handle'Access);
      Host           : Receiver;
      Action         : Callback;
      Shutdown       : Boolean; -- True if shutdown is in progress
   end record;

end AWS.SMTP.Server;
