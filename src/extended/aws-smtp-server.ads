------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2008-2013, AdaCore                     --
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

pragma Ada_2012;

with AWS.SMTP.Messages;

private with AWS.Config;

package AWS.SMTP.Server is

   type Callback is access procedure (Message : Messages.Data);

   type Handle is limited private;

   procedure Start
     (Server : in out Handle;
      Host   : Receiver;
      Action : Callback);
   --  Start the server. This must be called once

   procedure Shutdown (Server : in out Handle);
   --  Stop the server and release all associated memory

private

   task type Mail_Handler (Server : not null access Handle)
     with Priority => Config.Service_Priority
   is
      entry Start;
   end Mail_Handler;

   type Handle is limited record
      Server_Handler : Mail_Handler (Handle'Access);
      Host           : Receiver;
      Action         : Callback;
      Shutdown       : Boolean; -- True if shutdown is in progress
   end record;

end AWS.SMTP.Server;
