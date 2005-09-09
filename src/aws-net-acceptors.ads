------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                            Copyright (C) 2005                            --
--                                ACT-Europe                                --
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

--  $Id$

--  Waiting for group of sockets for read data availability and accept for new
--  connections.

with Ada.Finalization;

with AWS.Net.Std;
with AWS.Net.Sets;

with AWS.Utils;

package AWS.Net.Acceptors is

   type Acceptor_Type is limited private;

   procedure Listen
     (Acceptor   : in out Acceptor_Type;
      Host       : in     String;
      Port       : in     Positive;
      Queue_Size : in     Positive);
   --  Prepare Acceptor to accept sockets and wait for incoming data.

   procedure Get
     (Acceptor : in out Acceptor_Type;
      Socket   :    out Socket_Access);
   --  Wait and get the socket from internal socket set which have a data to
   --  process. Should not be called simultaneously from different tasks.

   procedure Give_Back
     (Acceptor : in out Acceptor_Type;
      Socket   : in     Socket_Access);
   --  Give back socket which have been handled.
   --  Should be called from tasks different from task where Get routine is
   --  waiting.

   procedure Shutdown (Acceptor : in out Acceptor_Type);
   --  Shutdown all internal sockets.
   --  Should be called from task different from task where Get routine is
   --  waiting.

private

   package Mailboxes is new Utils.Mailbox_G (Socket_Access);

   type Acceptor_Type is new Ada.Finalization.Limited_Controlled with record
      Set      : Sets.Socket_Set_Type;
      W_Signal : Std.Socket_Type;
      R_Signal : Std.Socket_Type;
      Server   : Std.Socket_Type;
      Box      : Mailboxes.Mailbox (8);
      Index    : Sets.Socket_Count;
      Last     : Sets.Socket_Count;
   end record;

   procedure Finalize (Acceptor : in out Acceptor_Type);
   --  Automatic finalization.

end AWS.Net.Acceptors;
