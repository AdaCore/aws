------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                            Copyright (C) 2000                            --
--                      Dmitriy Anisimkov & Pascal Obry                     --
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

with Ada.Calendar;

with Sockets;
with AWS.Response;

package AWS.Server is

   Default_Connection : constant := 10;
   Default_Port       : constant := 8080;

   type HTTP
     (Max_Connection : Positive          := Default_Connection;
      Port           : Positive          := Default_Port;
      CB             : Response.Callback := Response.Default_Handler) is
   limited private;

   procedure Start (Web_Server : in out HTTP);
   --  Start the Web server. It initialize the connections lines.

private

   Keep_Open_Duration : constant Duration := 120.0;

   type HTTP_Access is access all HTTP;

   type Slot is record
      Sock                : Sockets.Socket_FD;
      Opened              : Boolean := False;
      Abortable           : Boolean := False;
      Quit                : Boolean := False;
      Activity_Time_Stamp : Ada.Calendar.Time;
   end record;

   --  Abortable is set to true when the line can be aborted by closing the
   --  associated socket. Activity_Time_Stamp is the last time the line has
   --  been used. The line with the oldest activity could be closed.
   --  Also a line is closed after Keep_Open_Duration seconds of inactivity.

   type Slot_Set is array (Positive range <>) of Slot;

   protected type Slots (N : Positive) is

      procedure Set_Abortable (Index : in Positive; Flag : in Boolean);
      --  set Abortable field to Flag for the Line number Index. This flag is
      --  used by the Line_Cleaner to know if a line can be aborted safely.

      procedure Mark_Activity_Time (Index : in Positive);
      --  set Activity_Time_Stamp which is the last time where the line number
      --  Index as been used.

      procedure Abort_Oldest  (Force : in Boolean);
      --  abort oldest line (the line with the oldest activity time stamp) if
      --  force is True. Otherwise the Line must be in an abortable state.

      procedure Get (FD : in Sockets.Socket_FD; Index : in Positive);
      --  get the slot number Index. Opened status is set to True.

      procedure Release  (Index : in Positive);
      --  release slot number Index. Opened status us set to False.

      function Free return Boolean;
      --  returns True if there is some free slots available.

   private
      Set   : Slot_Set (1 .. N);
      Count : Natural := N;
   end Slots;

   type Slots_Access is access all Slots;

   task type Line is
      entry Start (Server : in HTTP; Index : in Positive);
   end Line;

   type Line_Set is array (Positive range <>) of Line;

   task type Line_Cleaner (Server : HTTP_Access) is
     entry Force;
   end Line_Cleaner;
   --  run through the slots and see if some of them could be closed.

   type HTTP
     (Max_Connection : Positive          := Default_Connection;
      Port           : Positive          := Default_Port;
      CB             : Response.Callback := Response.Default_Handler) is
   limited record
      Sock    : Sockets.Socket_FD;
      --  this is the server socket for incoming connection.

      Lines   : Line_Set (1 .. Max_Connection);
      Slots   : Slots_Access := new Server.Slots (Max_Connection);
      Cleaner : Line_Cleaner (HTTP'Unchecked_Access);
   end record;

end AWS.Server;
