------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                            Copyright (C) 2000                            --
--                               Pascal Obry                                --
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

package AWS.Connection is

   use Ada;

   type Slot is limited private;

   task type Line (Slot : access Connection.Slot) is
      entry Start (FD : in Sockets.Socket_FD;
                   CB : in Response.Callback);
   end Line;

   procedure Create_Slots (N : in Positive);
   --  creates N slot lines. This is the maximum number of connection that
   --  will be available at the same time.

   function Get_Free_Slot return Line;
   --  return a free line to answer to a client.

private

   Keep_Open_Duration : constant Duration := 120.0;

   type Slot is record
      L                   : Line (Slot'Unchecked_Access);
      Sock                : Sockets.Socket_FD;
      Free                : Boolean := True;
      Abortable           : Boolean := False;
      Activity_Time_Stamp : Calendar.Time;
   end record;

   --  Abortable is set to true when the line can be aborted by closing the
   --  associated socket. Get_Free_Slot use this info to get a free line when
   --  none are available. Activity_Time_Stamp is the last time the line has
   --  been used. The line with the oldest activity is closed.
   --  Also a line is closed after Keep_Open_Duration seconds of inactivity.

   --  Note that Line discriminant is an auto-pointer to the line Slot record.

end AWS.Connection;
