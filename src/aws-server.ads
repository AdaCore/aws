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
with Ada.Strings.Unbounded;

with Sockets;
with AWS.Response;

package AWS.Server is

   Default_Connection  : constant := 10;
   Default_Port        : constant := 8080;
   Default_Upload_Path : constant String := ".";
   No_Admin            : constant String := "";

   type HTTP
     (Max_Connection : Positive          := Default_Connection;
      Port           : Positive          := Default_Port;
      Security       : Boolean           := False;
      CB             : Response.Callback := Response.Default_Handler;
      Session        : Boolean           := False) is
   limited private;
   --  Max_Connection is the maximum number of simultaneous connection
   --  handled by the server. Port is the socket server port. If security is
   --  set to True then an HTTPS (secure socket) will be created. CB is the
   --  user's callback function which reply to request sent to the
   --  server.

   procedure Start (Web_Server : in out HTTP;
                    Name       : in String;
                    Admin_URI  : in String := No_Admin);
   --  Start the Web server. It initialize the connections lines.
   --  Admin_URI must be set to enable the admin status page.

   procedure Shutdown (Web_Server : in out HTTP);

private

   Keep_Open_Duration : constant Duration := 30.0;

   type HTTP_Access is access all HTTP;

   type Slot is record
      Sock                : Sockets.Socket_FD;
      Peername            : Ada.Strings.Unbounded.Unbounded_String;
      Opened              : Boolean := False;
      Abortable           : Boolean := False;
      Activity_Counter    : Natural := 0;
      Activity_Time_Stamp : Ada.Calendar.Time := Ada.Calendar.Clock;
   end record;

   --  Abortable is set to true when the line can be aborted by closing the
   --  associated socket. Activity_Time_Stamp is the last time the line has
   --  been used. The line with the oldest activity could be closed.
   --  Also a line is closed after Keep_Open_Duration seconds of inactivity.

   type Slot_Set is array (Positive range <>) of Slot;

   -----------
   -- Slots --
   -----------

   protected type Slots (N : Positive) is

      procedure Set_Abortable (Index : in Positive; Flag : in Boolean);
      --  Set Abortable field to Flag for the Line number Index. This flag is
      --  used by the Line_Cleaner to know if a line can be aborted safely.

      procedure Set_Peername (Index : in Positive; Peername : in String);
      --  Set the Peername for the associated socket.

      procedure Mark_Activity_Time (Index : in Positive);
      --  Set Activity_Time_Stamp which is the last time where the line number
      --  Index as been used.

      procedure Abort_Oldest  (Force : in Boolean);
      --  Abort oldest line (the line with the oldest activity time stamp) if
      --  force is True. Otherwise the Line must be in an abortable state.

      procedure Get (FD : in Sockets.Socket_FD; Index : in Positive);
      --  Mark slot at position Index to be used. This slot will be associated
      --  with the socket FD. Opened status is set to True.

      procedure Release  (Index : in Positive);
      --  Release slot number Index. Opened status us set to False.

      function Free return Boolean;
      --  Returns True if there is some free slots available.

      function Get (Index : in Positive) return Slot;
      --  Returns Slot data

      function Get_Peername (Index : in Positive) return String;
      --  Returns the peername for socket at position Index.

   private
      Set   : Slot_Set (1 .. N);
      Count : Natural := N;
   end Slots;

   ----------
   -- Line --
   ----------

   task type Line is
      entry Start (Server : in HTTP; Index : in Positive);
   end Line;

   type Line_Set is array (Positive range <>) of Line;

   ------------------
   -- Line_Cleaner --
   ------------------

   task type Line_Cleaner (Server : HTTP_Access) is
     entry Force;
   end Line_Cleaner;
   --  run through the slots and see if some of them could be closed.

   use Ada.Strings.Unbounded;

   type HTTP
     (Max_Connection : Positive          := Default_Connection;
      Port           : Positive          := Default_Port;
      Security       : Boolean           := False;
      CB             : Response.Callback := Response.Default_Handler;
      Session        : Boolean           := False) is
   limited record
      Self        : HTTP_Access := HTTP'Unchecked_Access;
      Name        : Unbounded_String;
      Upload_Path : Unbounded_String;
      Sock        : Sockets.Socket_FD;
      --  this is the server socket for incoming connection.

      Lines       : Line_Set (1 .. Max_Connection);
      Slots       : Server.Slots (Max_Connection);
      Cleaner     : Line_Cleaner (HTTP'Unchecked_Access);
      Admin_URI   : Unbounded_String;
   end record;

end AWS.Server;
