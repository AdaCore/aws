------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                          Copyright (C) 2000-2001                         --
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
with AWS.Hotplug;

package AWS.Server is

   Default_Connection  : constant := 10;
   Default_Port        : constant := 8080;
   Default_Upload_Path : constant String := ".";
   No_Admin            : constant String := "";

   type HTTP
     (Max_Connection : Positive := Default_Connection) is
   limited private;
   --  Max_Connection is the maximum number of simultaneous connection
   --  handled by the server.

   procedure Start
     (Web_Server                : in out HTTP;
      Name                      : in     String;
      Callback                  : in     Response.Callback;
      Admin_URI                 : in     String            := No_Admin;
      Port                      : in     Positive          := Default_Port;
      Security                  : in     Boolean           := False;
      Session                   : in     Boolean           := False;
      Case_Sensitive_Parameters : in     Boolean           := True);
   --  Start the Web server. It initialize the connections lines.
   --  Name is just a string used to identify the server. This is used for
   --  example in the administrative page. Admin_URI must be set to enable the
   --  administrative status page.
   --  Callback is the procedure to call for each ressource requested.
   --  Port is the Web server port. If Security is set to True the server will
   --  use an HTTPS/SSL connection. If Session is set to True the server will
   --  be able to get a status for each client connected. A session ID is used
   --  for that, on the client side it is a cookie. Case_Sensitive_Parameters
   --  if set to False it means that the CGI parameters name will be handled
   --  without case sensitivity.

   procedure Shutdown (Web_Server : in out HTTP);
   --  Stop the server and release all associated memory.

   type HTTP_Access is access all HTTP;

private

   Keep_Open_Duration    : constant Duration := 80.0;
   Client_Header_Timeout : constant Duration := 5.0;

   type Slot_Phase is
     (Closed,

      Wait_For_Client,
      --  We can abort keep-alive connection in this stage

      Client_Header,
      --  We can abort keep-alive connection when client header
      --  takes too much time

      Client_Data,
      --  We should think about it. Maybe we should not trust the clients
      --  who are spending too much server time in sending data

      Server_Response
      --  We are already trust to ourselves
     );

   type Slot is record
      Sock                  : Sockets.Socket_FD;
      Peername              : Ada.Strings.Unbounded.Unbounded_String;
      Abortable             : Boolean := False;
      Phase                 : Slot_Phase := Closed;
      Phase_Time_Stamp      : Ada.Calendar.Time := Ada.Calendar.Clock;
      Slot_Activity_Counter : Natural := 0;
      Activity_Counter      : Natural := 0;
   end record;

   --  Abortable is set to true when the line can be aborted by closing the
   --  associated socket. Phase_Time_Stamp is the last time when Phase of line
   --  has been changed. The line in Abortable state and with oldest
   --  Phase_Time_Stamp could be closed.
   --  Also a line is closed after Keep_Open_Duration seconds of inactivity.

   type Slot_Set is array (Positive range <>) of Slot;

   -----------
   -- Slots --
   -----------

   protected type Slots (N : Positive) is

      procedure Set_Peername (Index : in Positive; Peername : in String);
      --  Set the Peername for the associated socket.

      procedure Mark_Phase (Index : in Positive; Phase : Slot_Phase);
      --  Set Activity_Time_Stamp which is the last time where the line number
      --  Index as been used.

      procedure Check_Timeouts;
      --  Check slots timeout and set slots abortable state if possible.

      procedure Abort_Oldest  (Force : in Boolean);
      --  Abort oldest line (the line with the oldest activity time stamp) if
      --  force is True. Otherwise the Line must be older then
      --  Keep_Open_Duration.
      --  Anyway Line mast be in abortable state for abortion.

      entry Get (FD : in Sockets.Socket_FD; Index : in Positive);
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

      procedure Increment_Slot_Activity_Counter (Index : in Positive);
      --  Add 1 to the slot activity. This is the total number of request
      --  handled by the slot.

   private
      Set             : Slot_Set (1 .. N);
      Count           : Natural := N;
      Abortable_Count : Natural := 0;

      procedure Set_Abortable (Index : in Positive; Flag : in Boolean);
      --  Set Abortable field to Flag for the Line number Index. This flag is
      --  used by the Line_Cleaner to know if a line can be aborted safely.

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
     (Max_Connection : Positive := Default_Connection) is
   limited record
      Self                      : HTTP_Access := HTTP'Unchecked_Access;
      --  Point to the record.
      Shutdown                  : Boolean     := False;
      --  True when shutdown has been requested.
      Name                      : Unbounded_String;
      --  The server's name.
      Upload_Path               : Unbounded_String;
      --  Path where uploaded file will be stored.
      Sock                      : Sockets.Socket_FD;
      --  This is the server socket for incoming connection.
      Port                      : Positive;
      --  The Web server port.
      Cleaner                   : Line_Cleaner (HTTP'Unchecked_Access);
      --  Task in charge of cleaning slots status. It checks from time to time
      --  is the slots is still in used and closed it if possible.
      Admin_URI                 : Unbounded_String;
      --  URI to get the administrative page.
      Security                  : Boolean;
      --  Is set to true if this is an SSL server.
      Session                   : Boolean;
      --  Is set to true if server must support session data.
      Case_Sensitive_Parameters : Boolean;
      --  Is set to true if forms parameters name are case sensitive.
      CB                        : Response.Callback;
      --  User's callback procedure.
      Filters                   : Hotplug.Filter_Set;
      --  Hotplug filters are recorded here.
      Lines                     : Line_Set (1 .. Max_Connection);
      --  The tasks doing the job.
      Slots                     : Server.Slots (Max_Connection);
      --  Information about each tasks above. This is a protected object to
      --  support concurrency.
   end record;

end AWS.Server;
