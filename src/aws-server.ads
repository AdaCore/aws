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
with AWS.Config;

package AWS.Server is

   Def_Admin_URI  : String   renames Config.Default_Admin_URI;
   Def_Upload_Dir : String   renames Config.Default_Upload_Directory;
   Def_Port       : constant := Config.Default_Server_Port;

   type HTTP
     (Max_Connection : Positive := Config.Default_Max_Connection)
   is limited private;
   --  Max_Connection is the maximum number of simultaneous connection
   --  handled by the server.

   procedure Start
     (Web_Server                : in out HTTP;
      Name                      : in     String;
      Callback                  : in     Response.Callback;
      Admin_URI                 : in     String            := Def_Admin_URI;
      Port                      : in     Positive          := Def_Port;
      Security                  : in     Boolean           := False;
      Session                   : in     Boolean           := False;
      Case_Sensitive_Parameters : in     Boolean           := True;
      Upload_Directory          : in     String            := Def_Upload_Dir);
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
   --  without case sensitivity. Upload directory point to a directory where
   --  uploaded files will be stored.

   procedure Shutdown (Web_Server : in out HTTP);
   --  Stop the server and release all associated memory.

   type HTTP_Access is access all HTTP;

private

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

      Server_Response,
      --  We are trusting ourselves but client may be too slow purposely in
      --  receiving data and we should disconnect him.

      Server_Processing
      --  While in the User's Callback procedure.
     );

   subtype Abortable_Phase is Slot_Phase
     range Wait_For_Client .. Server_Response;

   subtype Data_Phase is Abortable_Phase
     range Client_Data .. Server_Response;

   --  This is Force timeouts and timeouts for Line_Cleaner task.

   type Timeout_Mode is (Cleaner, Force);

   --  maybe this timeouts should be in the server configuration
   Timeouts : constant array (Timeout_Mode, Abortable_Phase) of Duration
     := (Cleaner => -- Timeouts for Line_Cleaner
           (Wait_For_Client  => Config.Cleaner_Wait_For_Client_Timeout,
            Client_Header    => Config.Cleaner_Client_Header_Timeout,
            Client_Data      => Config.Cleaner_Client_Data_Timeout,
            Server_Response  => Config.Cleaner_Server_Response_Timeout),

         Force   => -- Force timeouts used when there is no free slot
           (Wait_For_Client  => Config.Force_Wait_For_Client_Timeout,
            Client_Header    => Config.Force_Client_Header_Timeout,
            Client_Data      => Config.Force_Client_Data_Timeout,
            Server_Response  => Config.Cleaner_Server_Response_Timeout));

   Data_Timeouts : constant array (Data_Phase) of Duration
     := (Client_Data     => Config.Receive_Timeout,
         Server_Response => Config.Send_Timeout);

   type Slot is record
      Sock                  : Sockets.Socket_FD;
      Peername              : Ada.Strings.Unbounded.Unbounded_String;
      Phase                 : Slot_Phase := Closed;
      Phase_Time_Stamp      : Ada.Calendar.Time := Ada.Calendar.Clock;
      Data_Time_Stamp       : Ada.Calendar.Time;
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

      procedure Mark_Data_Time_Stamp (Index : in Positive);
      --  Mark timestamp for receive or send chunk of data.

      function Is_Abortable
        (Index : in Positive;
         Mode  : in Timeout_Mode)
        return Boolean;
      --  Return True when slot can be aborted.

      procedure Abort_On_Timeout (Mode : in Timeout_Mode; Done : out Boolean);
      --  Abort slots if timeout exceeded.
      --  Set Done to True in case of abortion.

      function Free_Slots return Natural;
      --  Returns number of free slots.

      procedure Get (FD : in Sockets.Socket_FD; Index : in Positive);
      --  Mark slot at position Index to be used. This slot will be associated
      --  with the socket FD. Phase set to Client_Header.

      procedure Release  (Index : in Positive);
      --  Release slot number Index. Opened status us set to False.

      function Free return Boolean;
      --  Returns True if there is some free slots available.

      function Get (Index : in Positive) return Slot;
      --  Returns Slot data.

      function Get_Peername (Index : in Positive) return String;
      --  Returns the peername for socket at position Index.

      procedure Increment_Slot_Activity_Counter (Index : in Positive);
      --  Add 1 to the slot activity. This is the total number of request
      --  handled by the slot.

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
     (Max_Connection : Positive := Config.Default_Max_Connection) is
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
