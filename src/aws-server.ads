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

--  $Id$

with Ada.Calendar;
with Ada.Finalization;

with Sockets.Naming;

with AWS.Response;
with AWS.Hotplug;
with AWS.Config;
with AWS.Default;
with AWS.Log;
with AWS.Dispatchers;

package AWS.Server is

   Def_Admin_URI   : String renames Default.Admin_URI;
   Def_Upload_Dir  : String renames Default.Upload_Directory;
   Def_Max_Connect     : constant := Default.Max_Connection;
   Def_Port            : constant := Default.Server_Port;
   Def_Line_Stack_Size : constant := Default.Line_Stack_Size;

   type HTTP is limited private;
   --  A Web server.

   procedure Start
     (Web_Server : in out HTTP;
      Callback   : in     Response.Callback;
      Config     : in     AWS.Config.Object);
   --  Start server using a full configuration object. With this routine it is
   --  possible to control all features of the server. A simplified version of
   --  Start is also provided below with the most common options.
   --  User_Config_Filename is a specific configuration file that will parsed
   --  after 'aws.ini', 'prognam.ini', '<servername>.ini' files.

   procedure Start
     (Web_Server : in out HTTP;
      Dispatcher : in     Dispatchers.Handler'Class;
      Config     : in     AWS.Config.Object);
   --  The same, but using the dispatcher tagged type instead of callback. See
   --  AWS.Services.Dispatchers hierarchy for built-in services.

   procedure Start
     (Web_Server                : in out HTTP;
      Name                      : in     String;
      Callback                  : in     Response.Callback;
      Max_Connection            : in     Positive     := Def_Max_Connect;
      Admin_URI                 : in     String       := Def_Admin_URI;
      Port                      : in     Positive     := Def_Port;
      Security                  : in     Boolean      := False;
      Session                   : in     Boolean      := False;
      Case_Sensitive_Parameters : in     Boolean      := True;
      Upload_Directory          : in     String       := Def_Upload_Dir;
      Line_Stack_Size           : in     Positive     := Def_Line_Stack_Size);
   --  Start the Web server. It initialize the Max_Connection connections
   --  lines. Name is just a string used to identify the server. This is used
   --  for example in the administrative page. Admin_URI must be set to enable
   --  the administrative status page. Callback is the procedure to call for
   --  each resource requested. Port is the Web server port. If Security is
   --  set to True the server will use an HTTPS/SSL connection. If Session is
   --  set to True the server will be able to get a status for each client
   --  connected. A session ID is used for that, on the client side it is a
   --  cookie. Case_Sensitive_Parameters if set to False it means that the CGI
   --  parameters name will be handled without case sensitivity. Upload
   --  directory point to a directory where uploaded files will be stored.

   procedure Shutdown (Web_Server : in out HTTP);
   --  Stop the server and release all associated memory. This routine can
   --  take some time to terminate because it waits for all tasks to terminate
   --  properly before releasing the memory. The log facilities will be
   --  automatically stopped by calling Stop_Log below.

   type Termination is (No_Server, Q_Key_Pressed, Forever);

   procedure Wait (Mode : in Termination := No_Server);
   --  The purpose of this procedure is to control the main procedure
   --  termination. This procedure will return only when no server is active
   --  (No_Server mode) or the 'q' key has been pressed. If mode is set to
   --  Forever Wait will never return. The process will have to be killed.

   function Config (Web_Server : in HTTP) return AWS.Config.Object;
   --  Returns configuration object for Web_Server.

   procedure Start_Log
     (Web_Server      : in out HTTP;
      Split_Mode      : in     Log.Split_Mode := Log.None;
      Filename_Prefix : in     String         := "");
   --  Activate server's logging activity. See AWS.Log.

   procedure Stop_Log (Web_Server : in out HTTP);
   --  Stop server's logging activity. See AWS.Log.

   type HTTP_Access is access all HTTP;

private

   type Slot_Phase is
     (Closed,
      --  Socket has been closed by one of the peer

      Aborted,
      --  After socket shutdown

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

   type Timeouts_Array is array (Timeout_Mode, Abortable_Phase) of Duration;

   type Data_Timeouts_Array is array (Data_Phase) of Duration;

   type Socket_Access is access all Sockets.Socket_FD'Class;

   type Slot is record
      Sock                  : Socket_Access := null;
      Socket_Taken          : Boolean := False;
      Peer_Addr             : Sockets.Naming.Address
         := Sockets.Naming.Any_Address;
      Phase                 : Slot_Phase := Closed;
      Phase_Time_Stamp      : Ada.Calendar.Time := Ada.Calendar.Clock;
      Data_Time_Stamp       : Ada.Calendar.Time;
      Alive_Time_Stamp      : Ada.Calendar.Time;
      Slot_Activity_Counter : Natural := 0;
      Activity_Counter      : Natural := 0;
      Alive_Counter         : Natural;
   end record;

   --  Abortable is set to true when the line can be aborted by closing the
   --  associated socket. Phase_Time_Stamp is the last time when Phase of line
   --  has been changed. The line in Abortable state and with oldest
   --  Phase_Time_Stamp could be closed.
   --  Also a line is closed after Keep_Open_Duration seconds of inactivity.

   type Slot_Set is array (Positive range <>) of Slot;

   package CNF renames AWS.Config;

   -----------
   -- Slots --
   -----------

   protected type Slots (N : Positive) is

      procedure Set_Peer_Addr
        (Index     : in Positive;
         Peer_Addr : in Sockets.Naming.Address);
      --  Set the Peer address for the associated socket.

      procedure Mark_Phase (Index : in Positive; Phase : Slot_Phase);
      --  Set Activity_Time_Stamp which is the last time where the line number
      --  Index as been used.

      procedure Socket_Taken (Index : in Positive);
      --  Used to mark slot associated socket has "taken" by some foreign code.
      --  The server must not close this socket on releasing the slot. It is
      --  used when passing socket to the server push part for example. In the
      --  future it could be used for other functionality over the same
      --  socket, changing HTTP to other protocol for example.

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

      procedure Get (FD : in Socket_Access; Index : in Positive);
      --  Mark slot at position Index to be used. This slot will be associated
      --  with the socket FD. Phase set to Client_Header.

      procedure Shutdown (Index : in Positive);
      --  Break all communications over the slot.
      --  Slot phase is set to Aborted.

      procedure Release  (Index : in Positive);
      --  Release slot number Index. Slot phase is set to Closed.

      function Get (Index : in Positive) return Slot;
      --  Returns Slot data.

      function Get_Peername (Index : in Positive) return String;
      --  Returns the peername for socket at position Index.

      procedure Increment_Slot_Activity_Counter (Index : in Positive);
      --  Add 1 to the slot activity. This is the total number of request
      --  handled by the slot.

      procedure Set_Timeouts
        (Phase_Timeouts : in Timeouts_Array;
         Data_Timeouts  : in Data_Timeouts_Array);
      --  Setup timeouts for slots before starting

   private

      Timeouts      : Timeouts_Array;
      Data_Timeouts : Data_Timeouts_Array;

      Set   : Slot_Set (1 .. N);
      Count : Natural := N;

   end Slots;

   type Slots_Access is access Slots;

   ----------
   -- Line --
   ----------

   task type Line (Stack_Size : Integer) is
      pragma Storage_Size (Stack_Size);
      entry Start (Server : in HTTP; Index : in Positive);
   end Line;

   type Line_Access is access Line;

   type Line_Set is array (Positive range <>) of Line_Access;

   type Line_Set_Access is access Line_Set;

   ---------------
   -- Semaphore --
   ---------------

   protected type Semaphore is
      entry Seize;
      procedure Release;
   private
      Seized : Boolean := False;
   end Semaphore;

   ------------------
   -- Line_Cleaner --
   ------------------

   task type Line_Cleaner (Server : HTTP_Access) is
     entry Force;
   end Line_Cleaner;

   type Line_Cleaner_Access is access Line_Cleaner;
   --  run through the slots and see if some of them could be closed.

   type HTTP is new Ada.Finalization.Limited_Controlled with record
      Self            : HTTP_Access := HTTP'Unchecked_Access;
      --  Point to the record.

      Start_Time      : Ada.Calendar.Time;
      --  Date and Time when server was started.

      Shutdown        : Boolean     := True;
      --  True when server is shutdown. This will be set to False when server
      --  will be started.

      Sock            : Sockets.Socket_FD;
      --  This is the server socket for incoming connection.

      Sock_Sem        : Semaphore;
      --  Semaphore used to serialize the accepts call on the server socket.

      Cleaner         : Line_Cleaner_Access;
      --  Task in charge of cleaning slots status. It checks from time to time
      --  is the slots is still in used and closed it if possible.

      Properties      : CNF.Object := CNF.Get_Current;
      --  All server properties controled by the configuration file.

      Log             : AWS.Log.Object;
      --  Loggin support.

      Dispatcher      : Dispatchers.Handler_Class_Access;
      --  Dispatcher for the user actions.

      Filters         : Hotplug.Filter_Set;
      --  Hotplug filters are recorded here.

      Lines           : Line_Set_Access;
      --  The tasks doing the job.

      Slots           : Slots_Access;
      --  Information about each tasks above. This is a protected object to
      --  support concurrency.
   end record;

   procedure Initialize (Web_Server : in out HTTP);
   procedure Finalize   (Web_Server : in out HTTP);

end AWS.Server;
