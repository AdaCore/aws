------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2017, AdaCore                     --
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

with AWS.Config;
with AWS.Default;
with AWS.Dispatchers;
with AWS.Exceptions;
with AWS.Net.SSL;
with AWS.Response;
with AWS.Status;

private with Ada.Calendar;
private with Ada.Exceptions;
private with Ada.Finalization;
private with Ada.Task_Attributes;

with Ada.Task_Identification;

private with Ada.Real_Time;
private with System;

private with AWS.Log;
private with AWS.Net.Acceptors;
private with AWS.Hotplug;
private with AWS.Utils;

package AWS.Server is

   type HTTP is limited private;
   --  A Web server

   ---------------------------
   -- Server initialization --
   ---------------------------

   --  Note that starting a sercure server if AWS has not been configured to
   --  support HTTPS will raise Program_Error.

   procedure Start
     (Web_Server : in out HTTP;
      Callback   : Response.Callback;
      Config     : AWS.Config.Object);
   --  Start server using a full configuration object. With this routine it is
   --  possible to control all features of the server. A simplified version of
   --  Start is also provided below with the most common options.

   procedure Start
     (Web_Server : in out HTTP;
      Dispatcher : Dispatchers.Handler'Class;
      Config     : AWS.Config.Object);
   --  Idem, but using the dispatcher tagged type instead of callback. See
   --  AWS.Services.Dispatchers and AWS.Dispatchers hierarchies for built-in
   --  services and interface to build your own dispatcher models.
   --  Note that a copy of the Dispatcher is keept into Web_Server. Any
   --  changes done to the Dispatcher object will not be part of the Web
   --  server dispatcher.

   procedure Get_Message_Body;
   --  If size of message body is bigger than Upload_Size_Limit configuration
   --  parameter, server do not receive message body before calling user's
   --  callback routine. If user decide to get the message body he should call
   --  this routine.

   procedure Start
     (Web_Server                : in out HTTP;
      Name                      : String;
      Callback                  : Response.Callback;
      Max_Connection            : Positive  := Default.Max_Connection;
      Admin_URI                 : String    := Default.Admin_URI;
      Port                      : Natural   := Default.Server_Port;
      Security                  : Boolean   := False;
      Session                   : Boolean   := False;
      Case_Sensitive_Parameters : Boolean   := True;
      Upload_Directory          : String    := Default.Upload_Directory;
      Line_Stack_Size           : Positive  := Default.Line_Stack_Size);
   --  Start the Web server. Max_Connection is the number of simultaneous
   --  connections the server's will handle (the number of slots in AWS).
   --  Name is just a string used to identify the server. This is used
   --  for example in the administrative page. Admin_URI must be set to enable
   --  the administrative status page. Callback is the procedure to call for
   --  each resource requested. Port is the Web server port. If Security is
   --  set to True the server will use an HTTPS/SSL connection. If Session is
   --  set to True the server will be able to get a status for each client
   --  connected. A session Id is used for that, on the client side it is a
   --  cookie. Case_Sensitive_Parameters if set to False it means that the
   --  parameters name will be handled without case sensitivity. Upload
   --  directory point to a directory where uploaded files will be stored.

   ------------------------
   -- Server termination --
   ------------------------

   procedure Shutdown (Web_Server : in out HTTP);
   --  Stop the server and release all associated memory. This routine can
   --  take some time to terminate because it waits for all tasks to terminate
   --  properly before releasing the memory. The log facilities will be
   --  automatically stopped by calling Stop_Log below.

   type Termination is (No_Server, Q_Key_Pressed, Forever);

   procedure Wait (Mode : Termination := No_Server);
   --  The purpose of this procedure is to control the main procedure
   --  termination. This procedure will return only when no server are running
   --  (No_Server mode) or the 'q' key has been pressed. If mode is set to
   --  Forever, Wait will never return and the process will have to be killed.

   --------------------------
   -- Server configuration --
   --------------------------

   function Config (Web_Server : HTTP) return AWS.Config.Object;
   --  Returns configuration object for Web_Server

   procedure Set_Unexpected_Exception_Handler
     (Web_Server : in out HTTP;
      Handler    : Exceptions.Unexpected_Exception_Handler);
   --  Set the unexpected exception handler. It is called whenever an
   --  unrecoverable error has been detected. The default handler just display
   --  (on standard output) an error message with the location of the
   --  error. By changing this handler it is possible to log or display full
   --  symbolic stack backtrace if needed.

   procedure Set
     (Web_Server : in out HTTP;
      Dispatcher : Dispatchers.Handler'Class);
   --  Dynamically associate a new dispatcher object to the server. With the
   --  feature it is possible to change server behavior at runtime. The
   --  complete set of callback procedures will be changed when calling this
   --  routine. Note that any change in a dispatcher associated with a server
   --  using Register or Unregister must be reset into the server using this
   --  routine.

   procedure Set_Security
     (Web_Server           : in out HTTP;
      Certificate_Filename : String;
      Security_Mode        : Net.SSL.Method := Net.SSL.TLS_Server;
      Key_Filename         : String         := "");
   --  Set security option for AWS. Certificate_Filename is the name of a file
   --  containing a certificate. Key_Filename is the name of the file
   --  containing the key, if the empty string the key will be taken from the
   --  certificate filename. This must be called before starting the secure
   --  server otherwise the default security options or options set in the
   --  config files will be used. After that the call will have no effect.

   procedure Set_SSL_Config
     (Web_Server : in out HTTP; SSL_Config : Net.SSL.Config);
   --  Set the SSL configuration for this server

   function SSL_Config
     (Web_Server : in out HTTP) return not null access Net.SSL.Config;
   --  Returns the access to SSL config of the server. Allow to change SSL
   --  config on the already created server.

   procedure Set_Socket_Constructor
     (Web_Server         : in out HTTP;
      Socket_Constructor : Net.Socket_Constructor);
   --  Set the socket constructor routine to use when creating new sockets on
   --  the server. By calling this routine it is possible to replace the
   --  default AWS communication layer used. The default constructor is
   --  AWS.Net.Socket. Note that this routine must be called before starting
   --  the server. It is also important to note that sockets returned by the
   --  constructor must have the cache properly initialized. See AWS.Net.Cache
   --  for more information.

   type HTTP_Access is access all HTTP;

   function Get_Current return not null access HTTP;
   --  Get current server. This can be used in a callback procedure to
   --  retrieve the running HTTP server. It is needed when a callback
   --  procedure is shared by multiple servers.

   function Get_Status return Status.Data;
   --  Returns the current status data. This is useful to get the full status
   --  in a templates engine callback procedure for example.

   function Session_Name return String;
   --  Returns the current session cookie name

   function Session_Private_Name return String;
   --  Returns the current private session cookie name

   ---------------
   -- Other API --
   ---------------

   procedure Give_Back_Socket
     (Web_Server : in out HTTP; Socket : Net.Socket_Type'Class);
   --  Give the socket back to the server. Socket must have been taken from
   --  the server using the Response.Socket_Taken routine in a callback.

   procedure Give_Back_Socket
     (Web_Server : in out HTTP;
      Socket     : not null access Net.Socket_Type'Class);
   --  Idem.
   --  Use Socket_Access to avoid memory reallocation for already allocated
   --  sockets.

   procedure Set_Field (Id, Value : String);
   --  Set the extended log field value for the server the controlling the
   --  current task.

   procedure Skip_Log_Record;
   --  Disable logging only for the current processing request

   procedure Add_Listening
     (Web_Server    : in out HTTP;
      Host          : String;
      Port          : Natural;
      Family        : Net.Family_Type := Net.Family_Unspec;
      Reuse_Address : Boolean         := False;
      IPv6_Only     : Boolean         := False);
   --  Add the binded/listening socket on host, port and protocol family. To be
   --  able to connect web enabled application with others in the internal
   --  network, and then give access for external clients by listening on
   --  externally available address. Also it could be used to bind one server
   --  to IPv4 and IPv6 protocols simultaneously.
   --  IPv6_Only allows restrict IPv6 server to accept only IPv6 connections.

   type Task_Id_Array is
     array (Positive range <>) of Ada.Task_Identification.Task_Id;

   function Line_Tasks (Web_Server : HTTP) return Task_Id_Array;
   --  Returns line tasks identifiers

private

   use Ada;
   use Ada.Exceptions;

   procedure Default_Unexpected_Exception_Handler
     (E      : Exception_Occurrence;
      Log    : in out AWS.Log.Object;
      Error  : Exceptions.Data;
      Answer : in out Response.Data);
   --  Default unexpected exception handler

   procedure Socket_Taken;
   --  Mark socket of the cureent line of the current server taken

   ------------
   -- Phases --
   ------------

   type Slot_Phase is
     (Closed,
      --  Socket has been closed on the server side

      In_Shutdown,
      --  Phase when socket is marked in protected Slots for shutdown

      Aborted,
      --  Slot was aborted by means of socket shutdown

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
      --  receiving data and we should disconnect him

      Server_Processing
      --  While in the User defined Dispatch procedure
     );

   subtype Abortable_Phase is Slot_Phase
     range Wait_For_Client .. Server_Response;

   subtype Data_Phase is Abortable_Phase
     range Client_Data .. Server_Response;

   --------------
   -- Timeouts --
   --------------

   type Timeout_Mode is (Cleaner, Force);
   --  This is Force timeouts and timeouts for Line_Cleaner task

   type Timeouts_Array is array (Timeout_Mode, Abortable_Phase) of Duration;

   type Data_Timeouts_Array is array (Data_Phase) of Duration;

   subtype Socket_Access is Net.Socket_Access;

   ----------
   -- Slot --
   ----------

   type Slot is record
      Sock                  : Socket_Access  := null;
      Socket_Taken          : Boolean        := False;
      Phase                 : Slot_Phase     := Closed;
      Phase_Time_Stamp      : Real_Time.Time := Real_Time.Clock;
      Alive_Time_Stamp      : Calendar.Time;
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

   --  Socket Data used to retrieve safely socket information

   type Socket_Data (Peername_Length : Natural) is record
      Peername : String (1 .. Peername_Length);
      FD       : Integer;
   end record;

   -----------
   -- Slots --
   -----------

   protected type Slots (N : Positive) is

      procedure Mark_Phase (Index : Positive; Phase : Slot_Phase);
      --  Set Activity_Time_Stamp which is the last time where the line number
      --  Index as been used.

      procedure Check_Data_Timeout (Index : Positive);
      --  Check timeout of send/receive message body

      procedure Socket_Taken (Index : Positive);
      --  Used to mark slot associated socket has "taken" by some foreign code.
      --  The server must not close this socket on releasing the slot. It is
      --  used when passing socket to the server push part for example. In the
      --  future it could be used for other functionality over the same
      --  socket, changing HTTP to other protocol for example.

      procedure Prepare_Back (Index : Positive; Possible : out Boolean);
      --  Test and prepare to put socket back into acceptor.
      --  If Possible value become False, it is not possible to
      --  back socket into acceptor because socket already in shutdown process.

      function Is_Abortable (Index : Positive) return Boolean;
      --  Return True when slot can be aborted due to "forced" timeouts

      procedure Abort_On_Timeout
        (Socket : out Socket_Access; Index : in out Positive);
      --  Get the socket pointer from slot where timeout exceeded.
      --  Index IN value is the current slot number, to avoid current slot
      --  abortion.
      --  Index OUT value is the slot number to abort.
      --  Return null if no such sockets.

      function Free_Slots return Natural;
      --  Returns number of free slots

      procedure Set
        (Socket : not null access Net.Socket_Type'Class;
         Index  : Positive);
      --  Mark slot at position Index to be used. This slot will be associated
      --  with Socket. Phase set to Wait_For_Client.

      procedure Get_For_Shutdown
        (Index : Positive; Socket : out Socket_Access);
      --  Get socket from the slot for shutdown, Slot phase is set to
      --  In_Shutdown.

      procedure Shutdown_Done (Index : Positive);
      --  Called when Shutdown is complete, Slot phase is set to Aborted

      entry Release (Index : Positive; Shutdown : out Boolean);
      --  Release slot number Index. Slot phase is set to Closed.
      --  Set the shutdown flag to True is called task have to
      --  shutdown and free the socket.
      --  Note that calling task is only the Line task,
      --  so the socket need to be shutdown is in the stack on the Line task.

      function Get (Index : Positive) return Slot;
      --  Returns Slot data

      function Phase (Index : Positive) return Slot_Phase;
      --  Returns Slot phase

      function Get_Socket_Info (Index : Positive) return Socket_Data;
      --  Returns information about socket (FD and Peername) associated with
      --  slot Index. If the socket is not opened returns
      --  (FD => 0, Peername => "-")

      function Get_Peername (Index : Positive) return String;
      --  Returns the peername for socket at position Index

      procedure Increment_Slot_Activity_Counter
        (Index : Positive; Free_Slots : out Natural);
      --  Add 1 to the slot activity. This is the total number of request
      --  handled by the slot.
      --  Returns number of Free slots in the Free_Slot out parameter, it is
      --  necessary information for Server line task to determine is it
      --  necessary to call Force_Clean and to determine keep-alive
      --  availability.

      procedure Set_Timeouts
        (Phase_Timeouts : Timeouts_Array;
         Data_Timeouts  : Data_Timeouts_Array);
      --  Setup timeouts for slots before starting

   private

      Timeouts      : Timeouts_Array;
      Data_Timeouts : Data_Timeouts_Array;

      Table : Slot_Set (1 .. N);
      Count : Natural := N;

      Shutdown_Count : Natural := 0;
      Last_Force     : Calendar.Time := Calendar.Clock;

   end Slots;

   type Slots_Access is access Slots;

   ----------
   -- Line --
   ----------

   task type Line (Priority : System.Any_Priority; Stack_Size : Integer)
     with Priority     => Priority,
          Storage_Size => Stack_Size
   is
      entry Start (Server : HTTP; Index : Positive);
   end Line;

   type Line_Access is access Line;

   type Line_Set is array (Positive range <>) of Line_Access;

   type Line_Set_Access is access Line_Set;

   ----------
   -- HTTP --
   ----------

   type HTTP is new Ada.Finalization.Limited_Controlled with record
      Self               : not null access HTTP := HTTP'Unchecked_Access;
      --  Point to the record

      Start_Time         : Calendar.Time;
      --  Date and Time when server was started

      Shutdown           : Boolean := True;
      --  True when server is shutdown. This will be set to False when server
      --  will be started.

      Acceptor           : Net.Acceptors.Acceptor_Type;
      --  This is the socket set where server socket and keep alive sockets
      --  waiting for read availability.

      Properties         : CNF.Object := CNF.Get_Current;
      --  All server properties controled by the configuration file

      Log                : AWS.Log.Object;
      --  Logging support

      Error_Log          : aliased AWS.Log.Object;
      --  Error loggin support

      Dispatcher         : Dispatchers.Handler_Class_Access;
      --  Dispatcher for the user actions

      New_Dispatcher     : Dispatchers.Handler_Class_Access;
      --  New dispatcher set in a user's callback, if set this dispatcher will
      --  replace the main dispatcher above.

      Dispatcher_Sem     : Utils.RW_Semaphore (Writers => 1);
      --  RW semaphore to be able to change dynamically the Dispatcher object

      Filters            : Hotplug.Filter_Set;
      --  Hotplug filters are recorded here

      Lines              : Line_Set_Access;
      --  The tasks doing the job

      Slots              : Slots_Access;
      --  Information about each tasks above. This is a protected object to
      --  support concurrency.

      Exception_Handler  : Exceptions.Unexpected_Exception_Handler
         := Default_Unexpected_Exception_Handler'Access;
      --  Exception handle used for unexpected errors found on the server
      --  implementation.

      SSL_Config         : aliased Net.SSL.Config;
   end record;

   overriding procedure Finalize (Web_Server : in out HTTP);

   type Line_Attribute_Record is record
      Server     : access HTTP;
      Line       : Positive;
      Stat       : Status.Data;
      Expect_100 : Boolean := False;
      Skip_Log   : Boolean := False;
      Log_Data   : Log.Fields_Table;
   end record;

   package Line_Attribute is new Ada.Task_Attributes
     (Line_Attribute_Record, (Line => 1, others => <>));
   --  A line specific attribute

   procedure Null_Procedure is null;

   First_Server : not null access procedure := Null_Procedure'Access;
   --  This procedure calls on first server started

   No_Servers   : not null access procedure := Null_Procedure'Access;
   --  This procedure calls on last server shutdown

end AWS.Server;
