------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2006                          --
--                                 AdaCore                                  --
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

with Ada.Task_Attributes;
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;

with AWS.Config.Set;
with AWS.Dispatchers.Callback;
with AWS.Messages;
with AWS.MIME;
with AWS.OS_Lib;
with AWS.Server.Log;
with AWS.Services.Transient_Pages.Control;
with AWS.Session.Control;
with AWS.Status.Translate_Table;
with AWS.Templates;

package body AWS.Server is

   use Ada;
   use type Net.Socket_Access;

   type Data_Access is access all Status.Data;

   type Line_Attribute_Record is record
      Server : HTTP_Access;
      Line   : Positive;
      Data   : Data_Access;
   end record;

   procedure Free is new Ada.Unchecked_Deallocation
     (Dispatchers.Handler'Class, Dispatchers.Handler_Class_Access);

   procedure Start
     (Web_Server : in out HTTP;
      Dispatcher : in     Dispatchers.Handler'Class);
   --  Start web server with current configuration.

   procedure Protocol_Handler
     (HTTP_Server : in out HTTP;
      Index       : in     Positive;
      Keep_Alive  : in     Boolean);
   --  Handle the lines, this is where all the HTTP protocol is defined.
   --  Keep_Alive is True when there is enough slots to enable Keep_Alive
   --  connections.

   function Accept_Socket_Serialized
     (Server : in HTTP_Access)
      return Net.Socket_Type'Class;
   --  Do a protected accept on the HTTP socket. It is not safe to call
   --  multiple accept on the same socket on some platforms.

   Server_Counter : Utils.Counter (Initial_Value => 0);

   package Line_Attribute is
     new Task_Attributes (Line_Attribute_Record, (null, 1, null));
   --  A line specific attribute

   ------------------------------
   -- Accept_Socket_Serialized --
   ------------------------------

   function Accept_Socket_Serialized
     (Server : in HTTP_Access)
      return Net.Socket_Type'Class
   is
      New_Socket      : Net.Socket_Type'Class
        := Server.Socket_Constructor (Security => False);

      Released_Socket : Net.Socket_Access;

      Accepting       : Boolean := False;
      --  Determine either "accept socket" mode or "give back" mode.
      --  Init to False to not Release semaphore in case of exception
      --  in Seize_Or_Socket call.

      procedure Free is new Ada.Unchecked_Deallocation
        (Net.Socket_Type'Class, Net.Socket_Access);

   begin
      Server.Sock_Sem.Seize_Or_Socket (Released_Socket);

      Accepting := Released_Socket = null;

      if Accepting then
         --  No socket was given back to the server, just accept a socket from
         --  the server socket.

         if Server.Shutdown then
            --  The server is being shutdown, raise an exception this will
            --  terminate the line.

            Server.Sock_Sem.Release;
            raise Net.Socket_Error;
         end if;

         Net.Accept_Socket (Server.Sock.all, New_Socket);

         Server.Sock_Sem.Release;

         if CNF.Security (Server.Properties) then
            return Net.SSL.Secure_Server (New_Socket, Server.SSL_Config);
         else
            return New_Socket;
         end if;

      else
         --  A socket was given back to the server, return it

         declare
            Result : Net.Socket_Type'Class := Released_Socket.all;
         begin
            --  We do not call AWS.Net.Free as we do not want to destroy the
            --  socket buffers.

            Free (Released_Socket);
            return Result;
         end;

      end if;

   exception
      when others =>
         if Accepting then
            Server.Sock_Sem.Release;
         end if;

         raise;
   end Accept_Socket_Serialized;

   ------------
   -- Config --
   ------------

   function Config (Web_Server : in HTTP) return AWS.Config.Object is
   begin
      return Web_Server.Properties;
   end Config;

   ------------------------------------------
   -- Default_Unexpected_Exception_Handler --
   ------------------------------------------

   procedure Default_Unexpected_Exception_Handler
     (E      : in     Ada.Exceptions.Exception_Occurrence;
      Log    : in out AWS.Log.Object;
      Error  : in     Exceptions.Data;
      Answer : in out Response.Data)
   is
      use Ada.Exceptions;
      use type Templates.Translate_Table;
      pragma Unreferenced (Log);

      Fatal_Error_Template  : constant String := "500.tmplt";
   begin
      if Error.Fatal then
         Text_IO.Put_Line
           (Text_IO.Current_Error, "Fatal error, slot"
              & Positive'Image (Error.Slot) & " is dead now.");
         Text_IO.New_Line (Text_IO.Current_Error);

         Text_IO.Put_Line
           (Text_IO.Current_Error, Exception_Information (E));

      else
         if AWS.OS_Lib.Is_Regular_File (Fatal_Error_Template) then
            Answer := Response.Build
              (MIME.Text_HTML,
               String'(Templates.Parse
                         (Fatal_Error_Template,
                          Status.Translate_Table (Error.Request)
                            & Templates.Assoc
                                ("EXCEPTION", Exception_Information (E)))),
               Messages.S500);
         else
            Answer := Response.Build
              (MIME.Text_HTML,
               "Internal Server Error.<br>"
                 & "Please, send the following information to the Web "
                 & "Master, thanks.<br><hr><br>"
                 & "<pre>" & Exception_Information (E) & "</pre>"
                 & "<br><hr>",
               Messages.S500);
         end if;
      end if;
   end Default_Unexpected_Exception_Handler;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Web_Server : in out HTTP) is
   begin
      Shutdown (Web_Server);
   end Finalize;

   -----------------
   -- Get_Current --
   -----------------

   function Get_Current return HTTP_Access is
   begin
      return Line_Attribute.Value.Server;
   end Get_Current;

   ----------------
   -- Get_Status --
   ----------------

   function Get_Status return Status.Data is
   begin
      return Line_Attribute.Value.Data.all;
   end Get_Status;

   ----------------------
   -- Give_Back_Socket --
   ----------------------

   procedure Give_Back_Socket
     (Web_Server : in out HTTP;
      Socket     : in     Net.Socket_Type'Class) is
   begin
      Web_Server.Sock_Sem.Put_Socket (new Net.Socket_Type'Class'(Socket));
   end Give_Back_Socket;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Web_Server : in out HTTP) is
      pragma Warnings (Off, Web_Server);
   begin
      null;
   end Initialize;

   ----------
   -- Line --
   ----------

   task body Line is
      HTTP_Server : HTTP_Access;
      Slot_Index  : Positive;
   begin

      select
         accept Start
           (Server : in HTTP;
            Index  : in Positive)
         do
            HTTP_Server := Server.Self;
            Slot_Index  := Index;
         end Start;
      or
         terminate;
      end select;

      --  Real job start here, we will exit only if there is an unrecoverable
      --  problem.

      while not HTTP_Server.Shutdown loop

         declare
            --  Wait for an incoming connection. Each call for the same server
            --  is serialized as some platforms do not handle properly
            --  multiple accepts on the same socket.

            Keep_Alive_Limit : constant Natural
              := CNF.Free_Slots_Keep_Alive_Limit (HTTP_Server.Properties);

            Socket           : aliased Net.Socket_Type'Class
              := Accept_Socket_Serialized (HTTP_Server);

            Free_Slots       : Natural;
            Need_Shutdown    : Boolean;
         begin
            HTTP_Server.Slots.Set
              (Socket'Unchecked_Access, Slot_Index, Free_Slots);

            --  If there is no more slot available and we have many
            --  of them, try to abort one of them.

            if Free_Slots = 0
              and then CNF.Max_Connection (HTTP_Server.Properties) > 1
            then
               select
                  HTTP_Server.Cleaner.Force;
               or
                  delay 4.0;
                  Ada.Text_IO.Put_Line
                    (Text_IO.Current_Error, "Server too busy.");
               end select;
            end if;

            Protocol_Handler
              (HTTP_Server.all, Slot_Index, Free_Slots >= Keep_Alive_Limit);

            HTTP_Server.Slots.Release (Slot_Index, Need_Shutdown);

            if Need_Shutdown then
               Net.Shutdown (Socket);
            end if;
         end;
      end loop;

   exception
      when E : others =>
         if not HTTP_Server.Shutdown then
            declare
               S      : Status.Data;
               pragma Warnings (Off, S);
               Answer : Response.Data;
            begin
               AWS.Log.Write
                 (HTTP_Server.Error_Log,
                  "Dead slot " & Utils.Image (Slot_Index) & ' '
                    & Utils.CRLF_2_Spaces
                        (Ada.Exceptions.Exception_Information (E)));

               HTTP_Server.Exception_Handler
                 (E, HTTP_Server.Error_Log, (True, Slot_Index, S), Answer);
            end;
         end if;
   end Line;

   ------------------
   -- Line_Cleaner --
   ------------------

   task body Line_Cleaner is
      Mode   : Timeout_Mode;
      Socket : Socket_Access;
      Slot   : Positive;
      Quit   : Boolean := False;
   begin
      while not Quit loop
         select
            accept Force do
               Mode := Force;
            end Force;
         or
            accept Shutdown do
               Quit := True;
            end Shutdown;
         or
            delay 30.0;
            Mode := Cleaner;
         end select;

         while not Quit loop
            Server.Slots.Abort_On_Timeout (Mode, Socket, Slot);

            if Socket = null then
               exit when Mode /= Force;
            else
               Net.Shutdown (Socket.all);
               Server.Slots.Shutdown_Done (Slot);
               exit;
            end if;

            select
               accept Force;
            or
               accept Shutdown do
                  Quit := True;
               end Shutdown;
            or
               delay 1.0;
            end select;
         end loop;

      end loop;
   end Line_Cleaner;

   ----------------------
   -- Protocol_Handler --
   ----------------------

   procedure Protocol_Handler
     (HTTP_Server : in out HTTP;
      Index       : in     Positive;
      Keep_Alive  : in     Boolean) is separate;

   ---------
   -- Set --
   ---------

   procedure Set
     (Web_Server : in out HTTP;
      Dispatcher : in     Dispatchers.Handler'Class)
   is
      Old : Dispatchers.Handler_Class_Access := Web_Server.Dispatcher;
   begin
      Web_Server.Dispatcher_Sem.Write;

      Web_Server.Dispatcher := new Dispatchers.Handler'Class'(Dispatcher);

      Web_Server.Dispatcher_Sem.Release_Write;

      Free (Old);
   end Set;

   ------------------
   -- Set_Security --
   ------------------

   procedure Set_Security
     (Web_Server           : in out HTTP;
      Certificate_Filename : in     String;
      Security_Mode        : in     Net.SSL.Method := Net.SSL.SSLv23_Server;
      Key_Filename         : in     String         := "") is
   begin
      AWS.Config.Set.Certificate (Web_Server.Properties, Certificate_Filename);

      if Key_Filename = "" then
         AWS.Config.Set.Key (Web_Server.Properties, Certificate_Filename);
      else
         AWS.Config.Set.Key (Web_Server.Properties, Key_Filename);
      end if;

      AWS.Config.Set.Security_Mode
        (Web_Server.Properties, Net.SSL.Method'Image (Security_Mode));
   end Set_Security;

   ----------------------------
   -- Set_Socket_Constructor --
   ----------------------------

   procedure Set_Socket_Constructor
     (Web_Server         : in out HTTP;
      Socket_Constructor : in     Net.Socket_Constructor) is
   begin
      Web_Server.Socket_Constructor := Socket_Constructor;
   end Set_Socket_Constructor;

   --------------------------------------
   -- Set_Unexpected_Exception_Handler --
   --------------------------------------

   procedure Set_Unexpected_Exception_Handler
     (Web_Server : in out HTTP;
      Handler    : in     Exceptions.Unexpected_Exception_Handler) is
   begin
      if Web_Server.Shutdown then
         Web_Server.Exception_Handler := Handler;
      else
         Ada.Exceptions.Raise_Exception (Constraint_Error'Identity,
            "Could not change exception handler on the active server.");
      end if;
   end Set_Unexpected_Exception_Handler;

   --------------
   -- Shutdown --
   --------------

   procedure Shutdown (Web_Server : in out HTTP) is

      procedure Free is
         new Ada.Unchecked_Deallocation (Line_Cleaner, Line_Cleaner_Access);

      procedure Free is
         new Ada.Unchecked_Deallocation (Line_Set, Line_Set_Access);

      procedure Free is
         new Ada.Unchecked_Deallocation (Slots, Slots_Access);

      procedure Free is
         new Ada.Unchecked_Deallocation (Line, Line_Access);

      All_Lines_Terminated : Boolean := False;
      Slot_State           : Slot_Phase;
      Slot_Index           : Positive;
      Wait_Counter         : Natural := 0;

   begin
      if Web_Server.Shutdown then
         return;
      end if;

      --  Set the current server status to shutdown. This will ensure than no
      --  request will be accepted anymore. All current lines on the
      --  Accept_Socket_Serialized queue will return. This leaves a single
      --  line to handle. See below.

      Web_Server.Shutdown := True;

      --  In the queue, waiting for a connection we have a single line waiting.
      --  We need to unlock this line properly. Closing the server socket
      --  directly is not possible as this line could be waiting on the routine
      --  Accept_Socket or poll (see Wait_For implementation). On some OS (AIX
      --  for example), trying to close a sockets waiting on a select/poll
      --  will lock until the select/poll return or timeout. So the server
      --  termination is a bit tricky and requires some attention.

      declare
         Sock : Net.Socket_Type'Class := Web_Server.Socket_Constructor (False);
      begin
         --  First we want to have the line waiting on the Wait_For for a
         --  connection to exit from the poll and to continue its
         --  execution. For this we connect to the server here.

         Net.Connect
           (Sock, "127.0.0.1", CNF.Server_Port (Web_Server.Properties),
            Wait => False);

         --  At this point the line is waiting for some data. It is possible
         --  to close the server socket, this won't lock anymore.

         Net.Shutdown (Web_Server.Sock.all);

         --  Close the dummy socket to the server

         Net.Shutdown (Sock);
      end;

      --  Release the slots

      for S in 1 .. Web_Server.Slots.N loop
         declare
            Socket : Socket_Access;
         begin
            Web_Server.Slots.Get_For_Shutdown (S, Socket);

            if Socket /= null then
               Net.Shutdown (Socket.all);
               Web_Server.Slots.Shutdown_Done (S);
            end if;

         exception
            when others =>
               Web_Server.Slots.Shutdown_Done (S);
               raise;
         end;
      end loop;

      --  Wait for all lines to be terminated to be able to release associated
      --  memory.

      while not All_Lines_Terminated loop
         All_Lines_Terminated := True;

         for K in Web_Server.Lines'Range loop
            if not Web_Server.Lines (K)'Terminated then
               All_Lines_Terminated := False;

               Slot_Index := K;
               Slot_State := Web_Server.Slots.Get (K).Phase;
            end if;
         end loop;

         delay 0.5;

         Wait_Counter := Wait_Counter + 1;

         if Wait_Counter > 30 then
            Ada.Text_IO.Put_Line
              (Text_IO.Current_Error,
               "Can't terminate all lines. Slot" & Positive'Image (Slot_Index)
               & " in " & Slot_State'Img & " state.");
            exit;
         end if;
      end loop;

      --  Release the cleaner task

      Web_Server.Cleaner.Shutdown;

      --  Wait for Cleaner task to terminate to be able to release associated
      --  memory.

      Wait_Counter := 0;

      while not Web_Server.Cleaner'Terminated loop
         delay 0.5;

         Wait_Counter := Wait_Counter + 1;

         if Wait_Counter > 10 then
            Text_IO.Put_Line
              (Text_IO.Current_Error, "Can't terminate task cleaner.");
            exit;
         end if;
      end loop;

      --  Release lines and slots memory

      for K in Web_Server.Lines'Range loop
         Free (Web_Server.Lines (K));
      end loop;

      Net.Free (Web_Server.Sock);

      Free (Web_Server.Lines);

      Free (Web_Server.Cleaner);

      Free (Web_Server.Slots);

      Free (Web_Server.Dispatcher);

      --  Release the session server if needed

      if CNF.Session (Web_Server.Properties) then
         Session.Control.Shutdown;
      end if;

      Services.Transient_Pages.Control.Shutdown;

      if CNF.Security (Web_Server.Properties) then
         Net.SSL.Release (Web_Server.SSL_Config);
      end if;

      --  Close logs, this ensure that all data will be written to the file.

      Log.Stop (Web_Server);

      Log.Stop_Error (Web_Server);

      --  Server removed

      Server_Counter.Decrement;
   end Shutdown;

   -----------
   -- Slots --
   -----------

   protected body Slots is

      ----------------------
      -- Abort_On_Timeout --
      ----------------------

      procedure Abort_On_Timeout
        (Mode   : in     Timeout_Mode;
         Socket :    out Socket_Access;
         Index  :    out Positive) is
      begin
         for S in Table'Range loop
            if Is_Abortable (S, Mode) then
               Get_For_Shutdown (S, Socket);

               if Socket /= null then
                  Index := S;
                  return;
               end if;
            end if;
         end loop;

         Socket := null;
      end Abort_On_Timeout;

      ----------------
      -- Free_Slots --
      ----------------

      function Free_Slots return Natural is
      begin
         return Count;
      end Free_Slots;

      ---------
      -- Get --
      ---------

      function Get (Index : in Positive) return Slot is
      begin
         return Table (Index);
      end Get;

      ----------------------
      -- Get_For_Shutdown --
      ----------------------

      procedure Get_For_Shutdown
        (Index  : in     Positive;
         Socket :    out Socket_Access) is
      begin
         if Table (Index).Phase not in Closed .. Aborted then
            Mark_Phase (Index, In_Shutdown);
            Shutdown_Count := Shutdown_Count + 1;
            Socket := Table (Index).Sock;
         else
            Socket := null;
         end if;
      end Get_For_Shutdown;

      ------------------
      -- Get_Peername --
      ------------------

      function Get_Peername (Index : in Positive) return String is
         use type Socket_Access;
         Socket : constant Socket_Access := Table (Index).Sock;
      begin
         if Socket = null then
            return "";
         else
            return Net.Peer_Addr (Socket.all);
         end if;
      end Get_Peername;

      ---------------------
      -- Get_Socket_Info --
      ---------------------

      function Get_Socket_Info (Index : in Positive) return Socket_Data is
         use type Socket_Access;
         Socket : constant Socket_Access := Table (Index).Sock;
      begin
         if Socket = null then
            return Socket_Data'
              (Peername_Length => 1, Peername => "-", FD => 0);

         else
            declare
               Peername : constant String := Net.Peer_Addr (Socket.all);
            begin
               return Socket_Data'
                  (Peername_Length => Peername'Length,
                   Peername        => Peername,
                   FD              => Net.Get_FD (Socket.all));
            end;
         end if;
      end Get_Socket_Info;

      -------------------------------------
      -- Increment_Slot_Activity_Counter --
      -------------------------------------

      procedure Increment_Slot_Activity_Counter (Index : in Positive) is
      begin
         Table (Index).Slot_Activity_Counter
           := Table (Index).Slot_Activity_Counter + 1;
         Table (Index).Alive_Counter
           := Table (Index).Alive_Counter + 1;
      end Increment_Slot_Activity_Counter;

      ------------------
      -- Is_Abortable --
      ------------------

      function Is_Abortable
        (Index : in Positive;
         Mode  : in Timeout_Mode)
         return Boolean
      is
         use type Calendar.Time;
         Phase : constant Slot_Phase    := Table (Index).Phase;
         Now   : constant Calendar.Time := Calendar.Clock;
      begin
         return
           (Phase in Abortable_Phase
            and then
            Now - Table (Index).Phase_Time_Stamp > Timeouts (Mode, Phase))

           or else

           (Phase in Data_Phase
            and then
            Now - Table (Index).Data_Time_Stamp > Data_Timeouts (Phase));
      end Is_Abortable;

      --------------------------
      -- Mark_Data_Time_Stamp --
      --------------------------

      procedure Mark_Data_Time_Stamp (Index : in Positive) is
      begin
         Table (Index).Data_Time_Stamp := Ada.Calendar.Clock;
      end Mark_Data_Time_Stamp;

      ----------------
      -- Mark_Phase --
      ----------------

      procedure Mark_Phase (Index : in Positive; Phase : in Slot_Phase) is
      begin
         --  Check if the Aborted phase happen between after socket operation
         --  and before Mark_Phase call.

         if Table (Index).Phase in In_Shutdown .. Aborted
           and then Phase in Wait_For_Client .. Server_Processing
         then
            raise Net.Socket_Error;
         end if;

         Table (Index).Phase_Time_Stamp := Ada.Calendar.Clock;
         Table (Index).Phase := Phase;

         if Phase in Data_Phase then
            Mark_Data_Time_Stamp (Index);
         end if;
      end Mark_Phase;

      -------------
      -- Release --
      -------------

      entry Release
        (Index    : in     Positive;
         Shutdown :    out Boolean)
        when Shutdown_Count = 0
      is
         use type Socket_Access;
      begin
         pragma Assert (Count < N);
         --  No more release than it is possible

         pragma Assert
           ((Table (Index).Phase = Closed
               and then -- If phase is closed, then Sock must be null
               (Table (Index).Sock = null))
            or else -- or phase is not closed
              (Table (Index).Phase /= Closed));

         Count := Count + 1;

         Shutdown := False;

         if Table (Index).Phase /= Closed then
            if not Table (Index).Socket_Taken  then
               if Table (Index).Phase /= Aborted then
                  --  We have to shutdown socket only if it is not in state:
                  --  In_Shutdow, Aborted or Closed.

                  Shutdown := True;
               end if;

            else
               Table (Index).Socket_Taken := False;
            end if;

            Mark_Phase (Index, Closed);
            Table (Index).Sock := null;
         end if;
      end Release;

      ---------
      -- Set --
      ---------

      procedure Set
        (Socket     : in     Socket_Access;
         Index      : in     Positive;
         Free_Slots :    out Natural) is
      begin
         pragma Assert (Count > 0);

         Mark_Phase (Index, Wait_For_Client);
         Table (Index).Sock := Socket;
         Table (Index).Alive_Counter := 0;
         Table (Index).Alive_Time_Stamp := Ada.Calendar.Clock;
         Table (Index).Activity_Counter := Table (Index).Activity_Counter + 1;
         Count := Count - 1;
         Free_Slots := Count;
      end Set;

      ------------------
      -- Set_Timeouts --
      ------------------

      procedure Set_Timeouts
        (Phase_Timeouts : in Timeouts_Array;
         Data_Timeouts  : in Data_Timeouts_Array) is
      begin
         Timeouts := Phase_Timeouts;
         Slots.Data_Timeouts := Set_Timeouts.Data_Timeouts;
      end Set_Timeouts;

      -------------------
      -- Shutdown_Done --
      -------------------

      procedure Shutdown_Done (Index : in Positive) is
      begin
         if Table (Index).Phase = In_Shutdown then
            Mark_Phase (Index, Aborted);
            Shutdown_Count := Shutdown_Count - 1;
         end if;
      end Shutdown_Done;

      ------------------
      -- Socket_Taken --
      ------------------

      procedure Socket_Taken (Index : in Positive; Flag : in Boolean) is
      begin
         Table (Index).Socket_Taken := Flag;
      end Socket_Taken;

   end Slots;

   ----------------------
   -- Socket_Semaphore --
   ----------------------

   protected body Socket_Semaphore is

      ----------------
      -- Put_Socket --
      ----------------

      entry Put_Socket (Socket : in Net.Socket_Access)
        when Size /= Max_Sockets is
      begin
         Size := Size + 1;

         if Last > Max_Sockets then
            Last := Sockets'First;
         end if;

         Sockets (Last) := Socket;

         Last := Last + 1;
      end Put_Socket;

      -------------
      -- Release --
      -------------

      procedure Release is
      begin
         Seized := False;
      end Release;

      ---------------------
      -- Seize_Or_Socket --
      ---------------------

      entry Seize_Or_Socket (Socket : out Net.Socket_Access)
        when not Seized or else Size /= 0 is
      begin
         if not Seized then
            Seized := True;

         else
            Size := Size - 1;

            if Current > Max_Sockets then
               Current := Sockets'First;
            end if;

            Socket := Sockets (Current);

            Current := Current + 1;
         end if;
      end Seize_Or_Socket;

   end Socket_Semaphore;

   ------------------
   -- Socket_Taken --
   ------------------

   procedure Socket_Taken (Flag : in Boolean) is
      TA : constant Line_Attribute.Attribute_Handle
        := Line_Attribute.Reference;
   begin
      TA.Server.Slots.Socket_Taken (TA.Line, Flag);
   end Socket_Taken;

   -----------
   -- Start --
   -----------

   procedure Start
     (Web_Server                : in out HTTP;
      Name                      : in     String;
      Callback                  : in     Response.Callback;
      Max_Connection            : in     Positive  := Default.Max_Connection;
      Admin_URI                 : in     String    := Default.Admin_URI;
      Port                      : in     Positive  := Default.Server_Port;
      Security                  : in     Boolean   := False;
      Session                   : in     Boolean   := False;
      Case_Sensitive_Parameters : in     Boolean   := True;
      Upload_Directory          : in     String    := Default.Upload_Directory;
      Line_Stack_Size           : in     Positive  := Default.Line_Stack_Size)
   is
   begin
      CNF.Set.Server_Name      (Web_Server.Properties, Name);
      CNF.Set.Admin_URI        (Web_Server.Properties, Admin_URI);
      CNF.Set.Server_Port      (Web_Server.Properties, Port);
      CNF.Set.Security         (Web_Server.Properties, Security);
      CNF.Set.Session          (Web_Server.Properties, Session);
      CNF.Set.Upload_Directory (Web_Server.Properties, Upload_Directory);
      CNF.Set.Max_Connection   (Web_Server.Properties, Max_Connection);
      CNF.Set.Line_Stack_Size  (Web_Server.Properties, Line_Stack_Size);

      CNF.Set.Case_Sensitive_Parameters
        (Web_Server.Properties, Case_Sensitive_Parameters);

      Start (Web_Server, Dispatchers.Callback.Create (Callback));
   end Start;

   -----------
   -- Start --
   -----------

   procedure Start
     (Web_Server : in out HTTP;
      Callback   : in     Response.Callback;
      Config     : in     AWS.Config.Object) is
   begin
      Web_Server.Properties := Config;
      Start (Web_Server, Dispatchers.Callback.Create (Callback));
   end Start;

   -----------
   -- Start --
   -----------

   procedure Start
     (Web_Server : in out HTTP;
      Dispatcher : in     Dispatchers.Handler'Class;
      Config     : in     AWS.Config.Object) is
   begin
      Web_Server.Properties := Config;
      Start (Web_Server, Dispatcher);
   end Start;

   -----------
   -- Start --
   -----------

   procedure Start
     (Web_Server : in out HTTP;
      Dispatcher : in     Dispatchers.Handler'Class)
   is
      use type Net.SSL.Config;

      Max_Connection : constant Positive
        := CNF.Max_Connection (Web_Server.Properties);

      function Security_Mode return Net.SSL.Method;
      --  Returns the server security mode, returns the default method if the
      --  current one is not recognized.

      -------------------
      -- Security_Mode --
      -------------------

      function Security_Mode return Net.SSL.Method is
      begin
         return Net.SSL.Method'Value
           (CNF.Security_Mode (Web_Server.Properties));
      exception
         when Constraint_Error =>
            return Net.SSL.Method'Value (Default.Security_Mode);
      end Security_Mode;

   begin
      --  If it is an SSL connection, initialize the SSL library

      if CNF.Security (Web_Server.Properties)
        and then Web_Server.SSL_Config = Net.SSL.Null_Config
      then
         Net.SSL.Initialize
           (Web_Server.SSL_Config,
            CNF.Certificate (Web_Server.Properties),
            Security_Mode,
            CNF.Key (Web_Server.Properties),
            Exchange_Certificate =>
              CNF.Exchange_Certificate ((Web_Server.Properties)));
      end if;

      --  Create the Web Server socket

      Web_Server.Sock := new Net.Socket_Type'Class'
        (Web_Server.Socket_Constructor (Security => False));

      Net.Bind
        (Web_Server.Sock.all,
         CNF.Server_Port (Web_Server.Properties),
         CNF.Server_Host (Web_Server.Properties));

      Net.Listen
        (Web_Server.Sock.all,
         Queue_Size => CNF.Accept_Queue_Size (Web_Server.Properties));

      Web_Server.Dispatcher := new Dispatchers.Handler'Class'(Dispatcher);

      --  Initialize slots

      Web_Server.Slots := new Slots (Max_Connection);

      --  Set timeouts

      Web_Server.Slots.Set_Timeouts
        ((Cleaner => -- Timeouts for Line_Cleaner
            (Wait_For_Client  =>
               CNF.Cleaner_Wait_For_Client_Timeout (Web_Server.Properties),
             Client_Header    =>
               CNF.Cleaner_Client_Header_Timeout (Web_Server.Properties),
             Client_Data      =>
               CNF.Cleaner_Client_Data_Timeout (Web_Server.Properties),
             Server_Response  =>
               CNF.Cleaner_Server_Response_Timeout (Web_Server.Properties)),

          Force   => -- Force timeouts used when there is no free slot
            (Wait_For_Client  =>
               CNF.Force_Wait_For_Client_Timeout (Web_Server.Properties),
             Client_Header    =>
               CNF.Force_Client_Header_Timeout (Web_Server.Properties),
             Client_Data      =>
               CNF.Force_Client_Data_Timeout (Web_Server.Properties),
             Server_Response  =>
               CNF.Force_Server_Response_Timeout (Web_Server.Properties))),

         (Client_Data     =>
            CNF.Receive_Timeout (Web_Server.Properties),
          Server_Response =>
            CNF.Send_Timeout (Web_Server.Properties)));

      --  Started time

      Web_Server.Start_Time := Calendar.Clock;

      --  Initialize the connection lines

      Web_Server.Lines := new Line_Set'
        (1 .. Max_Connection
         => new Line (CNF.Line_Stack_Size (Web_Server.Properties)));

      --  Initialize the cleaner task

      Web_Server.Cleaner := new Line_Cleaner (Web_Server.Self);

      --  Set Shutdown to False here since it must be done before starting the
      --  lines.

      Web_Server.Shutdown := False;

      --  Start each connection lines

      for I in 1 .. Max_Connection loop
         Web_Server.Lines (I).Start (Web_Server, I);
      end loop;

      --  Initialize session server

      if AWS.Config.Session (Web_Server.Properties) then
         AWS.Session.Control.Start
           (Session_Check_Interval => CNF.Session_Cleanup_Interval,
            Session_Lifetime       => CNF.Session_Lifetime);
      end if;

      --  Initialize transient service

      Services.Transient_Pages.Control.Register
        (Transient_Check_Interval => CNF.Transient_Cleanup_Interval);

      Server_Counter.Increment;
   end Start;

   ----------
   -- Wait --
   ----------

   procedure Wait (Mode : in Termination := No_Server) is
   begin
      case Mode is
         when No_Server =>
            Server_Counter.Zero;

         when Q_Key_Pressed =>
            declare
               K : Character;
            begin
               loop
                  Text_IO.Get_Immediate (K);
                  exit when K = 'q' or else K = 'Q';
               end loop;
            end;

         when Forever =>
            loop
               delay Duration'Last;
            end loop;
      end case;
   end Wait;

end AWS.Server;
