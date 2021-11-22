------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2021, AdaCore                     --
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

with Ada.Streams;
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;

with AWS.Config.Set;
with AWS.Dispatchers.Callback;
with AWS.Messages;
with AWS.MIME;
with AWS.Net.Buffered;
with AWS.Net.WebSocket.Registry.Control;
with AWS.Server.HTTP_Utils;
with AWS.Server.Log;
with AWS.Services.Transient_Pages.Control;
with AWS.Session.Control;
with AWS.Status.Set;
with AWS.Status.Translate_Set;
with AWS.Templates;

package body AWS.Server is

   use type Net.Socket_Access;

   procedure Start
     (Web_Server : in out HTTP;
      Dispatcher : Dispatchers.Handler'Class);
   --  Start web server with current configuration

   procedure Protocol_Handler (LA : in out Line_Attribute_Record);
   --  Handle the lines, this is where all the HTTP/1.1 protocol is defined

   procedure Protocol_Handler_V2
     (LA            : in out Line_Attribute_Record;
      Will_Close    : out Boolean;
      HTTP2_Ref     : HTTP2_Context_References.Ref;
      Check_Preface : Boolean := True);
   --  Handle the lines, this is where all the HTTP/2 protocol is defined

   function Next_Request_Or_Give_Back
     (LA     : in out Line_Attribute_Record;
      Sock   : Socket_Access;
      H2_Ref : HTTP2_Context_References.Ref) return Boolean;
   --  Return True if there is something in socket read buffer

   function Accept_Socket_Serialized
     (Server : not null access HTTP;
      Data   : out HTTP2_Context_References.Ref)
      return not null Net.Socket_Access;
   --  Do a protected accept on the HTTP socket. It is not safe to call
   --  multiple accept on the same socket on some platforms.

   procedure Force_Clean (Web_Server : in out HTTP);
   --  Close a socket on a slot for which a force timeout has expired

   Server_Counter : Utils.Counter (Initial_Value => 0);

   ------------------------------
   -- Accept_Socket_Serialized --
   ------------------------------

   function Accept_Socket_Serialized
     (Server : not null access HTTP;
      Data   : out HTTP2_Context_References.Ref)
      return not null Net.Socket_Access
   is
      New_Socket : Net.Socket_Access;

      procedure Accept_Error (E : Ada.Exceptions.Exception_Occurrence);

      ------------------
      -- Accept_Error --
      ------------------

      procedure Accept_Error (E : Ada.Exceptions.Exception_Occurrence) is
      begin
         AWS.Log.Write
           (Server.Error_Log,
            "Accept error "
            & Utils.CRLF_2_Spaces (Ada.Exceptions.Exception_Information (E)));

         Force_Clean (Server.all);
      end Accept_Error;

   begin
      loop
         HTTP_Acceptors.Get
           (Server.Acceptor, New_Socket, Data, Accept_Error'Access);

         if CNF.Security (Server.Properties)
           and then not New_Socket.Is_Secure
         then
            declare
               SSL_Socket : Net.SSL.Socket_Type;
            begin
               SSL_Socket := Net.SSL.Secure_Server
                 (New_Socket.all, Server.SSL_Config);
               Net.Free (New_Socket);
               SSL_Socket.Do_Handshake; -- Handshake need for HTTP/2 ALPN
               pragma Warnings (Off);
               return new Net.SSL.Socket_Type'(SSL_Socket);
            exception
               when Net.Socket_Error =>
                  if New_Socket = null then
                     --  It mean error in SSL handshake, shutdown socket and
                     --  get another one in next iteration.

                     SSL_Socket.Shutdown;

                  else
                     --  Unexpected error

                     Net.Shutdown (New_Socket.all);
                     Net.Free (New_Socket);
                     raise;
                  end if;

               when others =>
                  Net.Shutdown (New_Socket.all);
                  Net.Free (New_Socket);
                  raise;
            end;

         else
            return New_Socket;
         end if;
      end loop;
   end Accept_Socket_Serialized;

   -------------------
   -- Add_Listening --
   -------------------

   procedure Add_Listening
     (Web_Server    : in out HTTP;
      Host          : String;
      Port          : Natural;
      Family        : Net.Family_Type := Net.Family_Unspec;
      Reuse_Address : Boolean         := False;
      IPv6_Only     : Boolean         := False) is
   begin
      HTTP_Acceptors.Add_Listening
        (Web_Server.Acceptor, Host, Port, Family, IPv6_Only => IPv6_Only,
         Reuse_Address => Reuse_Address);
   end Add_Listening;

   ------------
   -- Config --
   ------------

   function Config (Web_Server : HTTP) return AWS.Config.Object is
   begin
      return Web_Server.Properties;
   end Config;

   ------------------------------------------
   -- Default_Unexpected_Exception_Handler --
   ------------------------------------------

   procedure Default_Unexpected_Exception_Handler
     (E      : Exception_Occurrence;
      Log    : in out AWS.Log.Object;
      Error  : Exceptions.Data;
      Answer : in out Response.Data)
   is
      pragma Unreferenced (Log);

      use type Templates.Translate_Set;

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
         if Utils.Is_Regular_File (Fatal_Error_Template) then
            Answer := Response.Build
              (MIME.Text_HTML,
               String'(Templates.Parse
                         (Fatal_Error_Template,
                          Status.Translate_Set (Error.Request)
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

   overriding procedure Finalize (Web_Server : in out HTTP) is
   begin
      Shutdown (Web_Server);
   end Finalize;

   -----------------
   -- Force_Clean --
   -----------------

   procedure Force_Clean (Web_Server : in out HTTP) is
      Socket : Socket_Access;
      Slot   : Positive := Line_Attribute.Reference.Line;
      --  Initialize the slot by the current slot number to avoid current slot
      --  abortion.
   begin
      Web_Server.Slots.Abort_On_Timeout (Socket, Slot);
      if Socket /= null then
         Net.Shutdown (Socket.all);
         Web_Server.Slots.Shutdown_Done (Slot);
      end if;
   end Force_Clean;

   -----------------
   -- Get_Current --
   -----------------

   function Get_Current return not null access HTTP is
   begin
      return Line_Attribute.Reference.Server;
   end Get_Current;

   ----------------------
   -- Get_Message_Body --
   ----------------------

   procedure Get_Message_Body is
      use type Ada.Streams.Stream_Element_Count;
      TA : constant Line_Attribute.Attribute_Handle :=
             Line_Attribute.Reference;
   begin
      if not Status.Is_Body_Uploaded (TA.Stat.all)
        and then
          (Status.Content_Length (TA.Stat.all) > 0
           or else Status.Transfer_Encoding (TA.Stat.all) = "chunked")
      then
         HTTP_Utils.Get_Message_Data
           (TA.Server.all, TA.Line, TA.Stat.all, TA.Expect_100);
      end if;
   end Get_Message_Body;

   ----------------
   -- Get_Status --
   ----------------

   function Get_Status return Status.Data is
   begin
      return Line_Attribute.Reference.Stat.all;
   end Get_Status;

   ----------------------
   -- Give_Back_Socket --
   ----------------------

   procedure Give_Back_Socket
     (Web_Server : in out HTTP;
      Socket     : not null Net.Socket_Access) is
   begin
      HTTP_Acceptors.Give_Back (Web_Server.Acceptor, Socket);
   end Give_Back_Socket;

   procedure Give_Back_Socket
     (Web_Server : in out HTTP; Socket : Net.Socket_Type'Class) is
   begin
      Give_Back_Socket (Web_Server, new Net.Socket_Type'Class'(Socket));
   end Give_Back_Socket;

   ----------
   -- Line --
   ----------

   task body Line is
      TA : constant Line_Attribute.Attribute_Handle :=
             Line_Attribute.Reference;
      Request : aliased AWS.Status.Data;
   begin

      select
         accept Start
           (Server : HTTP;
            Index  : Positive)
         do
            TA.Server := Server.Self;
            TA.Line   := Index;
         end Start;
      or
         terminate;
      end select;

      --  Real job start here, we will exit only if there is an unrecoverable
      --  problem.

      while not TA.Server.Shutdown loop
         declare
            --  Wait for an incoming connection. Each call for the same server
            --  is serialized as some platforms do not handle properly
            --  multiple accepts on the same socket.

            HTTP2_Ref     : HTTP2_Context_References.Ref;
            Socket        : Net.Socket_Access :=
                              Accept_Socket_Serialized (TA.Server, HTTP2_Ref);
            Need_Shutdown : Boolean;
            Will_Close    : Boolean;
         begin
            TA.Stat := Request'Unchecked_Access;

            if CNF.Send_Buffer_Size (TA.Server.Config) /= 0 then
               Net.Set_Send_Buffer_Size
                 (Socket.all, CNF.Send_Buffer_Size (TA.Server.Config));
            end if;

            Net.Set_No_Delay (Socket.all, CNF.TCP_No_Delay (TA.Server.Config));

            TA.Server.Slots.Set (Socket, TA.Line);

            if not HTTP2_Ref.Is_Null then
               AWS.Status.Set.Protocol (Request, AWS.Status.H2);
               Protocol_Handler_V2
                 (TA.all, Will_Close, HTTP2_Ref, Check_Preface => False);

            elsif Socket.Is_Secure
              and then Net.SSL.Socket_Type (Socket.all).ALPN_Get
                       = Messages.H2_Token
              and then CNF.HTTP2_Activated (TA.Server.Config)
            then
               --  Protocol is secure H2
               AWS.Status.Set.Protocol (Request, AWS.Status.H2);
               Protocol_Handler_V2 (TA.all, Will_Close, Null_H2_Ref);

            else
               Protocol_Handler (TA.all);
               Will_Close := False;
            end if;

            TA.Server.Slots.Release (TA.Line, Need_Shutdown);

            if Need_Shutdown then
               Socket.Shutdown;

               --  Don't use Socket.Free, it does not deallocate Socket

               Net.Free (Socket);

            elsif Will_Close then
               TA.Server.Slots.Get_For_Shutdown (TA.Line, Socket);

               if Socket /= null then
                  Socket.Shutdown;
                  TA.Server.Slots.Shutdown_Done (TA.Line);
               end if;
            end if;
         end;
      end loop;

   exception
      when E : others =>
         if not TA.Server.Shutdown then
            declare
               Answer : Response.Data;
            begin
               Answer := Response.Build
                 (Status_Code  => Messages.S500,
                  Content_Type => "text/plain",
                  Message_Body => Exception_Message (E));

               AWS.Log.Write
                 (TA.Server.Error_Log,
                  "Dead slot " & Utils.Image (TA.Line) & ' '
                    & Utils.CRLF_2_Spaces (Exception_Information (E)));

               TA.Server.Exception_Handler
                 (E, TA.Server.Error_Log, (True, TA.Line, Request), Answer);
            end;
         end if;
   end Line;

   ----------------
   -- Line_Tasks --
   ----------------

   function Line_Tasks (Web_Server : HTTP) return Task_Id_Array is
      Result : Task_Id_Array (Web_Server.Lines'Range);
   begin
      for J in Result'Range loop
         Result (J) := Web_Server.Lines (J)'Identity;
      end loop;

      return Result;
   end Line_Tasks;

   -------------------------------
   -- Next_Request_Or_Give_Back --
   -------------------------------

   function Next_Request_Or_Give_Back
     (LA     : in out Line_Attribute_Record;
      Sock   : Socket_Access;
      H2_Ref : HTTP2_Context_References.Ref) return Boolean
   is
      use type AWS.Status.Protocol_State;
      use type Ada.Streams.Stream_Element_Count;
      Back_OK    : Boolean;
      Waiter     : FD_Set_Access;
      Wait_Count : Natural;
      Wait       : constant Boolean := LA.Server.Slots.Free_Slots >=
                     CNF.Free_Slots_Keep_Alive_Limit (LA.Server.Properties);
   begin
      --  If there are few slots available and we have many of them, try to
      --  release one of them.

      if LA.Server.Slots.N > 1 and then not Wait then
         Force_Clean (LA.Server.all);
      end if;

      if Net.Buffered.Pending (Sock.all) > 0 then
         return True;
      end if;

      if Wait then
         LA.Server.Slots.Get_Wait_For_Data (LA.Line, Waiter);

         Waiter.Wait
           (CNF.Cleaner_Client_Header_Timeout (LA.Server.Properties),
            Wait_Count);
         LA.Server.Slots.Mark_Phase (LA.Line, Client_Header);

         if Wait_Count > 0 and then Waiter.Status (2) (Net.Input) then
            return True;
         end if;

      elsif Sock.Check ((Net.Input => True, others => False)) (Net.Input) then
         return True;
      end if;

      LA.Server.Slots.Prepare_Back (LA.Line, Back_OK);

      if Back_OK then
         HTTP_Acceptors.Give_Back
           (LA.Server.Acceptor, Sock, H2_Ref, Success => Back_OK);

         if not Back_OK then
            AWS.Log.Write
              (LA.Server.Error_Log,
               "Could not put socket back into acceptor, line"
               & LA.Line'Img);

            Sock.Shutdown;
         end if;
      end if;

      return False;
   end Next_Request_Or_Give_Back;

   --------------------------
   -- Session_Private_Name --
   --------------------------

   function Session_Private_Name return String is
   begin
      return CNF.Session_Private_Name (Server.Get_Current.Config);
   end Session_Private_Name;

   ----------------------
   -- Protocol_Handler --
   ----------------------

   procedure Protocol_Handler (LA : in out Line_Attribute_Record) is separate;

   procedure Protocol_Handler_V2
     (LA            : in out Line_Attribute_Record;
      Will_Close    : out Boolean;
      HTTP2_Ref     : HTTP2_Context_References.Ref;
      Check_Preface : Boolean := True) is separate;

   ------------------
   -- Session_Name --
   ------------------

   function Session_Name return String is
   begin
      return CNF.Session_Name (Server.Get_Current.Config);
   end Session_Name;

   ---------
   -- Set --
   ---------

   procedure Set
     (Web_Server : in out HTTP;
      Dispatcher : Dispatchers.Handler'Class) is
   begin
      Dispatchers.Free (Web_Server.New_Dispatcher);
      Web_Server.New_Dispatcher :=
        new Dispatchers.Handler'Class'
          (Dispatchers.Handler'Class (Dispatcher.Clone));
   end Set;

   ---------------
   -- Set_Field --
   ---------------

   procedure Set_Field (Id, Value : String) is
      Task_Ptr : constant Line_Attribute.Attribute_Handle :=
                   Line_Attribute.Reference;
   begin
      AWS.Log.Set_Field (Task_Ptr.Server.Log, Task_Ptr.Log_Data, Id, Value);
   end Set_Field;

   ------------------
   -- Set_Security --
   ------------------

   procedure Set_Security
     (Web_Server           : in out HTTP;
      Certificate_Filename : String;
      Security_Mode        : Net.SSL.Method := Net.SSL.TLS_Server;
      Key_Filename         : String         := "") is
   begin
      CNF.Set.Certificate (Web_Server.Properties, Certificate_Filename);

      if Key_Filename = "" then
         CNF.Set.Key (Web_Server.Properties, Certificate_Filename);
      else
         CNF.Set.Key (Web_Server.Properties, Key_Filename);
      end if;

      CNF.Set.Security_Mode
        (Web_Server.Properties, Net.SSL.Method'Image (Security_Mode));
   end Set_Security;

   ----------------------------
   -- Set_Socket_Constructor --
   ----------------------------

   procedure Set_Socket_Constructor
     (Web_Server         : in out HTTP;
      Socket_Constructor : Net.Socket_Constructor) is
   begin
      HTTP_Acceptors.Set_Socket_Constructor
        (Web_Server.Acceptor, Socket_Constructor);
   end Set_Socket_Constructor;

   --------------------
   -- Set_SSL_Config --
   --------------------

   procedure Set_SSL_Config
     (Web_Server : in out HTTP; SSL_Config : Net.SSL.Config) is
   begin
      Web_Server.SSL_Config := SSL_Config;

      if CNF.HTTP2_Activated (Web_Server.Properties) then
         Net.SSL.ALPN_Include (Web_Server.SSL_Config, Messages.H2_Token);
      end if;
   end Set_SSL_Config;

   --------------------------------------
   -- Set_Unexpected_Exception_Handler --
   --------------------------------------

   procedure Set_Unexpected_Exception_Handler
     (Web_Server : in out HTTP;
      Handler    : Exceptions.Unexpected_Exception_Handler) is
   begin
      if Web_Server.Shutdown then
         Web_Server.Exception_Handler := Handler;
      else
         raise Constraint_Error
           with "Could not change exception handler on the active server.";
      end if;
   end Set_Unexpected_Exception_Handler;

   --------------
   -- Shutdown --
   --------------

   procedure Shutdown (Web_Server : in out HTTP) is

      procedure Unchecked_Free is
        new Unchecked_Deallocation (Line_Set, Line_Set_Access);
      procedure Unchecked_Free is
        new Unchecked_Deallocation (Slots, Slots_Access);
      procedure Unchecked_Free is
        new Unchecked_Deallocation (Line, Line_Access);

      All_Lines_Terminated : Boolean := False;
      Slot_State           : Slot_Phase := Closed;
      Slot_Index           : Positive := Positive'Last;
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
      --  Net.Acceptors doing shutdown in the accepting task,
      --  Net.Acceptors.Shutdown only sending command into task where the
      --  waiting for accept is.

      HTTP_Acceptors.Shutdown (Web_Server.Acceptor);

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
               Slot_State := Web_Server.Slots.Phase (K);
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

      --  Release lines and slots memory

      for K in Web_Server.Lines'Range loop
         Unchecked_Free (Web_Server.Lines (K));
      end loop;

      Unchecked_Free (Web_Server.Lines);

      Unchecked_Free (Web_Server.Slots);

      Dispatchers.Free (Web_Server.Dispatcher);

      --  Release the session server if needed

      if CNF.Session (Web_Server.Properties) then
         Session.Control.Shutdown;
      end if;

      Services.Transient_Pages.Control.Shutdown;

      Net.WebSocket.Registry.Control.Shutdown;

      if CNF.Security (Web_Server.Properties) then
         Net.SSL.Release (Web_Server.SSL_Config);
      end if;

      --  Close logs, this ensure that all data will be written to the file

      Log.Stop (Web_Server);

      Log.Stop_Error (Web_Server);

      --  Server removed

      Server_Counter.Decrement (Value => Wait_Counter);

      if Wait_Counter = 0 then
         No_Servers.all;
      end if;
   end Shutdown;

   ---------------------
   -- Skip_Log_Record --
   ---------------------

   procedure Skip_Log_Record is
   begin
      Line_Attribute.Reference.Skip_Log := True;
   end Skip_Log_Record;

   -----------
   -- Slots --
   -----------

   protected body Slots is

      ----------------------
      -- Abort_On_Timeout --
      ----------------------

      procedure Abort_On_Timeout
        (Socket : out Socket_Access; Index : in out Positive)
      is
         use Ada.Calendar;
         Now : constant Time := Clock;

         function Test_Slot (S : Positive) return Boolean;

         ---------------
         -- Test_Slot --
         ---------------

         function Test_Slot (S : Positive) return Boolean is
         begin
            if Table (S).Phase = Wait_For_Client then
               Table (S).Wait_Breaker.Send ((1 => 0));

            elsif Is_Abortable (S) then
               Get_For_Shutdown (S, Socket);

               if Socket /= null then
                  Index := S;
                  return True;
               end if;
            end if;

            return False;
         end Test_Slot;

      begin
         Socket := null;

         if Now - Last_Force < 0.5 then
            --  Look for force timeout not faster than two times in a second
            return;
         end if;

         Last_Force := Now;

         for S in Index + 1 .. Table'Last loop
            if Test_Slot (S) then
               return;
            end if;
         end loop;

         for S in Table'First .. Index - 1 loop
            if Test_Slot (S) then
               return;
            end if;
         end loop;
      end Abort_On_Timeout;

      ------------------------
      -- Check_Data_Timeout --
      ------------------------

      procedure Check_Data_Timeout (Index : Positive) is
         use Ada.Real_Time;
      begin
         if Clock - Table (Index).Phase_Time_Stamp
           > To_Time_Span (Timeouts (Cleaner, Table (Index).Phase))
         then
            raise Net.Socket_Error;
         end if;
      end Check_Data_Timeout;

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

      function Get (Index : Positive) return Slot is
      begin
         return Table (Index);
      end Get;

      ----------------------
      -- Get_For_Shutdown --
      ----------------------

      procedure Get_For_Shutdown
        (Index : Positive; Socket : out Socket_Access) is
      begin
         if Table (Index).Phase not in Closed .. Aborted then
            Socket := Table (Index).Sock;

            if Socket = null then
               Mark_Phase (Index, Aborted);
            else
               Mark_Phase (Index, In_Shutdown);
               Shutdown_Count := Shutdown_Count + 1;
            end if;
         else
            Socket := null;
         end if;
      end Get_For_Shutdown;

      ------------------
      -- Get_Peername --
      ------------------

      function Get_Peername (Index : Positive) return String is
         Socket : constant Socket_Access := Table (Index).Sock;
      begin
         if Socket = null then
            return "";
         else
            return Net.Peer_Addr (Socket.all);
         end if;
      end Get_Peername;

      ----------------
      -- Get_Socket --
      ----------------

      function Get_Socket (Index : Positive) return Socket_Access is
      begin
         return Table (Index).Sock;
      end Get_Socket;

      ---------------------
      -- Get_Socket_Info --
      ---------------------

      function Get_Socket_Info (Index : Positive) return Socket_Data is
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

      -----------------------
      -- Get_Wait_For_Data --
      -----------------------

      procedure Get_Wait_For_Data
        (Index     : Positive;
         Reference : out not null FD_Set_Access)
      is
         use Ada.Streams;
         S : Slot renames Table (Index);
      begin
         if S.Wait_For_Data.Length = 0 then
            S.Wait_Receiver.Socket_Pair (S.Wait_Breaker);
            S.Wait_Receiver.Set_Timeout (0.0);
            S.Wait_For_Data.Add
              (S.Wait_Receiver.Get_FD, (Net.Input => True, others => False));

            S.Wait_For_Data.Add
              (S.Sock.Get_FD, (Net.Input => True, others => False));
         else
            while S.Wait_Receiver.Pending > 0 loop
               declare
                  Buffer : Stream_Element_Array (1 .. 8);
                  Last   : Ada.Streams.Stream_Element_Offset;
               begin
                  S.Wait_Receiver.Receive (Buffer, Last);
               end;
            end loop;

            S.Wait_For_Data.Replace (2, S.Sock.Get_FD);
         end if;

         S.Phase := Wait_For_Client;

         Reference := S.Wait_For_Data'Access;
      end Get_Wait_For_Data;

      -------------------------------------
      -- Increment_Slot_Activity_Counter --
      -------------------------------------

      procedure Increment_Slot_Activity_Counter
        (Index : Positive; Free_Slots : out Natural) is
      begin
         Table (Index).Slot_Activity_Counter :=
           Table (Index).Slot_Activity_Counter + 1;
         Table (Index).Alive_Counter :=
           Table (Index).Alive_Counter + 1;
         Free_Slots := Count;
      end Increment_Slot_Activity_Counter;

      ------------------
      -- Is_Abortable --
      ------------------

      function Is_Abortable (Index : Positive) return Boolean is
         use Real_Time;
         Phase : constant Slot_Phase := Table (Index).Phase;
      begin
         return Phase in Abortable_Phase
           and then Clock - Table (Index).Phase_Time_Stamp
                    > To_Time_Span (Timeouts (Force, Phase));
      end Is_Abortable;

      ----------------
      -- Mark_Phase --
      ----------------

      procedure Mark_Phase (Index : Positive; Phase : Slot_Phase) is
         Was : constant Slot_Phase := Table (Index).Phase;
         Mode : constant array (Boolean) of Timeout_Mode :=
                  (True => Force, False => Cleaner);
      begin
         --  Check if the Aborted phase happen after socket operation
         --  and before Mark_Phase call.

         if Table (Index).Phase in In_Shutdown .. Aborted
           and then Phase in Wait_For_Client .. Server_Processing
         then
            raise Net.Socket_Error;
         end if;

         pragma Assert
           ((if Table (Index).Phase = Closed then Table (Index).Sock = null
             else True),
            Table (Index).Phase'Img & ' ' & Phase'Img
            & ' ' & Boolean'Image (Table (Index).Sock = null));

         Table (Index).Phase_Time_Stamp := Real_Time.Clock;
         Table (Index).Phase := Phase;

         if Phase in Data_Phase then
            Net.Set_Timeout (Table (Index).Sock.all, Data_Timeouts (Phase));

         elsif Phase in Abortable_Phase then
            Net.Set_Timeout
              (Table (Index).Sock.all, Timeouts (Mode (Count = 0), Phase));

         elsif Phase = Closed then
            Table (Index).Sock := null;
         end if;
      end Mark_Phase;

      -----------
      -- Phase --
      -----------

      function Phase (Index : Positive) return Slot_Phase is
      begin
         return Table (Index).Phase;
      end Phase;

      ------------------
      -- Prepare_Back --
      ------------------

      procedure Prepare_Back (Index : Positive; Possible : out Boolean) is
      begin
         Possible := not (Table (Index).Phase in In_Shutdown .. Aborted);

         if Possible then
            Mark_Phase (Index, Closed);
            Table (Index).Sock := null;
         end if;
      end Prepare_Back;

      -------------
      -- Release --
      -------------

      entry Release
        (Index : Positive; Shutdown : out Boolean) when Shutdown_Count = 0 is
      begin
         pragma Assert (Count < N);
         --  No more release than it is possible

         pragma Assert
           ((Table (Index).Phase = Closed
               and then -- If phase is closed, then Sock must be null
             Table (Index).Sock = null)
            or else -- or phase is not closed
              Table (Index).Phase /= Closed,
            Table (Index).Phase'Img
            & ' ' & Boolean'Image (Table (Index).Sock = null));

         Count := Count + 1;

         Shutdown := False;

         if Table (Index).Phase /= Closed then
            if not Table (Index).Socket_Taken then
               if Table (Index).Phase /= Aborted then
                  --  We have to shutdown socket only if it is not in state:
                  --  In_Shutdow, Aborted or Closed.

                  Shutdown := True;
               else
                  Net.Free (Table (Index).Sock);
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

      procedure Set (Socket : not null Net.Socket_Access; Index : Positive) is
      begin
         pragma Assert (Count > 0);
         Table (Index).Phase            := Client_Header;
         Table (Index).Sock             := Socket;
         Table (Index).Alive_Counter    := 0;
         Table (Index).Alive_Time_Stamp := Ada.Calendar.Clock;
         Table (Index).Activity_Counter := Table (Index).Activity_Counter + 1;
         Count := Count - 1;
      end Set;

      ------------------
      -- Set_Timeouts --
      ------------------

      procedure Set_Timeouts
        (Phase_Timeouts : Timeouts_Array;
         Data_Timeouts  : Data_Timeouts_Array) is
      begin
         Timeouts := Phase_Timeouts;
         Slots.Data_Timeouts := Set_Timeouts.Data_Timeouts;
      end Set_Timeouts;

      -------------------
      -- Shutdown_Done --
      -------------------

      procedure Shutdown_Done (Index : Positive) is
      begin
         if Table (Index).Phase = In_Shutdown then
            Mark_Phase (Index, Aborted);
            Shutdown_Count := Shutdown_Count - 1;
         end if;
      end Shutdown_Done;

      ------------------
      -- Socket_Taken --
      ------------------

      procedure Socket_Taken (Index : Positive) is
      begin
         Table (Index).Socket_Taken := True;
         Table (Index).Sock         := null;
      end Socket_Taken;

   end Slots;

   ------------------
   -- Socket_Taken --
   ------------------

   procedure Socket_Taken is
      TA : constant Line_Attribute.Attribute_Handle :=
             Line_Attribute.Reference;
   begin
      TA.Server.Slots.Socket_Taken (TA.Line);
   end Socket_Taken;

   ----------------
   -- SSL_Config --
   ----------------

   function SSL_Config
     (Web_Server : in out HTTP) return not null access Net.SSL.Config is
   begin
      return Web_Server.SSL_Config'Unchecked_Access;
   end SSL_Config;

   -----------
   -- Start --
   -----------

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
      Line_Stack_Size           : Positive  := Default.Line_Stack_Size) is
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
      Callback   : Response.Callback;
      Config     : AWS.Config.Object) is
   begin
      Web_Server.Properties := Config;
      Start (Web_Server, Dispatchers.Callback.Create (Callback));
   end Start;

   -----------
   -- Start --
   -----------

   procedure Start
     (Web_Server : in out HTTP;
      Dispatcher : Dispatchers.Handler'Class;
      Config     : AWS.Config.Object) is
   begin
      Web_Server.Properties := Config;
      Start (Web_Server, Dispatcher);
   end Start;

   -----------
   -- Start --
   -----------

   procedure Start
     (Web_Server : in out HTTP;
      Dispatcher : Dispatchers.Handler'Class)
   is
      use type Net.SSL.Config;

      Max_Connection : constant Positive :=
                         CNF.Max_Connection (Web_Server.Properties);
      SC : Natural;

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

      if CNF.Security (Web_Server.Properties) then
         if Web_Server.SSL_Config = Net.SSL.Null_Config then
            Net.SSL.Initialize
              (Web_Server.SSL_Config,
               CNF.Certificate (Web_Server.Properties),
               Security_Mode,
               Priorities           =>
                 CNF.Cipher_Priorities (Web_Server.Properties),
               Ticket_Support       =>
                 CNF.TLS_Ticket_Support (Web_Server.Properties),
               Key_Filename         =>
                 CNF.Key (Web_Server.Properties),
               Exchange_Certificate =>
                 CNF.Exchange_Certificate (Web_Server.Properties),
               Certificate_Required =>
                 CNF.Certificate_Required (Web_Server.Properties),
               Trusted_CA_Filename  =>
                 CNF.Trusted_CA (Web_Server.Properties),
               CRL_Filename         =>
                 CNF.CRL_File (Web_Server.Properties),
               Session_Cache_Size   =>
                 CNF.SSL_Session_Cache_Size (Web_Server.Properties));
         end if;

         if CNF.HTTP2_Activated (Web_Server.Properties) then
            Net.SSL.ALPN_Include (Web_Server.SSL_Config, Messages.H2_Token);
         end if;
      end if;

      --  Create the Web Server socket set

      HTTP_Acceptors.Listen
        (Acceptor            => Web_Server.Acceptor,
         Host                => CNF.Server_Host (Web_Server.Properties),
         Port                => CNF.Server_Port (Web_Server.Properties),
         Queue_Size          => CNF.Accept_Queue_Size (Web_Server.Properties),
         Family              =>
           Net.Family_Type'Value (CNF.Protocol_Family (Web_Server.Properties)),
         Timeout             =>
           CNF.Cleaner_Wait_For_Client_Timeout (Web_Server.Properties),
         First_Timeout       =>
           CNF.Cleaner_Client_Header_Timeout (Web_Server.Properties),
         Force_Timeout       =>
           CNF.Force_Wait_For_Client_Timeout (Web_Server.Properties),
         Force_First_Timeout =>
           CNF.Force_Client_Header_Timeout (Web_Server.Properties),
         Force_Length        =>
           CNF.Keep_Alive_Force_Limit (Web_Server.Properties),
         Close_Length        =>
           CNF.Keep_Alive_Close_Limit (Web_Server.Properties),
         Reuse_Address       => CNF.Reuse_Address (Web_Server.Properties),
         IPv6_Only           => CNF.IPv6_Only (Web_Server.Properties));

      --  Clone main dispatcher

      Web_Server.Dispatcher :=
        new Dispatchers.Handler'Class'
          (Dispatchers.Handler'Class (Dispatcher.Clone));

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
        (1 .. Max_Connection => new Line
           (Priority   => CNF.Server_Priority (Web_Server.Properties),
            Stack_Size => CNF.Line_Stack_Size (Web_Server.Properties)));

      --  Set Shutdown to False here since it must be done before starting the
      --  lines.

      Web_Server.Shutdown := False;

      --  Start each connection lines

      for I in 1 .. Max_Connection loop
         Web_Server.Lines (I).Start (Web_Server, I);
      end loop;

      --  Initialize session server

      if CNF.Session (Web_Server.Properties) then
         AWS.Session.Control.Start
           (Session_Check_Interval => CNF.Session_Cleanup_Interval,
            Session_Lifetime       => CNF.Session_Lifetime);
      end if;

      --  Initialize transient service

      Services.Transient_Pages.Control.Register
        (Transient_Check_Interval => CNF.Transient_Cleanup_Interval);

      Server_Counter.Increment (Value => SC);

      if SC = 1 then
         First_Server.all;
      end if;

      --  Activate log if requested

      if CNF.Log_Activated (Web_Server.Properties) then
         Log.Start
           (Web_Server,
            Split_Mode => AWS.Log.Split_Mode'Value
              (CNF.Log_Split_Mode (Web_Server.Properties)));
      end if;

      if CNF.Error_Log_Activated (Web_Server.Properties) then
         Log.Start_Error
           (Web_Server,
            Split_Mode => AWS.Log.Split_Mode'Value
              (CNF.Error_Log_Split_Mode (Web_Server.Properties)));
      end if;
   end Start;

   ----------
   -- Wait --
   ----------

   procedure Wait (Mode : Termination := No_Server) is
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
