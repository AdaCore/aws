------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2001                          --
--                                ACT-Europe                                --
--                                                                          --
--  Authors: Dmitriy Anisimov - Pascal Obry                                 --
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
with Ada.Exceptions;
with Ada.Text_IO;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with Interfaces.C;

with Sockets.Thin;
with Sockets.Naming;
with AWS.Config.Set;

with AWS.Net;
with AWS.Session.Control;

package body AWS.Server is

   use Ada;

   protected File_Upload_UID is
      procedure Get (ID : out Natural);
      --  returns a UID for file upload. This is to ensure that files
      --  coming from clients will always have different name.
   private
      UID : Natural := 0;
   end File_Upload_UID;

   procedure Start
     (Web_Server : in out HTTP;
      Callback   : in     Response.Callback);
   --  Start web server with current configuration

   procedure Protocol_Handler
     (Sock        : in     Sockets.Socket_FD'Class;
      HTTP_Server : in out HTTP;
      Index       : in     Positive);
   --  Handle the lines, this is where all the HTTP protocol is defined.

   ------------
   -- Config --
   ------------

   function Config (Web_Server : in HTTP) return AWS.Config.Object is
   begin
      return Web_Server.Properties;
   end Config;

   ---------------------
   -- File_Upload_UID --
   ---------------------

   protected body File_Upload_UID is

      procedure Get (ID : out Natural) is
      begin
         ID := UID;
         UID := UID + 1;
      end Get;

   end File_Upload_UID;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Web_Server : in out HTTP) is
   begin
      Shutdown (Web_Server);
   end Finalize;

   ----------
   -- Line --
   ----------

   task body Line is

      HTTP_Server : HTTP_Access;
      Slot_Index  : Positive;

      function Get_Peername (Sock : in Sockets.Socket_FD) return String;
      --  Returns the peername for Sock.

      function Get_Peername (Sock : in Sockets.Socket_FD)
         return String
      is
         package C renames Interfaces.C;
         use type C.int;
         use Sockets;

         Sockaddr    : aliased Thin.Sockaddr;
         Sockaddr_In : Thin.Sockaddr_In;

         function To_Sockaddr_In is new
           Ada.Unchecked_Conversion (Thin.Sockaddr, Thin.Sockaddr_In);

         Len      : aliased C.int := Thin.Sockaddr'Size / 8;
         Result   : C.int;
      begin
         Result := Sockets.Thin.C_Getpeername (Sockets.Get_FD (Sock),
                                               Sockaddr'Address,
                                               Len'Unchecked_Access);

         Sockaddr_In := To_Sockaddr_In (Sockaddr);

         return Sockets.Naming.Image (Sockaddr_In.Sin_Addr);
      end Get_Peername;

   begin

      select
         accept Start (Server : HTTP;
                       Index  : Positive)
         do
            HTTP_Server := Server.Self;
            Slot_Index  := Index;
         end Start;
      or
         terminate;
      end select;

      loop
         declare
            --  Wait for an incoming connection.

            Sock : aliased Sockets.Socket_FD'Class :=
              AWS.Net.Accept_Socket
              (HTTP_Server.Sock,
               CNF.Security (HTTP_Server.Properties));

         begin
            begin
               --  If there is only one more slot available and we have many
               --  of them, try to abort one of them.

               if HTTP_Server.Slots.Free_Slots = 1
                 and then CNF.Max_Connection (HTTP_Server.Properties) > 1
               then
                  HTTP_Server.Cleaner.Force;
               end if;

               HTTP_Server.Slots.Get (Sockets.Socket_FD (Sock), Slot_Index);

               HTTP_Server.Slots.Set_Peername
                 (Slot_Index,
                  Get_Peername (Sockets.Socket_FD (Sock)));

               Protocol_Handler (Sock, HTTP_Server.all, Slot_Index);

            exception

               --  We must never exit from the outer loop as a Line task is
               --  supposed to live forever.
               --  We have here a pool of Line and each line is recycled when
               --  needed.

               when Sockets.Connection_Closed
                 | Connection_Error
                 | Constraint_Error =>
                  --  ??? Constraint_Error should be removed at some
                  --  point. This is just because AdaSockets Send raises a
                  --  Constraint_Error when a connection was closed while
                  --  sending data.
                  null;

               when E : others =>
                  Text_IO.Put_Line ("A problem has been detected!");
                  Text_IO.Put_Line ("Connection will be closed...");
                  Text_IO.New_Line;
                  Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
            end;

            HTTP_Server.Slots.Release (Slot_Index);

         end;
      end loop;

   exception

      when E : others =>

         if not HTTP_Server.Shutdown then
            Text_IO.Put_Line
              (Text_IO.Current_Error,
               "Slot problem has been detected!");

            Text_IO.Put_Line
              (Text_IO.Current_Error,
               Ada.Exceptions.Exception_Information (E));
         end if;

   end Line;

   ------------------
   -- Line_Cleaner --
   ------------------

   task body Line_Cleaner is
      Mode : Timeout_Mode;
      Done : Boolean := False;
   begin
      loop
         select
            accept Force do
               Mode := Force;
            end Force;
         or
            delay 30.0;
            Mode := Cleaner;
         end select;

         loop
            Server.Slots.Abort_On_Timeout (Mode, Done);
            exit when Mode /= Force or else Done;
            delay 1.0;
         end loop;

      end loop;
   end Line_Cleaner;

   ----------------------
   -- Protocol_Handler --
   ----------------------

   procedure Protocol_Handler
     (Sock        : in     Sockets.Socket_FD'Class;
      HTTP_Server : in out HTTP;
      Index       : in     Positive) is separate;

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

   begin
      if Web_Server.Shutdown then
         return;
      end if;

      Web_Server.Shutdown := True;

      --  First, close the sever socket, so no more request will be queued

      Sockets.Shutdown (Web_Server.Sock);

      --  Release the cleaner task

      abort Web_Server.Cleaner.all;
      Free (Web_Server.Cleaner);

      --  Release lines

      Free (Web_Server.Lines);

      --  Release the slots

      for S in 1 .. CNF.Max_Connection (Web_Server.Properties) loop
         Web_Server.Slots.Release (S);
      end loop;

      Free (Web_Server.Slots);

      --  Release the session server if needed

      if CNF.Session (Web_Server.Properties) then
         Session.Control.Shutdown;
      end if;
   end Shutdown;

   -----------
   -- Slots --
   -----------

   protected body Slots is

      ------------------
      -- Set_Peername --
      ------------------

      procedure Set_Peername (Index : in Positive; Peername : in String) is
      begin
         Set (Index).Peername := To_Unbounded_String (Peername);
      end Set_Peername;

      ----------------
      -- Mark_Phase --
      ----------------

      procedure Mark_Phase (Index : in Positive; Phase : in Slot_Phase) is
      begin
         Set (Index).Phase_Time_Stamp := Ada.Calendar.Clock;
         Set (Index).Phase := Phase;

         if Phase in Data_Phase then
            Mark_Data_Time_Stamp (Index);
         end if;
      end Mark_Phase;

      --------------------------
      -- Mark_Data_Time_Stamp --
      --------------------------

      procedure Mark_Data_Time_Stamp (Index : in Positive) is
      begin
         Set (Index).Data_Time_Stamp := Ada.Calendar.Clock;
      end Mark_Data_Time_Stamp;

      ------------------
      -- Is_Abortable --
      ------------------

      function Is_Abortable
        (Index : in Positive;
         Mode  : in Timeout_Mode)
        return Boolean
      is
         use type Calendar.Time;
         Phase : constant Slot_Phase    := Set (Index).Phase;
         Now   : constant Calendar.Time := Calendar.Clock;
      begin
         return
           (Phase in Abortable_Phase
            and then
            Now - Set (Index).Phase_Time_Stamp > Timeouts (Mode, Phase))

           or else

           (Phase in Data_Phase
            and then
            Now - Set (Index).Data_Time_Stamp > Data_Timeouts (Phase));
      end Is_Abortable;

      ----------------------
      -- Abort_On_Timeout --
      ----------------------

      procedure Abort_On_Timeout
        (Mode : in Timeout_Mode; Done : out Boolean) is
      begin
         Done := False;

         for S in Set'Range loop
            if Is_Abortable (S, Mode) then
               Sockets.Shutdown (Set (S).Sock);
               Mark_Phase (S, Closed);
               Done := True;
            end if;
         end loop;
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

      procedure Get (FD : in Sockets.Socket_FD; Index : in Positive) is
      begin
         Set (Index).Sock := FD;
         Mark_Phase (Index, Client_Header);
         Set (Index).Activity_Counter := Set (Index).Activity_Counter + 1;
         Count := Count - 1;
      end Get;

      ---------
      -- Get --
      ---------

      function Get (Index : in Positive) return Slot is
      begin
         return Set (Index);
      end Get;

      -------------------------------------
      -- Increment_Slot_Activity_Counter --
      -------------------------------------

      procedure Increment_Slot_Activity_Counter (Index : in Positive) is
      begin
         Set (Index).Slot_Activity_Counter
           := Set (Index).Slot_Activity_Counter + 1;
      end Increment_Slot_Activity_Counter;

      -------------
      -- Release --
      -------------

      procedure Release (Index : in Positive) is
      begin
         Count := Count + 1;

         if Set (Index).Phase /= Closed then

            if not Set (Index).Socket_Taken then
               Sockets.Shutdown (Set (Index).Sock);

            else
               Set (Index).Socket_Taken := False;
            end if;

            Mark_Phase (Index, Closed);
         end if;
      end Release;

      ------------------
      -- Socket_Taken --
      ------------------

      procedure Socket_Taken (Index : in Positive) is
      begin
         Set (Index).Socket_Taken := True;
      end Socket_Taken;

      ----------
      -- Free --
      ----------

      function Free return Boolean is
      begin
         return Count > 0;
      end Free;

      ------------------
      -- Get_Peername --
      ------------------

      function Get_Peername (Index : in Positive) return String is
      begin
         return To_String (Set (Index).Peername);
      end Get_Peername;

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

   end Slots;

   -----------
   -- Start --
   -----------

   procedure Start
     (Web_Server                : in out HTTP;
      Name                      : in     String;
      Callback                  : in     Response.Callback;
      Max_Connection            : in     Positive         := Def_Max_Connect;
      Admin_URI                 : in     String           := Def_Admin_URI;
      Port                      : in     Positive         := Def_Port;
      Security                  : in     Boolean          := False;
      Session                   : in     Boolean          := False;
      Case_Sensitive_Parameters : in     Boolean          := True;
      Upload_Directory          : in     String           := Def_Upload_Dir) is
   begin
      CNF.Set.Server_Name      (Web_Server.Properties, Name);
      CNF.Set.Admin_URI        (Web_Server.Properties, Admin_URI);
      CNF.Set.Server_Port      (Web_Server.Properties, Port);
      CNF.Set.Security         (Web_Server.Properties, Security);
      CNF.Set.Session          (Web_Server.Properties, Session);
      CNF.Set.Upload_Directory (Web_Server.Properties, Upload_Directory);
      CNF.Set.Max_Connection   (Web_Server.Properties, Max_Connection);

      CNF.Set.Case_Sensitive_Parameters
        (Web_Server.Properties, Case_Sensitive_Parameters);

      Start (Web_Server, Callback);
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
      Start (Web_Server, Callback);
   end Start;

   -----------
   -- Start --
   -----------

   procedure Start
     (Web_Server : in out HTTP;
      Callback   : in     Response.Callback)
   is
      Accepting_Socket : Sockets.Socket_FD;

      Max_Connection   : constant Positive
        := CNF.Max_Connection (Web_Server.Properties);

   begin
      Web_Server.CB := Callback;

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
               CNF.Cleaner_Server_Response_Timeout (Web_Server.Properties))),

         (Client_Data     =>
            CNF.Receive_Timeout (Web_Server.Properties),
          Server_Response =>
            CNF.Send_Timeout (Web_Server.Properties)));

      --  Started time

      Web_Server.Start_Time := Calendar.Clock;

      --  Initialize the connection lines

      Web_Server.Lines := new Line_Set (1 .. Max_Connection);

      --  Initialize the cleaner task

      Web_Server.Cleaner := new Line_Cleaner (Web_Server.Self);

      --  Initialize the server socket

      Sockets.Socket
        (Accepting_Socket,
         Sockets.AF_INET,
         Sockets.SOCK_STREAM);

      Sockets.Bind (Accepting_Socket,
                    CNF.Server_Port (Web_Server.Properties));

      Sockets.Listen
        (Accepting_Socket,
         Queue_Size => CNF.Accept_Queue_Size (Web_Server.Properties));

      Web_Server.Sock := Accepting_Socket;

      --  Start each connection lines.

      for I in 1 .. Max_Connection loop
         Web_Server.Lines (I).Start (Web_Server, I);
      end loop;

      --  Initialize session server.

      if AWS.Config.Session (Web_Server.Properties) then
         AWS.Session.Control.Start
           (Session_Check_Interval => CNF.Session_Cleanup_Interval,
            Session_Lifetime       => CNF.Session_Lifetime);
      end if;

   end Start;

   ---------------
   -- Start_Log --
   ---------------

   procedure Start_Log
     (Web_Server        : in out HTTP;
      Split_Mode        : in     Log.Split_Mode := Log.None;
      Filename_Prefix   : in     String         := "")
   is
      use type AWS.Log.Split_Mode;
   begin
      if Split_Mode /= Log.None then
         CNF.Set.Log_Split_Mode
           (Web_Server.Properties, Log.Split_Mode'Image (Split_Mode));
      end if;

      if Filename_Prefix /= "" then
         CNF.Set.Log_Filename_Prefix
           (Web_Server.Properties, Filename_Prefix);
      end if;

      Log.Start
        (Web_Server.Log,
         Log.Split_Mode'Value (CNF.Log_Split_Mode (Web_Server.Properties)),
         CNF.Log_File_Directory (Web_Server.Properties),
         CNF.Log_Filename_Prefix (Web_Server.Properties));
   end Start_Log;

   --------------
   -- Stop_Log --
   --------------

   procedure Stop_Log (Web_Server : in out HTTP) is
   begin
      Log.Stop (Web_Server.Log);
   end Stop_Log;

end AWS.Server;
