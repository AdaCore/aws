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
with Ada.Exceptions;
with Ada.Text_IO;
with Ada.Unchecked_Conversion;
with Interfaces.C;

with Sockets.Thin;
with Sockets.Naming;

with AWS.Net;
with AWS.Session;

package body AWS.Server is

   use Ada;

   protected File_Upload_UID is
      procedure Get (ID : out Natural);
      --  returns a UID for file upload. This is to ensure that files
      --  coming from clients will always have different name.
   private
      UID : Natural := 0;
   end File_Upload_UID;

   procedure Protocol_Handler
     (Sock        : in     Sockets.Socket_FD'Class;
      HTTP_Server : in out HTTP;
      Index       : in     Positive);
   --  handle the line, this is where the HTTP protocol is defined.

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

   -----------
   -- Start --
   -----------

   procedure Start (Web_Server : in out HTTP;
                    Name       : in     String;
                    Admin_URI  : in     String := No_Admin)
   is
      Accepting_Socket : Sockets.Socket_FD;
   begin
      Web_Server.Name        := To_Unbounded_String (Name);
      Web_Server.Admin_URI   := To_Unbounded_String (Admin_URI);
      Web_Server.Upload_Path := To_Unbounded_String (Default_Upload_Path);

      Sockets.Socket (Accepting_Socket,
                      Sockets.AF_INET,
                      Sockets.SOCK_STREAM);

      Sockets.Setsockopt (Accepting_Socket,
                          Sockets.SOL_SOCKET,
                          Sockets.SO_REUSEADDR,
                          1);

      Sockets.Bind (Accepting_Socket, Web_Server.Port);

      Sockets.Listen (Accepting_Socket);

      Web_Server.Sock := Accepting_Socket;

      --  initialize each connection lines.

      for I in 1 .. Web_Server.Max_Connection loop
         Web_Server.Lines (I).Start (Web_Server, I);
      end loop;

      if Web_Server.Session then
         Session.Start;
      end if;
   end Start;

   --------------
   -- Shutdown --
   --------------

   procedure Shutdown (Web_Server : in out HTTP) is
   begin
      Web_Server.Shutdown := True;

      Sockets.Shutdown (Web_Server.Sock);
      abort Web_Server.Cleaner;

      for S in 1 .. Web_Server.Max_Connection loop
         Web_Server.Slots.Release (S);
      end loop;

      if Web_Server.Session then
         Session.Shutdown;
      end if;
   end Shutdown;

   -----------
   -- Slots --
   -----------

   protected body Slots is

      -------------------
      -- Set_Abortable --
      -------------------

      procedure Set_Abortable (Index : in Positive; Flag : in Boolean) is
      begin
         Set (Index).Abortable := Flag;
      end Set_Abortable;

      ------------------
      -- Set_Peername --
      ------------------

      procedure Set_Peername (Index : in Positive; Peername : in String) is
      begin
         Set (Index).Peername := To_Unbounded_String (Peername);
      end Set_Peername;

      ------------------------
      -- Mark_Activity_Time --
      ------------------------

      procedure Mark_Activity_Time (Index : in Positive) is
      begin
         Set (Index).Activity_Time_Stamp := Ada.Calendar.Clock;
      end Mark_Activity_Time;

      ------------------
      -- Abort_Oldest --
      ------------------

      procedure Abort_Oldest (Force : in Boolean) is
         use type Calendar.Time;
         To_Be_Closed        : Natural := 0;
         Time_Stamp          : Calendar.Time := Calendar.Clock;
         Activity_Time_Stamp : Calendar.Time;
      begin

         --  look for the oldest abortable slot

         for S in Set'Range loop
            Activity_Time_Stamp := Set (S).Activity_Time_Stamp;

            if Set (S).Abortable
            and then Activity_Time_Stamp < Time_Stamp then
               To_Be_Closed := S;
               Time_Stamp   := Activity_Time_Stamp;
            end if;

         end loop;

         --  if one Slot has and abortable state, and we are in Force mode or
         --  slot is open since more than Keep_Open_Duration we close it.

         if To_Be_Closed /= 0
           and then
           (Force or else (Calendar.Clock - Time_Stamp) > Keep_Open_Duration)
         then
            Sockets.Shutdown (Set (To_Be_Closed).Sock);
            Set (To_Be_Closed).Opened := False;
         end if;
      end Abort_Oldest;

      ---------
      -- Get --
      ---------

      procedure Get (FD    : in Sockets.Socket_FD; Index : in Positive) is
      begin
         Set (Index).Sock      := FD;
         Set (Index).Opened    := True;
         Set (Index).Abortable := False;
         Set (Index).Activity_Counter := Set (Index).Activity_Counter + 1;

         Count := Count - 1;

         if Count = 0 and then Set'Length > 1 then
            Abort_Oldest (True);
         end if;
      end Get;

      ---------
      -- Get --
      ---------

      function Get (Index : in Positive) return Slot is
      begin
         return Set (Index);
      end Get;

      -------------
      -- Release --
      -------------

      procedure Release (Index : in Positive) is
      begin
         Count := Count + 1;
         if Set (Index).Opened then
            Sockets.Shutdown (Set (Index).Sock);
            Set (Index).Opened := False;
         end if;
      end Release;

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

   end Slots;


   ------------------
   -- Line_Cleaner --
   ------------------

   task body Line_Cleaner is
      Is_Force : Boolean;
   begin
      loop
         select
            accept Force do
               Is_Force := True;
            end Force;
         or
            delay 30.0;
            Is_Force := False;
         end select;

         Server.Slots.Abort_Oldest (Is_Force);
      end loop;
   end Line_Cleaner;

   ----------------------
   -- Protocol_Handler --
   ----------------------

   procedure Protocol_Handler
     (Sock        : in     Sockets.Socket_FD'Class;
      HTTP_Server : in out HTTP;
      Index       : in     Positive) is separate;

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
                                               Sockaddr'Unchecked_Access,
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
              AWS.Net.Accept_Socket (HTTP_Server.Sock,
                                     HTTP_Server.Security);

         begin
            begin
               HTTP_Server.Slots.Get (Sockets.Socket_FD (Sock), Slot_Index);

               HTTP_Server.Slots.Set_Peername
                 (Slot_Index,
                  Get_Peername (Sockets.Socket_FD (Sock)));

               Protocol_Handler (Sock, HTTP_Server.all, Slot_Index);

            exception

               --  we must never exit from the outer loop as a Line task is
               --  supposed to live forever.
               --  We have here a pool of Line and each line is recycled when
               --  needed.

               when Sockets.Connection_Closed | Connection_Error =>
                  Text_IO.Put_Line
                    ("Connection closed (time-out or not enough slots).");

               when E : others =>
                  Text_IO.Put_Line ("A problem has been detected!");
                  Text_IO.Put_Line ("Connection will be closed...");
                  Text_IO.New_Line;
                  Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
            end;

            HTTP_Server.Slots.Set_Abortable (Slot_Index, False);
            HTTP_Server.Slots.Release (Slot_Index);
            Sockets.Shutdown (Sock);
         end;
      end loop;

   exception

      when E : others =>

         if not HTTP_Server.Shutdown then
            Text_IO.Put_Line ("Slot problem has been detected!");
            Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
         end if;

   end Line;

end AWS.Server;
