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

with Sockets;

package body AWS.Server is

   use Ada;

   procedure Protocol_Handler (Sock    : in Sockets.Socket_FD;
                               Handler : in Response.Callback;
                               Slots   : in Slots_Access;
                               Index   : in Positive);
   --  handle the line, this is where the HTTP protocol is defined.

   -----------
   -- Start --
   -----------

   procedure Start (Web_Server : in out HTTP) is
      Accepting_Socket : Sockets.Socket_FD;
   begin
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

   end Start;

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

            if Set (S).Abortable and then Activity_Time_Stamp < Time_Stamp then
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

      procedure Get (FD : in Sockets.Socket_FD; Index : in Positive) is
      begin
         Set (Index).Sock   := FD;
         Set (Index).Opened := True;
         Count := Count - 1;
         if Count = 0 then
            Abort_Oldest (True);
         end if;
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

   procedure Protocol_Handler (Sock    : in Sockets.Socket_FD;
                               Handler : in Response.Callback;
                               Slots   : in Slots_Access;
                               Index   : in Positive) is separate;

   ----------
   -- Line --
   ----------

   task body Line is

      Server_Sock : Sockets.Socket_FD;
      Sock        : Sockets.Socket_FD;
      CB          : Response.Callback;
      Slots       : Slots_Access;
      Slot_Index  : Positive;

   begin

      select
         accept Start (Server : HTTP;
                       Index  : Positive)
         do
            Slots       := Server.Slots;
            Server_Sock := Server.Sock;
            Slot_Index  := Index;
            CB          := Server.CB;
         end Start;
      or
         terminate;
      end select;

      loop

         --  wait for an incoming connection.

         Sockets.Accept_Socket (Server_Sock, Sock);

         begin
            Slots.Get (Sock, Slot_Index);
            Slots.Set_Abortable (Slot_Index, True);

            Protocol_Handler (Sock, CB, Slots, Slot_Index);

         exception

            --  we must never exit from the outer loop as a Line task is
            --  supposed to live forever. We have here a pool of Line and each
            --  line is recycled when needed.

            when Sockets.Connection_Closed =>
               Text_IO.Put_Line ("Connection time-out, close it.");

            when E : others =>
               Text_IO.Put_Line ("A problem has been detected!");
               Text_IO.Put_Line ("Connection will be closed...");
               Text_IO.New_Line;
               Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));

         end;

         Slots.Set_Abortable (Slot_Index, False);
         Slots.Release (Slot_Index);

      end loop;

   exception

      when E : others =>
         Text_IO.Put_Line ("Slot problem has been detected!");
         Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));

   end Line;

end AWS.Server;
