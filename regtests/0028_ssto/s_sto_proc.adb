------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2004-2014, AdaCore                     --
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

--  Test for socket timeouts

with Ada.Exceptions;
with Ada.Real_Time;
with Ada.Streams;
with Ada.Text_IO;

with AWS.Net;

procedure S_STO_Proc (Security : Boolean) is

   use Ada;
   use Ada.Streams;
   use AWS;

   Sample : Stream_Element_Array (1 .. 100_000);

   Server       : Net.Socket_Type'Class := Net.Socket (False);
   Peer, Client : Net.Socket_Type'Class := Net.Socket (Security);

   procedure Check_Timeout (Span : Ada.Real_Time.Time_Span);

   task Client_Side is
      entry Done;
      entry Start;
      entry Stop;
   end Client_Side;

   -------------------
   -- Check_Timeout --
   -------------------

   procedure Check_Timeout (Span : Ada.Real_Time.Time_Span) is
      D : constant Duration := Ada.Real_Time.To_Duration (Span);
   begin
      if not (D in 0.98 .. 1.1) then
         Text_IO.Put_Line ("wrong timeout" & D'Img);
      end if;
   end Check_Timeout;

   -----------------
   -- Client_Side --
   -----------------

   task body Client_Side is
      First : Stream_Element_Offset := Sample'First;
   begin
      accept Start;

      --  delay for accept timeout

      delay 1.5;

      Client.Connect (Server.Get_Addr, Server.Get_Port);
      Client.Set_Timeout (1.0);

      declare
         use Ada.Real_Time;
         Stamp : constant Time := Clock;
      begin
         if Client.Receive /= (1 .. 0 => 0) then
            Text_IO.Put_Line ("wrong receive");
         end if;
      exception
         when E : Net.Socket_Error =>
            Check_Timeout (Clock - Stamp);
            Ada.Text_IO.Put_Line ("receive");
            Ada.Text_IO.Put_Line (Exceptions.Exception_Message (E));
      end;

      loop
         declare
            Buffer : constant Stream_Element_Array
              := Net.Receive
                   (Client,
                    Stream_Element_Count'Min (4096, Sample'Last - First + 1));

            Next : constant Stream_Element_Offset := First + Buffer'Length;
         begin
            if Buffer'Length = 0 then
               Text_IO.Put_Line ("short data");
            elsif Sample (First .. Next - 1) /= Buffer then
               Text_IO.Put_Line ("wrong data");
            end if;

            exit when Next > Sample'Last;

            First := Next;
         end;
      end loop;

      accept Done;

      Client.Shutdown;

      Text_IO.Put_Line ("client task done.");

      accept Stop;

   exception
      when E : others =>
         Text_IO.Put_Line (Exceptions.Exception_Information (E));
         accept Stop;
   end Client_Side;

begin
   for J in Sample'Range loop
      Sample (J) := Stream_Element
                      (J mod Stream_Element_Offset (Stream_Element'Last));
   end loop;

   Text_IO.Put_Line ("start");

   Server.Bind (0, "localhost");
   Server.Listen;
   Server.Set_Timeout (1.0);

   Client_Side.Start;

   declare
      use Ada.Real_Time;
      Stamp : constant Time := Clock;
   begin
      Net.Accept_Socket (Server, Peer);
   exception
      when E : Net.Socket_Error =>
         Check_Timeout (Clock - Stamp);
         Ada.Text_IO.Put_Line ("accept");
         Ada.Text_IO.Put_Line (Exceptions.Exception_Message (E));
   end;

   Server.Accept_Socket (Peer);

   Peer.Set_Timeout (1.0);

   delay 1.5;

   declare
      use Ada.Real_Time;
      Stamp : Time;
   begin
      loop
         Stamp := Clock;
         Net.Send (Peer, Sample);
      end loop;
   exception
      when E : Net.Socket_Error =>
         Check_Timeout (Clock - Stamp);
         Ada.Text_IO.Put_Line ("send");
         Ada.Text_IO.Put_Line (Exceptions.Exception_Message (E));
   end;

   Client_Side.Done;

   Client_Side.Stop;

   Peer.Shutdown;

   Text_IO.Put_Line ("done.");

exception
   when E : others =>
      Text_IO.Put_Line (Exceptions.Exception_Information (E));
end S_STO_Proc;
