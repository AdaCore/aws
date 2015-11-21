------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                        Copyright (C) 2015, AdaCore                       --
--                                                                          --
--  This is free software;  you can redistribute it  and/or modify it       --
--  under terms of the  GNU General Public License as published  by the     --
--  Free Software  Foundation;  either version 3,  or (at your option) any  --
--  later version.  This software is distributed in the hope  that it will  --
--  be useful, but WITHOUT ANY WARRANTY;  without even the implied warranty --
--  of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU     --
--  General Public License for  more details.                               --
--                                                                          --
--  You should have  received  a copy of the GNU General  Public  License   --
--  distributed  with  this  software;   see  file COPYING3.  If not, go    --
--  to http://www.gnu.org/licenses for a complete copy of the license.      --
------------------------------------------------------------------------------

with Ada.Calendar;
with Ada.Text_IO;
with Ada.Exceptions;
with Ada.Streams;

with AWS.Net.Std;

procedure SendSet is

   use AWS;
   use Ada;
   use Ada.Streams;

   use type Calendar.Time;

   Max_Client : constant := 20;

   Start, Stop : Calendar.Time;

   Sample : Stream_Element_Array (1 .. 4_000_000);

   Server : Net.Socket_Type'Class := Net.Socket (False);
   Peers  : Net.Socket_Set (1 .. Max_Client + 1) :=
              (others => new Net.Socket_Type'Class'(Net.Socket (False)));

   task type Client_Side is
      entry Start (Index : Positive);
      entry Stop;
   end Client_Side;

   -----------------
   -- Client_Side --
   -----------------

   task body Client_Side is
      First  : Stream_Element_Offset := Sample'First;
      Client : Net.Socket_Type'Class := Net.Socket (False);
      I      : Positive;
      F      : Positive := 1;
   begin
      accept Start (Index : Positive) do
         I := Index;
      end Start;

      --  Client.Set_Timeout (20.0);

      Client.Connect (Server.Get_Addr, Server.Get_Port);

--      if I = 1 then
--         delay 5.0;
--      else
--         F := 4096;
--      end if;

      loop
         declare
            Buffer : Stream_Element_Array :=
                       Net.Receive
                         (Client,
                          Max => Stream_Element_Offset (Sample'Last / 3));
            Next   : Stream_Element_Offset := First + Buffer'Length;
         begin
            if Buffer'Length = 0 then
               Text_IO.Put_Line ("short data");
            elsif Sample (First .. Next - 1) /= Buffer then
               Text_IO.Put_Line ("wrong data");
            end if;

            exit when Next > Sample'Last;

--            if I = 1 and then Next > 2 * (Sample'Last / 3) then
--               delay 2.0;
--            end if;

            First := Next;
         end;
      end loop;

      Text_IO.Put_Line ("client task done:" & Positive'Image (I));
      Text_IO.Flush;

      accept Stop;

      Net.Shutdown (Client);
   exception
      when E : others =>
         Text_IO.Put_Line (Exceptions.Exception_Information (E));

         accept Stop;
   end Client_Side;

   Clients : array (1 .. Max_Client) of Client_Side;

begin
   for J in Sample'Range loop
      Sample (J) := Stream_Element
                      (J mod Stream_Element_Offset (Stream_Element'Last));
   end loop;

   Text_IO.Put_Line ("start");

   Net.Bind (Server, 0, "roxy");
   Net.Listen (Server);

   Text_IO.Put_Line ("port: " & Positive'Image (Server.Get_Port));

   Server.Accept_Socket (Peers (1).all);
   --  Peers (1).Set_Timeout (3.0);
   Peers (1).Set_Send_Buffer_Size (Integer (Sample'Last / 3));

   for K in Clients'Range loop
      Clients (K).Start (K);
   end loop;

   --  Server.Set_Timeout (2.0);

   for K in 2 .. Peers'Last loop
      Server.Accept_Socket (Peers (K).all);
      --  Peers (K).Set_Timeout (3.0);
      --  Peers (K).Set_Send_Buffer_Size (Integer (Sample'Last / 3));
   end loop;

   Start := Calendar.Clock;

   Net.Send (Peers, Sample);

   for K in Clients'Range loop
      Clients (K).Stop;
   end loop;

   Stop := Calendar.Clock;

   for K in Clients'Range loop
      Net.Shutdown (Peers (K).all);
   end loop;

   Text_IO.Put_Line ("Send timing: " & Duration'Image (Stop - Start));

   Net.Shutdown (Server);

   Text_IO.Put_Line ("done.");

exception
   when E : others =>
      Text_IO.Put_Line (Exceptions.Exception_Information (E));

      for K in Clients'Range loop
         Clients (K).Stop;
      end loop;
end SendSet;
