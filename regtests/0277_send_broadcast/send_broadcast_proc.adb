------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                       Copyright (C) 2015, AdaCore                        --
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

--  Test for broadcast data sending

with Ada.Exceptions;
with Ada.Streams;
with Ada.Text_IO;

with AWS.Net.SSL;

procedure Send_Broadcast_Proc (Security : Boolean) is

   use Ada;
   use Ada.Streams;
   use AWS;

   Max_Client : constant := 10;

   Sample     : Stream_Element_Array (1 .. 500_000);

   Server     : Net.Socket_Type'Class := Net.Socket (False);
   Peers      : Net.Socket_Set (1 .. Max_Client) :=
                  (others =>
                     new Net.Socket_Type'Class'(Net.Socket (Security)));

   protected Shared is
      procedure Connect
        (Client : in out Net.Socket_Type'Class;
         Addr   : String;
         Port   : Positive);
   end Shared;

   task type Client_Side is
      entry Start;
      entry Stop;
   end Client_Side;

   ------------
   -- Shared --
   ------------

   protected body Shared is

      procedure Connect
        (Client : in out Net.Socket_Type'Class;
         Addr   : String;
         Port   : Positive) is
      begin
         Client.Connect (Addr, Port);
      end Connect;

   end Shared;

   -----------------
   -- Client_Side --
   -----------------

   task body Client_Side is
      First  : Stream_Element_Offset := Sample'First;
      Client : Net.Socket_Type'Class := Net.Socket (Security);
   begin
      accept Start;

      Shared.Connect (Client, Server.Get_Addr, Server.Get_Port);

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

            First := Next;
         end;
      end loop;

      Net.Shutdown (Client);

      accept Stop;
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

   Net.Bind (Server, 0, "localhost");
   Net.Listen (Server);

   for K in Clients'Range loop
      Clients (K).Start;
   end loop;

   Server.Set_Timeout (20.0);

   for K in Clients'Range loop
      Server.Accept_Socket (Peers (K).all);
      Peers (K).Set_Timeout (10.0);
      Peers (K).Set_Send_Buffer_Size (Integer (Sample'Last / 3));
   end loop;

   Net.Send (Peers, Sample);

   for K in Clients'Range loop
      Clients (K).Stop;
   end loop;

   for K in Clients'Range loop
      Net.Shutdown (Peers (K).all);
   end loop;

   Net.Shutdown (Server);

   Text_IO.Put_Line ("done.");

exception
   when E : others =>
      Text_IO.Put_Line (Exceptions.Exception_Information (E));
      for K in Clients'Range loop
         Clients (K).Stop;
      end loop;
end Send_Broadcast_Proc;
