------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2004-2014, AdaCore                     --
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

--  Test for big data send/receive over secure socket

with Ada.Text_IO;
with Ada.Exceptions;
with Ada.Streams;

with AWS.Net.Log;
with AWS.Net.SSL;

procedure S_Sock2_Proc (Security : Boolean) is

   use AWS;
   use Ada;
   use Ada.Streams;

   Sample : Stream_Element_Array (1 .. 100_000);

   Server : Net.Socket_Type'Class := Net.Socket (False);
   Peer   : Net.Socket_Type'Class := Net.Socket (Security);

   task Client_Side is
      entry Start;
      entry Stop;
   end Client_Side;

   -----------------
   -- Client_Side --
   -----------------

   task body Client_Side is
      First  : Stream_Element_Offset := Sample'First;
      Client : Net.Socket_Type'Class := Net.Socket (Security);
   begin
      accept Start;

      delay 0.125;

      Client.Connect (Server.Get_Addr, Server.Get_Port);

      loop
         declare
            Buffer : Stream_Element_Array := Net.Receive (Client);
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

      Client.Shutdown;

      Text_IO.Put_Line ("client task done.");

      accept Stop;
   exception
      when E : others =>
         Text_IO.Put_Line (Exceptions.Exception_Information (E));

         accept Stop;
   end Client_Side;

   --------------------
   -- Error_Callback --
   --------------------

   procedure Error_Callback
     (Socket : Net.Socket_Type'Class; Message : String) is
   begin
      Text_IO.Put_Line ("# " & Message);
   end Error_Callback;

   --  The callback procedure which is called for every socket error

begin
   for J in Sample'Range loop
      Sample (J) := Stream_Element
                      (J mod Stream_Element_Offset (Stream_Element'Last));
   end loop;

   Net.Log.Start (Write => null,
                  Event => null,
                  Error => Error_Callback'Unrestricted_Access);

   Text_IO.Put_Line ("start");

   Server.Bind (0, "localhost");
   Server.Listen;

   Client_Side.Start;

   Server.Accept_Socket (Peer);

   Peer.Send (Sample);

   Peer.Shutdown;
   Server.Shutdown;

   Client_Side.Stop;

   Text_IO.Put_Line ("done.");

exception
   when E : others =>
      Text_IO.Put_Line (Exceptions.Exception_Information (E));
end S_Sock2_Proc;
