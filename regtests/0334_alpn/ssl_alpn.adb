------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2004-2021, AdaCore                     --
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

--  Test for

with Ada.Text_IO;
with Ada.Exceptions;
with Ada.Streams;

with AWS.Containers.String_Vectors;
with AWS.Net.Log;
with AWS.Net.SSL;

procedure SSL_ALPN is

   use AWS;
   use Ada;
   use Ada.Streams;

   package SV renames AWS.Containers.String_Vectors;

   Sample : Stream_Element_Array (1 .. 10_000);

   Server : Net.Socket_Type'Class := Net.Socket (False);
   Peer   : Net.SSL.Socket_Type;

   task Client_Side is
      entry Start;
      entry Act (Protocols : SV.Vector);
      entry Add (Protocol  : String);
   end Client_Side;

   -----------------
   -- Client_Side --
   -----------------

   task body Client_Side is
      First  : Stream_Element_Offset;
      Client : Net.SSL.Socket_Type;
      Config : Net.SSL.Config;
      Enough : Boolean := False;
   begin
      Net.SSL.Initialize (Config, "");

      Client.Set_Config (Config);

      accept Start;

      while not Enough loop
         Client.Connect (Server.Get_Addr, Server.Get_Port);

         Text_IO.Put_Line ("Protocol: """ & Client.ALPN_Get & '"');

         First := Sample'First;

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

         select
            accept Act (Protocols : SV.Vector) do
               Net.SSL.ALPN_Set (Config, Protocols);
               Enough := Protocols.Is_Empty;
            end;
         or
            accept Add (Protocol : String) do
               Net.SSL.ALPN_Include (Config, Protocol);
            end;
         end select;
      end loop;

      Text_IO.Put_Line ("client task done.");

   exception
      when E : others =>
         Text_IO.Put_Line (Exceptions.Exception_Information (E));
   end Client_Side;

   --------------------
   -- Error_Callback --
   --------------------

   procedure Error_Callback
     (Socket : Net.Socket_Type'Class; Message : String) is
   begin
      Text_IO.Put_Line ("# " & Message);
   end Error_Callback;

   Config : Net.SSL.Config;

begin
   for J in Sample'Range loop
      Sample (J) := Stream_Element
                      (J mod Stream_Element_Offset (Stream_Element'Last));
   end loop;

   Net.SSL.Initialize
     (Config, Certificate_Filename => "cert.pem", ALPN => SV."&" ("ap2", "ap3"));
   Peer.Set_Config (Config);

   Net.Log.Start
     (Write => null, Event => null,
      Error => Error_Callback'Unrestricted_Access);

   Text_IO.Put_Line ("start");

   Server.Bind (0, "localhost");
   Server.Listen;

   Client_Side.Start;
   Net.SSL.Accept_Socket (Server, Peer);
   Peer.Send (Sample);
   Peer.Shutdown;

   Client_Side.Act (SV."&" ("ap1", "ap2"));
   Net.SSL.Accept_Socket (Server, Peer);
   Peer.Send (Sample);
   Peer.Shutdown;

   Client_Side.Act (SV."&" ("ap2", "ap3"));
   Net.SSL.Accept_Socket (Server, Peer);
   Peer.Send (Sample);
   Peer.Shutdown;

   Client_Side.Act (SV."&" ("ap3", "ap4"));
   Net.SSL.Accept_Socket (Server, Peer);
   Peer.Send (Sample);
   Peer.Shutdown;

   Client_Side.Act (SV."&" ("ap4", "ap5"));
   Net.SSL.Accept_Socket (Server, Peer);
   Peer.Send (Sample);
   Peer.Shutdown;

   Client_Side.Add ("ap3");
   Net.SSL.Accept_Socket (Server, Peer);
   Peer.Send (Sample);
   Peer.Shutdown;

   Client_Side.Act (SV.Empty_Vector);

   Server.Shutdown;

   Text_IO.Put_Line ("done.");

exception
   when E : others =>
      Text_IO.Put_Line (Exceptions.Exception_Information (E));
end SSL_ALPN;
