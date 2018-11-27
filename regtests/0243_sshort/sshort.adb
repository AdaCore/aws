------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                       Copyright (C) 2014, AdaCore                        --
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

--  Test to do not stuck data in the SSL input buffer

pragma Ada_2012;

with Ada.Calendar;
with Ada.Command_Line;
with Ada.Text_IO;
with Ada.Exceptions;
with Ada.Streams;

with AWS.Net.Log;
with AWS.Net.SSL;
with AWS.Net.Std;
with AWS.Utils;

with GNAT.Traceback.Symbolic;

procedure SShort is

   use AWS;
   use Ada;
   use Ada.Streams;

   use type Utils.Random_Integer;

   Sample : Stream_Element_Array (1 .. 1024);
   First  : Stream_Element_Offset; -- For fragments
   Last   : Stream_Element_Offset;
   Server : Net.Socket_Type'Class := Net.Socket (False);
   Peer   : Net.SSL.Socket_Type;
   Stamp  : Ada.Calendar.Time;
   Config : Net.SSL.Config;

   task Client_Side is
      entry Start;
      entry Receive;
      entry Received (More : Boolean);
   end Client_Side;

   -----------------
   -- Client_Side --
   -----------------

   task body Client_Side is
      Client : Net.SSL.Socket_Type;
      Buffer : Stream_Element_Array (Sample'Range);
      First  : Stream_Element_Offset;
      Last   : Stream_Element_Offset;
      More   : Boolean;
      Config : Net.SSL.Config;
   begin
      accept Start;

      Net.SSL.Initialize (Config, "");
      Client.Set_Config (Config);

      Client.Connect (Server.Get_Addr, Server.Get_Port);
      Client.Set_Timeout (0.25);

      loop
         accept Receive;

         Last := 0;

         Client.Send ((1 => 11));

         loop
            First := Last + 1;
            Client.Receive (Buffer (First .. Buffer'Last), Last);
            exit when Client.Pending = 0;
         end loop;

         if Buffer (1 .. Last) /= Sample (1 .. SShort.Last) then
            if Buffer (1 .. Last) = Sample (1 .. Last) then
               Ada.Text_IO.Put_Line
                 ("Data shorter " & SShort.Last'Img & Last'Img);
            else
               Ada.Text_IO.Put_Line ("Data differ");
            end if;
            exit;
         end if;

         accept Received (More : Boolean) do
            Client_Side.More := More;
         end Received;

         exit when not More;
      end loop;

      Client.Shutdown;

   exception
      when E : others =>
         Text_IO.Put_Line (Exceptions.Exception_Information (E));
   end Client_Side;

   -----------
   -- Error --
   -----------

   procedure Error (Socket : Net.Socket_Type'Class; Message : String) is
      use GNAT.Traceback;
      Trace : Tracebacks_Array (1 .. 64);
      Last  : Natural;
   begin
      Call_Chain (Trace, Last);

      Ada.Text_IO.Put_Line
        ("# Network error: "
         & Message & Symbolic.Symbolic_Traceback (Trace (1 .. Last)));
   end Error;

   ------------------
   -- Set_Fragment --
   ------------------

   procedure Set_Fragment is
   begin
      First := Last + 1;

      Last := First + Stream_Element_Offset (AWS.Utils.Random rem 256);

      for J in First .. Last loop
         Sample (J) := Stream_Element'Mod (Utils.Random);
      end loop;
   end Set_Fragment;

begin
   Net.Log.Start (Error => Error'Unrestricted_Access, Write => null);

   Text_IO.Put_Line ("start");

   Server.Bind (0, "localhost");
   Server.Listen;

   Client_Side.Start;

   Net.SSL.Initialize (Config, "certificate.pem");
   Peer.Set_Config (Config);

   Net.SSL.Accept_Socket (Server, Peer);

   Stamp := Ada.Calendar.Clock;

   for J in 1 .. 10000 loop
      Last := 0;

      Set_Fragment;
      Peer.Send (Sample (First .. Last));
      Set_Fragment;
      Peer.Send (Sample (First .. Last));
      Set_Fragment;
      Peer.Send (Sample (First .. Last));

      Client_Side.Receive;

      if Peer.Receive /= (1 => 11) then
         Ada.Text_IO.Put_Line ("Unexpected responce");
      end if;

      select Client_Side.Received (More => True);
      or delay 0.25;
         Ada.Text_IO.Put_Line ("Timeout");
      end select;
   end loop;

   Client_Side.Receive;

   Peer.Send (Sample (1 .. Last));

   Client_Side.Received (More => False);

   Peer.Shutdown;
   Server.Shutdown;

   if Ada.Command_Line.Argument_Count > 0 then
      Text_IO.Put_Line
         (Duration'Image (Ada.Calendar."-" (Ada.Calendar.Clock, Stamp)));
   end if;

   Text_IO.Put_Line ("done.");

exception
   when E : others =>
      Text_IO.Put_Line (Exceptions.Exception_Information (E));
end SShort;
