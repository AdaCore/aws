------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2012, AdaCore                     --
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

--  Regression test of the socket timeout

with AWS.Net;
with Ada.Streams;
with Ada.Text_IO;
with Ada.Exceptions;

procedure SockTO_Proc (Security : Boolean) is

   use AWS;
   use Ada;
   use Streams;

   D1 : constant Duration := 1.0;
   D2 : constant Duration := D1 * 1.5;

   Server : Net.Socket_Type'Class := Net.Socket (False);
   Peer   : Net.Socket_Type'Class := Net.Socket (Security);

   task Client_Side is
      entry Start;
      entry Stop;
   end Client_Side;

   function Data (Length : Stream_Element_Count) return Stream_Element_Array;
   pragma Inline (Data);

   procedure Get
     (Socket : Net.Socket_Type'Class; Length : Stream_Element_Count);
   --  Read all data length

   -----------------
   -- Client_Side --
   -----------------

   task body Client_Side is
      Client : Net.Socket_Type'Class := Net.Socket (Security);
   begin
      accept Start;

      Net.Set_Timeout (Client, D1);

      Client.Connect (Server.Get_Addr, Server.Get_Port);

      for J in 1 .. 5 loop
         Net.Send (Client, Data (1234));
         delay D2;
      end loop;

      Get (Client, 100_000);

      accept Stop;
   exception
      when E : others =>
         Text_IO.Put_Line
           ("Client side " & ASCII.LF & Exceptions.Exception_Information (E));
         accept Stop;
   end Client_Side;

   ----------
   -- Data --
   ----------

   function Data (Length : Stream_Element_Count) return Stream_Element_Array is
      Result : Stream_Element_Array (1 .. Length);
   begin
      for J in Result'Range loop
         Result (J)
           := Stream_Element
                (J mod (Stream_Element_Offset (Stream_Element'Last) + 1));
      end loop;

      return Result;
   end Data;

   ---------
   -- Get --
   ---------

   procedure Get
     (Socket : Net.Socket_Type'Class; Length : Stream_Element_Count)
   is
      Sample : constant Stream_Element_Array := Data (Length);
      Rest   : Stream_Element_Count  := Length;
      Index  : Stream_Element_Offset := Sample'First;
   begin
      loop
         declare
            Buffer : constant Stream_Element_Array := Socket.Receive (Rest);
            Next   : constant Stream_Element_Offset := Index + Buffer'Length;
         begin
            if Buffer /= Sample (Index .. Next - 1) then
               Text_IO.Put_Line
                 ("Data error" & Integer'Image (Buffer'Length)
                  & Stream_Element'Image (Buffer (Buffer'First))
                  & Stream_Element'Image (Sample (Sample'First)));
            end if;

            exit when Next > Sample'Last;

            Rest  := Rest - Buffer'Length;
            Index := Next;
         end;
      end loop;

      Text_IO.Put_Line ("Got length" & Stream_Element_Count'Image (Length));
   exception
      when E : others =>
         Text_IO.Put_Line
           ("Got length"
            & Stream_Element_Count'Image (Length - Rest)
            & ' ' & Exceptions.Exception_Message (E));
   end Get;

begin
   Server.Bind (0, "localhost");
   Server.Listen;

   Client_Side.Start;

   Net.Set_Timeout (Server, 3.0);
   Net.Set_Timeout (Peer, D1);

   Net.Accept_Socket (Server, Peer);

   for J in 1 .. 10 loop
      Get (Peer, 1234);
   end loop;

   loop
      Net.Send (Peer, Data (100_000));
   end loop;

   Client_Side.Stop;

exception
   when E : others =>
      Client_Side.Stop;
      Text_IO.Put_Line
        ("Server side " & Exceptions.Exception_Message (E));
      Net.Shutdown (Server);
      Net.Shutdown (Peer);
end SockTO_Proc;
