------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2001                          --
--                               ACT-Europe                                 --
--                                                                          --
--  Authors: Dmitriy Anisimkov - Pascal Obry                                --
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

--  Candidate for regression test of the socket timeout.

with AWS.Net.Buffered;
with Ada.Text_IO;
with Ada.Exceptions;

procedure SockTO is
   use AWS;
   use Ada;

   Port : constant := 8800;

   Server, Peer, Client : Net.Socket_Type'Class := Net.Socket (False);

   task Client_Side;

   procedure Read_Line
     (Socket  : in out Net.Socket_Type'Class;
      Timeout : in     Duration);

   -----------------
   -- Client_Side --
   -----------------

   task body Client_Side is
   begin
      Net.Connect (Client, "localhost", Port);
      Read_Line (Client, 1.0);

      delay 10.0;

      Net.Buffered.Put_Line (Client, "Hi, wassup man ;-/");
      Read_Line (Client, 6.0);
   end Client_Side;

   ---------------
   -- Read_Line --
   ---------------

   procedure Read_Line
     (Socket  : in out Net.Socket_Type'Class;
      Timeout : in     Duration) is
   begin
      Net.Set_Timeout (Socket, Timeout);
      Text_IO.Put_Line (Net.Buffered.Get_Line (Socket));
   exception
      when E : others =>
         Text_IO.Put_Line (Exceptions.Exception_Message (E));
   end Read_Line;

begin
   Net.Bind (Server, Port);
   Net.Listen (Server);

   Net.Accept_Socket (Server, Peer);

   Net.Buffered.Put_Line (Peer, "It's me");

   Read_Line (Peer, 6.0);
   Read_Line (Peer, 6.0);

   for J in 1 .. 10000 loop
      Net.Buffered.Put_Line (Peer, Integer'Image (J));
   end loop;
end SockTO;