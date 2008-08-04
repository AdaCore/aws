------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2004-2008, AdaCore                     --
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

--  Test for socket timeouts

with Ada.Text_IO;
with Ada.Exceptions;
with Ada.Streams;

with AWS.Net;

procedure STO_Proc (Security : in Boolean; Port : in Positive) is

   use AWS;
   use Ada;
   use Ada.Streams;

   Sample : Stream_Element_Array (1 .. 100_000);

   Server       : Net.Socket_Type'Class := Net.Socket (False);
   Peer, Client : Net.Socket_Type'Class := Net.Socket (Security);

   task Client_Side is
      entry Start;
      entry Stop;
   end Client_Side;

   -----------------
   -- Client_Side --
   -----------------

   task body Client_Side is
      First : Stream_Element_Offset := Sample'First;
   begin
      accept Start;

      --  delay for accept timeout

      delay 1.5;

      Net.Connect (Client, "localhost", Port);
      Net.Set_Timeout (Client, 1.0);

      begin
         declare
            Buffer : Stream_Element_Array := Net.Receive (Client);
         begin
            null;
         end;
      exception
         when E : Net.Socket_Error =>
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

      delay 2.0;

      Net.Shutdown (Client);

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

   Net.Bind (Server, Port);
   Net.Listen (Server);
   Net.Set_Timeout (Server, 1.0);

   Client_Side.Start;

   begin
      Net.Accept_Socket (Server, Peer);
   exception
      when E : Net.Socket_Error =>
         Ada.Text_IO.Put_Line ("accept");
         Ada.Text_IO.Put_Line (Exceptions.Exception_Message (E));
   end;

   Net.Accept_Socket (Server, Peer);

   Net.Set_Timeout (Peer, 1.0);

   delay 1.5;

   begin
      loop
         Net.Send (Peer, Sample);
      end loop;
   exception
      when E : Net.Socket_Error =>
         Ada.Text_IO.Put_Line ("send");
         Ada.Text_IO.Put_Line (Exceptions.Exception_Message (E));
   end;

   Client_Side.Stop;

   Net.Shutdown (Peer);

   Text_IO.Put_Line ("done.");

exception
   when E : others =>
      Text_IO.Put_Line (Exceptions.Exception_Information (E));
end STO_Proc;
