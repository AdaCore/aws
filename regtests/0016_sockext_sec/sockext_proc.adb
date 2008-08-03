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

--  Test for detect output buffer overflow

with Ada.Text_IO;
with Ada.Exceptions;
with Ada.Streams;

with AWS.Net.Buffered;

procedure SockExt_Proc (Security : Boolean; Port : Positive) is

   use AWS;
   use Ada;
   use Ada.Streams;

   Sample1, Sample2 : Stream_Element_Array (1 .. 1000);

   Server : Net.Socket_Type'Class := Net.Socket (False);
   Peer   : Net.Socket_Type'Class := Net.Socket (Security);
   Last   : Stream_Element_Offset;

   Write_Count : Natural := 0;
   Read_Count  : Natural := 0;

   task Client_Side is
      entry Start;
      entry Read;
      entry Stop;
   end Client_Side;

   -----------------
   -- Client_Side --
   -----------------

   task body Client_Side is
      Client     : Net.Socket_Type'Class := Net.Socket (Security);
   begin
      accept Start;

      Text_IO.Put_Line ("Client start.");

      Net.Connect (Client, "localhost", Port);
      Net.Set_Timeout (Client, 1.0);

      accept Read;

      loop
         declare
            Buffer : Stream_Element_Array (Sample1'Range);
         begin
            Net.Buffered.Read (Client, Buffer);

            Read_Count := Read_Count + 1;

            exit when Buffer = Sample2;

            if Buffer /= Sample1 then
               raise Program_Error;
            end if;
         end;
      end loop;

      declare
         Buffer : Stream_Element_Array (Sample1'Range);
         Last   : Stream_Element_Offset;
      begin
         Net.Buffered.Read (Client, Buffer, Last);
         Text_IO.Put_Line ("Regression.");
      exception
         when E : Net.Socket_Error =>
            Text_IO.Put_Line ("Expected: " & Exceptions.Exception_Message (E));
      end;

      Net.Shutdown (Client);

      Text_IO.Put_Line ("Client done.");

      accept Stop;

   exception
      when E : others =>
         Text_IO.Put_Line (Exceptions.Exception_Information (E));
         accept Stop;
   end Client_Side;

begin
   for J in Sample1'Range loop
      Sample1 (J) := Stream_Element
                       (J mod (Stream_Element_Offset (Stream_Element'Last)));
      Sample2 (Sample2'Last + Sample2'First - J) := Sample1 (J);
   end loop;

   Text_IO.Put_Line ("Server start.");

   Net.Bind (Server, Port);
   Net.Listen (Server);

   Client_Side.Start;

   Net.Accept_Socket (Server, Peer);

   Net.Set_Timeout (Peer, 0.0);

   loop
      Net.Send (Peer, Sample1, Last);
      Write_Count := Write_Count + 1;
      exit when Last < Sample1'Last;
   end loop;

   Client_Side.Read;

   Net.Set_Timeout (Peer, 1.0);

   Net.Send (Peer, Sample1 (Last + 1 .. Sample1'Last));

   Net.Send (Peer, Sample2);

   Client_Side.Stop;

   Net.Shutdown (Peer);

   if Read_Count /= Write_Count + 1 then
      Text_IO.Put_Line ("Read_Count /= Write_Count + 1");
   end if;

   Text_IO.Put_Line ("Server done.");

exception
   when E : others =>
      Text_IO.Put_Line (Exceptions.Exception_Information (E));
end SockExt_Proc;
