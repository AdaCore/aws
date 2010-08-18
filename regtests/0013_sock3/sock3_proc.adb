------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2004-2010, AdaCore                     --
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

--  Test for big data pending in socket

with Ada.Text_IO;
with Ada.Exceptions;
with Ada.Streams;

with AWS.Net;
with AWS.Net.SSL;

with Get_Free_Port;
with Stack_Size;

procedure Sock3_Proc (Security : Boolean; Port : Positive) is

   use AWS;
   use Ada;
   use Ada.Streams;

   B_Size    : constant := 100_000;

   Sample    : Stream_Element_Array (1 .. B_Size) := (others => 12);

   Server    : Net.Socket_Type'Class := Net.Socket (False);
   Peer      : Net.Socket_Type'Class := Net.Socket (Security);

   Free_Port : Positive := Port;

   task Client_Side is
      pragma Storage_Size (Stack_Size.Value + B_Size);
      entry Start;
      entry Stop;
   end Client_Side;

   -----------------
   -- Client_Side --
   -----------------

   task body Client_Side is
      First  : Stream_Element_Offset := Sample'First;
      Client : Net.Socket_Type'Class := Net.Socket (Security);
      Size   : Natural := 0;

      function NVL (Item : Stream_Element_Count) return Stream_Element_Count is
      begin
         if Item = 0 then
            return 1;
         else
            return Item;
         end if;
      end NVL;

   begin
      accept Start;

      delay 0.125;

      Net.Connect (Client, "localhost", Free_Port);

      if Security then
         Net.Set_Timeout (Client, 2.0);
      else
         Net.Set_Timeout (Client, 0.5);
      end if;

      loop
         delay 0.01;

         declare
            Buffer : Stream_Element_Array (1 .. NVL (Net.Pending (Client)));
            Last   : Stream_Element_Offset;
         begin
            --  Want see ?
            --  Text_IO.Put_Line (Buffer'Length'Img); Text_IO.Flush;

            Net.Receive (Client, Buffer, Last);

            if Last /= Buffer'Last then
               Text_IO.Put_Line
                 (Last'Img & " Last /= Buffer'Last" & Buffer'Last'Img);
            end if;

            Size := Size + Buffer'Length;
         end;
      end loop;

   exception
      when E : Net.Socket_Error =>

         if Size /= Sample'Length then
            Text_IO.Put_Line ("Size /= Sample'Length");
         end if;

         Net.Shutdown (Client);

         Text_IO.Put_Line
           ("client side expected exception "
            & Exceptions.Exception_Message (E));

         accept Stop;

      when E : others =>
         Text_IO.Put_Line (Exceptions.Exception_Information (E));

         accept Stop;
   end Client_Side;

begin
   Get_Free_Port (Free_Port);

   Text_IO.Put_Line ("start");

   Net.Bind (Server, Free_Port);
   Net.Listen (Server);

   Client_Side.Start;

   Net.Accept_Socket (Server, Peer);

   Net.Send (Peer, Sample);

   Client_Side.Stop;

   Net.Shutdown (Peer);
   Net.Shutdown (Server);

   Text_IO.Put_Line ("done.");

exception
   when E : others =>
      Text_IO.Put_Line (Exceptions.Exception_Information (E));
end Sock3_Proc;
