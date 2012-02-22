------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2004-2012, AdaCore                     --
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

--  Test for big data pending in socket

with Ada.Text_IO;
with Ada.Exceptions;
with Ada.Streams;

with AWS.Net;
with AWS.Net.SSL;

with Stack_Size;

procedure Sock3_Proc (Security : Boolean) is

   use AWS;
   use Ada;
   use Ada.Streams;

   B_Size : constant := 100_000;

   Sample : Stream_Element_Array (1 .. B_Size) := (others => 12);

   Server : Net.Socket_Type'Class := Net.Socket (False);
   Peer   : Net.Socket_Type'Class := Net.Socket (Security);

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

      Client.Connect (Server.Get_Addr, Server.Get_Port);

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
   Text_IO.Put_Line ("start");

   Server.Bind (0, "localhost");
   Server.Listen;

   Client_Side.Start;

   Server.Accept_Socket (Peer);

   Net.Send (Peer, Sample);

   Client_Side.Stop;

   Net.Shutdown (Peer);
   Net.Shutdown (Server);

   Text_IO.Put_Line ("done.");

exception
   when E : others =>
      Text_IO.Put_Line (Exceptions.Exception_Information (E));
end Sock3_Proc;
