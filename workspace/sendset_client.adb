------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                        Copyright (C) 2015, AdaCore                       --
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

--  $ trickle -s -u 10 -d 10 ./sendset_client localhost 44673

with Ada.Command_Line;
with Ada.Calendar;
with Ada.Text_IO;
with Ada.Exceptions;
with Ada.Streams;

with AWS.Net.Std;

procedure SendSet_Client is

   use AWS;
   use Ada;
   use Ada.Streams;

   Sample : Stream_Element_Array (1 .. 4_000_000);

   First  : Stream_Element_Offset := Sample'First;
   Client : Net.Socket_Type'Class := Net.Socket (False);
   F      : Positive := 1;
begin
   for J in Sample'Range loop
      Sample (J) := Stream_Element
                      (J mod Stream_Element_Offset (Stream_Element'Last));
   end loop;

   Client.Set_Timeout (20.0);
   Client.Connect
     (Command_Line.Argument (1),
      Port => Positive'Value (Command_Line.Argument (2)));

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

   Text_IO.Put_Line ("client done");
   Text_IO.Flush;

   Net.Shutdown (Client);

exception
   when E : others =>
      Text_IO.Put_Line (Exceptions.Exception_Information (E));
end SendSet_Client;
