------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2005-2012, AdaCore                     --
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

--  Common code for plain and SSL testing Socket_Pair routine

with Ada.Streams;
with Ada.Text_IO;
with AWS.Net;

procedure SPair_Proc (Security : Boolean) is
   use Ada.Streams;
   use Ada.Text_IO;

   S1, S2 : AWS.Net.Socket_Type'Class := AWS.Net.Socket (Security);
   Sample : Stream_Element_Array (1 .. 128);
   Buffer : Stream_Element_Array (1 .. 130);
   Last   : Stream_Element_Offset;

   procedure Test (S1, S2 : in out AWS.Net.Socket_Type'Class);

   procedure Test (S1, S2 : in out AWS.Net.Socket_Type'Class) is
   begin
      if Security then
         --  SSL socket need handshake during send/receive.
         --  we need different tasks to do this.

         declare
            task Send;
            task body Send is
            begin
               AWS.Net.Send (S2, Sample);
            end Send;
         begin
            AWS.Net.Receive (S1, Buffer, Last);
         end;

      else
         AWS.Net.Send (S2, Sample);
         AWS.Net.Receive (S1, Buffer, Last);
      end if;

      if Sample = Buffer (1 .. Last) then
         Put_Line ("OK");
      else
         Put_Line ("Error");
      end if;
   end Test;

begin
   for J in Sample'Range loop
      Sample (J) := Stream_Element (J);
   end loop;

   AWS.Net.Socket_Pair (S1, S2);

   Test (S1, S2);
   Test (S2, S1);
   AWS.Net.Shutdown (S1);
   AWS.Net.Shutdown (S2);
end SPair_Proc;
