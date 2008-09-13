------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2005-2008, AdaCore                     --
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

--  Common code for plain and SSL testing Socket_Pair routine

with Ada.Streams;
with Ada.Text_IO;
with AWS.Net;

procedure S_SPair_Proc (Security : in Boolean) is
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
end S_SPair_Proc;
