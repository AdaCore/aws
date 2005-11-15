------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                            Copyright (C) 2005                            --
--                                ACT-Europe                                --
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

--  Test for output buffer overflow.

with Ada.Text_IO;
with Ada.Exceptions;
with Ada.Streams;

with AWS.Net;

with Get_Free_Port;

procedure SockOver is

   use AWS;
   use Ada;
   use Ada.Streams;

   One   : constant Stream_Element := 1;
   Two   : constant Stream_Element := 2;
   Three : constant Stream_Element := 3;

   Sample1 : constant Stream_Element_Array
     := One & Two & Three & One & Two & Three & One & Two & Three & One & Two
        & Three & One & Two & Three & One & Two & Three & One & Two & Three
        & One & Two & Three & One & Two & Three & One & Two & Three & One & Two
        & Three & One & Two & Three & One & Two & Three & One & Two & Three;

   Sample2 : constant Stream_Element_Array
                        (Sample1'First + 1 .. Sample1'Last + 1) := Sample1;
   Sample3 : constant Stream_Element_Array (1 .. Sample1'Length) := Sample1;

   Server : Net.Socket_Type'Class := Net.Socket (False);
   Peer   : Net.Socket_Type'Class := Net.Socket (False);
   Client : Net.Socket_Type'Class := Net.Socket (False);

   Free_Port : Positive := 8080;

   --------------
   -- Transmit --
   --------------

   procedure Transmit (Sample : in Stream_Element_Array) is
      Last   : Stream_Element_Offset;
      Buffer : Stream_Element_Array (1 .. 1024);
   begin
      --  Fill output buffer.

      loop
         Net.Send (Client, Sample, Last);
         exit when Last < Sample'Last;
      end loop;

      --  provoke to timeout.

      begin
         Net.Send (Client, Sample);
      exception
         when E : Net.Socket_Error =>
            --  Expected timeout exception message.

            Text_IO.Put_Line (Exceptions.Exception_Message (E));
      end;

      begin
         loop
            Net.Receive (Peer, Buffer, Last);
            exit when Last < Buffer'Last;
         end loop;
      exception when Net.Socket_Error => null; -- Ignore input timeout.
      end;
   end Transmit;

begin
   Text_IO.Put_Line ("Start.");

   Get_Free_Port (Free_Port);

   Net.Bind (Server, Free_Port);
   Net.Listen (Server);

   Net.Connect (Client, "localhost", Free_Port);
   Net.Accept_Socket (Server, Peer);

   Net.Set_Timeout (Client, 0.1);
   Net.Set_Timeout (Peer,   0.1);

   Transmit (Sample3);
   Transmit (Sample2);
   Transmit (Sample1);

   Net.Shutdown (Server);
   Net.Shutdown (Client);
   Net.Shutdown (Peer);

   Text_IO.Put_Line ("Done.");

exception
   when E : others =>
      Text_IO.Put_Line (Exceptions.Exception_Information (E));
end SockOver;
