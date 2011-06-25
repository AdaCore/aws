------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2004-2011, AdaCore                     --
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

with Ada.Exceptions;
with Ada.Streams;
with Ada.Text_IO;
with AWS.Net.SSL;
with Get_Free_Port;

procedure Plain2SSL is
   use AWS.Net;
   use Ada.Streams;
   use Ada.Text_IO;

   Client, Server, Peer : Socket_Type'Class := Socket (False);
   Port   : Positive := 3456;
   Sample : Stream_Element_Array (1 .. 1000);

   procedure Test (Source, Target : Socket_Type'Class);

   ----------
   -- Test --
   ----------

   procedure Test (Source, Target : Socket_Type'Class) is
      Buffer : Stream_Element_Array (Sample'Range);
      First  : Stream_Element_Offset := Buffer'First;
      Last   : Stream_Element_Offset;

      task Send_Task;

      task body Send_Task is
      begin
         --  We have to send in task for SSL negotiation

         Send (Source, Sample);

      exception
         when E : others =>
            Put_Line ("Send_Task " & Ada.Exceptions.Exception_Information (E));
      end Send_Task;

   begin
      loop
         Receive (Target, Buffer (First .. Buffer'Last), Last);
         exit when Last = Buffer'Last;
         First := Last + 1;
      end loop;

      if Buffer = Sample then
         Put_Line ("Ok.");
      else
         Put_Line ("Error.");
      end if;

   exception
      when E : Socket_Error =>
         if Is_Timeout (E) then
            Put_Line ("Error.");
         else
            raise;
         end if;
   end Test;

begin
   for J in Sample'Range loop
      Sample (J) := Stream_Element
                      (J mod (Stream_Element_Offset (Stream_Element'Last)));
   end loop;

   Get_Free_Port (Port);
   Bind (Server, Port);
   Listen (Server);
   Connect (Client, "localhost", Port);
   Accept_Socket (Server, Peer);

   Set_Timeout (Client, 1.0);
   Set_Timeout (Peer, 1.0);

   Test (Client, Peer);
   Test (Peer, Client);

   declare
      --  Convert to SSL connection

      SSL_Client : SSL.Socket_Type := SSL.Secure_Client (Client);
      SSL_Peer   : SSL.Socket_Type := SSL.Secure_Server (Peer);
   begin
      --  Between SSL

      Test (SSL_Client, SSL_Peer);
      Test (SSL_Peer, SSL_Client);

      --  Provoke data errors

      Test (SSL_Client, Peer);
      Test (SSL_Peer, Client);

      --  Provoke SSL errors

      begin
         Test (Client, SSL_Peer);
      exception
         when E : Socket_Error =>
            Put_Line (Ada.Exceptions.Exception_Message (E));
      end;

      SSL.Shutdown (SSL_Client);
      SSL.Shutdown (SSL_Peer);
   end;
end Plain2SSL;
