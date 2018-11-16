------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2004-2018, AdaCore                     --
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

with Ada.Exceptions;
with Ada.Streams;
with Ada.Text_IO;
with AWS.Net.SSL;

procedure Plain2SSL is
   use AWS.Net;
   use Ada.Streams;
   use Ada.Text_IO;

   Client, Server, Peer : Socket_Type'Class := Socket (False);
   Sample : Stream_Element_Array (1 .. 1000);
   Family : Family_Type;

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

   if IPv6_Available then
      Family := Family_Inet6;
   else
      Family := Family_Inet;
   end if;

   Server.Bind (0, Family => Family);
   Server.Listen;
   Connect (Client, "localhost", Server.Get_Port, Family => Family);
   Accept_Socket (Server, Peer);

   Set_Timeout (Client, 1.0);
   Set_Timeout (Peer, 1.0);

   Test (Client, Peer);
   Test (Peer, Client);

   declare
      --  Convert to SSL connection

      SSL_Client : SSL.Socket_Type := SSL.Secure_Client
                                        (Client, Host => "localhost");
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
            declare
               Text : constant String := Ada.Exceptions.Exception_Message (E);
               Last : Positive := Text'Last;
            begin
               if Text (Last) = ASCII.LF then
                  Last := Last - 1;
               end if;

               if Text (1 .. Last) in "An unexpected TLS packet was received."
                 | "A record packet with illegal version was received."
                 | "A packet with illegal or unsupported version was received."
                 | "1408F10B:SSL routines:SSL3_GET_RECORD:wrong version number"
                 | "1408F10B:SSL routines:ssl3_get_record:wrong version number"
                 | "1404C10B:SSL routines:ST_OK:wrong version number"
               then
                  Put_Line ("Expected error about wrong data received");
               else
                  Put_Line ("Unexpected: " & Text);
               end if;
            end;
      end;

      SSL.Shutdown (SSL_Client);
      SSL.Shutdown (SSL_Peer);
   end;
end Plain2SSL;
