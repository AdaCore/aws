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
         --  We have to send in task for SSL negotiation.

         Send (Source, Sample);
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

   Set_Timeout (Client, 2.0);
   Set_Timeout (Peer, 2.0);

   Test (Client, Peer);
   Test (Peer, Client);

   declare
      --  Convert to SSL connection.

      SSL_Client : SSL.Socket_Type := SSL.Secure_Client (Client);
      SSL_Peer   : SSL.Socket_Type := SSL.Secure_Server (Peer);
   begin
      --  Between SSL.

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
      SSL.Free (SSL_Client);
      SSL.Free (SSL_Peer);
   end;
end Plain2SSL;
