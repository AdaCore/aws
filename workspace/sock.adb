
--  $Id$

--  Check heavy loaded server and how a client reuse a socket or not.
--  There is a problem here in the client API, after some time the program
--  fails because a socket is reused it seems to be closed but it can't be
--  opened. Anyway this problem will have to be sorted out...

with Ada.Exceptions;
with Ada.Text_IO;

with Sockets;
with Sockets.Thin;

procedure Sock is

   N_Client : constant := 20;
   N_Mess   : constant := 500;

   use Ada;

   task Server is
      entry Start;
   end Server;

   task type Client is
      entry Start;
   end Client;

   task body Server is
      K : Natural := 0;
      Accepting_Socket : Sockets.Socket_FD;
      New_Socket       : Sockets.Socket_FD;
   begin
      Sockets.Socket
        (Accepting_Socket,
         Sockets.AF_INET,
         Sockets.SOCK_STREAM);

      Sockets.Bind (Accepting_Socket, 8786);

      Sockets.Listen (Accepting_Socket, 5064);

      accept Start;

      loop
         begin
            Sockets.Accept_Socket (Accepting_Socket, New_Socket);

            Sockets.Put_Line (New_Socket, "STOP");

            declare
               Res : constant String := Sockets.Get_Line (New_Socket);
            begin
               if Res /= "END" then
                  Text_IO.Put_Line ("S] error, res is " & Res);
               end if;
            end;

            Sockets.Shutdown (New_Socket);

            K := K + 1;

            if K mod 100 = 0 then
               Text_IO.Put_Line ("S " & K'Img);
            end if;

            exit when K = N_Client * N_Mess;

         exception
            when E : others =>
               Text_IO.Put_Line ("S] " & Exceptions.Exception_Information (E));
         end;
      end loop;
   end Server;

   task body Client is
      Socket : Sockets.Socket_FD;
   begin
      accept Start;
      for K in 1 .. N_Mess loop
         begin
            Sockets.Socket (Socket, Sockets.AF_INET, Sockets.SOCK_STREAM);

            Sockets.Setsockopt
              (Socket,
               Sockets.SOL_SOCKET,
               Sockets.SO_REUSEADDR,
               1);

            Sockets.Connect (Socket, "localhost", 8786);

            declare
               Res : constant String := Sockets.Get_Line (Socket);
            begin
               if Res /= "STOP" then
                  Text_IO.Put_Line ("C] error, res is " & Res);
               end if;
            end;

            Sockets.Put_Line (Socket, "END");

            Sockets.Shutdown (Socket);
            delay 0.5;

         exception
            when E : others =>
               Text_IO.Put_Line ("C] " & Exceptions.Exception_Information (E));
         end;
      end loop;
   end Client;

   Clients : array (1 .. N_Client) of Client;

begin
   -- let some time for server to start..
   delay 2.0;

   Server.Start;

   -- then launch clients

   for K in Clients'Range loop
      Clients (K).Start;
   end loop;
end Sock;
