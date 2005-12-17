
--  Test break socket operation by close.

with Ada.Exceptions;
with Ada.Streams;
with Ada.Text_IO;


with GNAT.Sockets;

procedure SBreak is

   use GNAT;

   Server  : Sockets.Socket_Type;
   Peer    : Sockets.Socket_Type;
   Address : Sockets.Sock_Addr_Type;
   Port    : constant := 8800;
   Count   : Natural  := 0;

   Indicator : array (0 .. 3) of Character := "|/-\";

   task Client is
      entry Start;
      entry Sent;
   end Client;

   task body Client is
      Socket  : Sockets.Socket_Type;
      Address : Sockets.Sock_Addr_Type;
      Last    : Ada.Streams.Stream_Element_Offset;
   begin
      accept Start;

      Address.Addr := Sockets.Inet_Addr ("127.0.0.1");
      Address.Port := Port;

      loop
         Sockets.Create_Socket (Socket);
         Sockets.Connect_Socket (Socket, Address);
         Sockets.Send_Socket (Socket, (1 .. 32 => 22), Last);

         accept Sent;

         Sockets.Shutdown_Socket (Peer);
         Sockets.Close_Socket (Peer);

         --  Just close, do not shutdown, because opposite
         --  peer is already closed.

         Sockets.Close_Socket (Socket);
      end loop;
   exception
      when E : others =>
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
   end Client;

begin
   Sockets.Initialize;

   Sockets.Create_Socket (Server);

   Sockets.Bind_Socket
     (Server,
      (Sockets.Family_Inet,
       Sockets.Any_Inet_Addr,
       Port));

   Sockets.Listen_Socket (Server);

   Client.Start;

   loop
      Sockets.Accept_Socket (Server, Peer, Address);

      declare
         use Ada.Streams;
         Data : Stream_Element_Array (1 .. 256);
         Last : Stream_Element_Offset;
      begin
         Client.Sent;

         loop
            Sockets.Receive_Socket (Peer, Data, Last);

            if Last < Data'First then
               Ada.Text_IO.Put_Line ("closed on opposite.");
            end if;
         end loop;
      exception
         when Sockets.Socket_Error =>
            Count := Count + 1;
            Ada.Text_IO.Put (Indicator (Count mod 4) & ASCII.CR);

            if Count rem 100 = 0 then
               Ada.Text_IO.Put (Count'Img & ASCII.CR);
            end if;
      end;
   end loop;

exception
   when E : others =>
      Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
end SBreak;
