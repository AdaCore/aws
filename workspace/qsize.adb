
--  $Id$
--  check for socket queue size.

with Ada.Exceptions;
with Ada.Text_Io;
with Sockets;
with Ada.Unchecked_Deallocation;

procedure Qsize is

   use Ada.Text_Io;

   task type Client;

   type Access_Client is access Client;
   procedure Free is new Ada.Unchecked_Deallocation (Client, Access_Client);

   protected Wait_Counter is
      procedure Add (N : Integer);
   private
      Internal : Integer := 0;
   end Wait_Counter;

   Port : constant := 1234;

   protected body Wait_Counter is

      procedure Add (N : Integer)
      is
      begin
         Internal := Internal + N;
         Put_Line ("Wait_Counter" & Internal'Img);
      end;

   end Wait_Counter;

   task body Client is
      Socket : Sockets.Socket_Fd;
      Char : String (1 .. 1);
   begin
      Sockets.Socket (Socket, Sockets.Af_Inet, Sockets.Sock_Stream);

      Wait_Counter.Add (1);

      Sockets.Connect (Socket, "localhost", Port);

      Char := Sockets.Get (Socket);

      -- We are never going to be there.
      -- becouse the server is not going to accept sockets.
      Ada.Text_IO.Put_Line ("We are never going to be there.");

      Wait_Counter.Add (-1);

   exception
      when E : others =>
         Put_Line (Ada.Exceptions.Exception_Information (E));
         Wait_Counter.Add (-1);
         Sockets.Shutdown (Socket);
   end Client;

   Client_Ptr : access_Client;
   Accepting_Socket : Sockets.Socket_Fd;

begin
   Sockets.Socket
     (Accepting_Socket,
      Sockets.Af_Inet,
      Sockets.Sock_Stream);

   Sockets.Bind (Accepting_Socket, Port);

   --  Increase the queue size parameter
   --  if you see that it is no more then OS limit.
   Sockets.Listen (Accepting_Socket, Queue_Size => 500);

   Put_Line ("Press 'q' to exit.");
   Put_Line ("Any other key to add new client trying to connect.");

   loop

     declare
        Char : Character;
     begin
        Get_Immediate (Char);
        exit when Char = 'q';
     end;

     if Client_Ptr /= null and then Client_Ptr'Terminated then
        Free (Client_Ptr);
     end if;

     Client_Ptr := new Client;

   end loop;

   Sockets.Shutdown (Accepting_Socket);

end Qsize;
