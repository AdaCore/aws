
--  $Id$

with Ada.Text_IO;
with Sockets;

procedure Demo is

   use Ada;

   procedure Server is
      Accepting_Socket : Sockets.Socket_FD;
      Incoming_Socket  : Sockets.Socket_FD;

   begin
      Accepting_Socket := Sockets.Socket (Sockets.AF_INET,
                                          Sockets.SOCK_STREAM);
      Sockets.Setsockopt (Accepting_Socket,
                          Sockets.SOL_SOCKET,
                          Sockets.SO_REUSEADDR,
                          1);

      Sockets.Bind (Accepting_Socket, 80);

      Sockets.Listen (Accepting_Socket);

      Incoming_Socket := Sockets.Accept_Socket (Accepting_Socket);
   end Server;

   procedure Client is
      Sock : Sockets.Socket_FD;
   begin
      Sock := Sockets.Socket (Sockets.AF_INET, Sockets.SOCK_STREAM);
      Sockets.Connect (Sock, "dieppe", 80);

      Sockets.Put_Line (Sock, "POST /cgi-bin/display.exe HTTP/1.1");
      Sockets.Put_Line (Sock, "Accept: image/gif, image/x-xbitmap, image/jpeg, image/pjpeg, */*");
--      Sockets.Put_Line (Sock, "Accept: text/html");
--      Sockets.Put_Line (Sock, "Referer: http://dieppe:80/toto/");
      Sockets.Put_Line (Sock, "Accept-Language: fr");
--      Sockets.Put_Line (Sock, "Content-Type: text/html");
--      Sockets.Put_Line (Sock, "Content-Type: application/x-www-form-urlencoded");
      Sockets.Put_Line (Sock, "Accept-Encoding: gzip, deflate");
      Sockets.Put_Line (Sock, "User-Agent: AWS");
      Sockets.Put_Line (Sock, "Host: dieppe:80");
      Sockets.Put_Line (Sock, "Content-Length: 22");
      Sockets.New_Line (Sock);

      Sockets.Put_Line (Sock, "name=pascal&surn=1234");

      loop
         declare
            Data : constant String := Sockets.Get_Line (Sock);
         begin
--            exit when Data = "";
            Text_IO.Put_Line (Data);
         end;
      end loop;
   end Client;

begin

   Client;

end Demo;


