
--  $Id$

--  http://www-config.der.edf.fr/proxy-HN0060A.pac

with Ada.Text_IO;
with AWS.Translater;
with Sockets;

procedure Demo is

   use Ada;

   procedure Server is
      Accepting_Socket : Sockets.Socket_FD;
      Incoming_Socket  : Sockets.Socket_FD;

   begin
      Sockets.Socket (Accepting_Socket,
                      Sockets.AF_INET,
                      Sockets.SOCK_STREAM);

      Sockets.Setsockopt (Accepting_Socket,
                          Sockets.SOL_SOCKET,
                          Sockets.SO_REUSEADDR,
                          1);

      Sockets.Bind (Accepting_Socket, 80);

      Sockets.Listen (Accepting_Socket);

      Sockets.Accept_Socket (Accepting_Socket, Incoming_Socket);
   end Server;

   procedure Client is
      Sock : Sockets.Socket_FD;
   begin
      Sockets.Socket (Sock, Sockets.AF_INET, Sockets.SOCK_STREAM);
      Sockets.Connect (Sock, "dieppe", 1234);

      Sockets.Put_Line (Sock, "HEAD /last HTTP/1.1");
--      Sockets.Put_Line (Sock, "Date: Thu, 18 Jan 2000 06:46:00 GMT");
--      Sockets.Put_Line (Sock, "Accept: image/gif, image/x-xbitmap, image/jpeg, image/pjpeg, */*");
      Sockets.Put_Line (Sock, "Accept: */*");
--      Sockets.Put_Line (Sock, "Referer: http://dieppe:80/toto/");
      Sockets.Put_Line (Sock, "Accept-Language: fr");
      Sockets.Put_Line (Sock, "Accept-Encoding: gzip, deflate");
--      Sockets.Put_Line (Sock, "Content-Type: text/html");
--      Sockets.Put_Line (Sock, "Content-Length: 0");
--      Sockets.Put_Line (Sock, "Content-Type: application/x-www-form-urlencoded");
      Sockets.Put_Line (Sock, "User-Agent: AWS");
      Sockets.Put_Line (Sock, "Host: dieppe");
      Sockets.Put_Line (Sock, "Proxy-Connection: Keep-Alive");
--      Sockets.Put_Line (Sock, "Extension: Security/Remote-Passphrase");

--      Sockets.Put_Line (Sock, "Content-Length: 22");
      Sockets.New_Line (Sock);

--      Sockets.Put_Line (Sock, "name=pascal&surn=1234");

      for K in 1 .. 50 loop
         declare
            Data : constant String := Sockets.Get_Line (Sock);
         begin
--            exit when Data = "";
            Text_IO.Put_Line (Data);
         end;
      end loop;

      Sockets.Shutdown (Sock);

      Sockets.Socket (Sock, Sockets.AF_INET, Sockets.SOCK_STREAM);
      Sockets.Connect (Sock, "130.98.248.13", 3128);

      Sockets.Put_Line (Sock, "GET http://www.microsoft.com/ HTTP/1.1");
      Sockets.Put_Line (Sock, "Accept: */*");
      Sockets.Put_Line (Sock, "Accept-Language: fr");
      Sockets.Put_Line (Sock, "Accept-Encoding: gzip, deflate");
      Sockets.Put_Line (Sock, "User-Agent: AWS");
      Sockets.Put_Line (Sock, "Host: www.microsoft.com");
      Sockets.Put_Line (Sock, "Proxy-Connection: Keep-Alive");
      Sockets.Put_Line
        (Sock, "Proxy-Authorization: Basic " &
         AWS.Translater.Base64_Encode ("pascal.obry:turboada"));

      Sockets.New_Line (Sock);

      for K in 1 .. 50 loop
         declare
            Data : constant String := Sockets.Get_Line (Sock);
         begin
            Text_IO.Put_Line (Data);
         end;
      end loop;

      Sockets.Shutdown (Sock);

   end Client;

begin

   Client;

end Demo;


