with Ada.Text_IO;
with AWS.Net.Sets;

procedure Max_Poll_Size is
   use AWS.Net;
   Set : Sets.Socket_Set_Type;
   Port : constant Positive := 8088;
   Server, Client, Peer : Socket_Type'Class := Socket (False);

begin
   Bind (Server, Port);
   Listen (Server);

   loop
      Connect (Client, "localhost", Port);
      Accept_Socket (Server, Peer);
      Sets.Add (Set, Peer, Sets.Output);
      Sets.Add (Set, Client, Sets.Input);
      Sets.Wait (Set, 0.001);
      Ada.Text_IO.Put_Line (Sets.Count (Set)'Img);
   end loop;
end Max_Poll_Size;
