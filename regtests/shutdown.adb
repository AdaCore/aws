
with Ada.Text_IO;
with AWS.Server;

with Srv;

procedure Shutdown is
   use Ada;
   use AWS;

   WS : Server.HTTP;

begin
   Text_IO.Put_Line ("start"); Text_IO.Flush;

   Server.Start (Srv.WS, "demo", Srv.CB'Access, Port => 1262);
   Server.Start (WS, "demo", Srv.CB'Access, Port => 1263);

   delay 2.0;

   Text_IO.Put_Line ("shutdown"); Text_IO.Flush;
   Server.Shutdown (Srv.WS);
   Server.Shutdown (WS);

   Text_IO.Put_Line ("wait..."); Text_IO.Flush;
   Server.Wait (Server.No_Server);
end Shutdown;
