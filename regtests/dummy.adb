with Ada.Text_IO;
with AWS.Server;

procedure Dummy is
   WS   : AWS.Server.HTTP;
begin
   Ada.Text_IO.Put_Line ("Dummy");
end Dummy;
