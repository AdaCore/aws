
with Ada.Text_IO;

with AWS.Server;
with AWS.Response;
with AWS.Status;
with AWS.MIME;

procedure Simple is

   use AWS;

   WS : Server.HTTP;

   function CB (Request : in Status.Data) return Response.Data is
   begin
      return Response.Build (MIME.Text_HTML, "should not be called");
   end CB;

begin
   Server.Start
     (WS, "simple", CB'Unrestricted_Access, Port => 1234, Max_Connection => 5);
   Ada.Text_IO.Put_Line ("started"); Ada.Text_IO.Flush;
   delay 1.0;
   Server.Shutdown (WS);
   Ada.Text_IO.Put_Line ("shutdown");
end Simple;
