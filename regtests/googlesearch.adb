
--  $Id$
--  Dummy body to check that the client and server code generated from WSDL
--  compile without problem.

with Ada.Text_IO;
with GoogleSearchService.Client;
with GoogleSearchService.Server;

procedure GoogleSearch is
begin
   Ada.Text_IO.Put_Line ("OK");
end GoogleSearch;
