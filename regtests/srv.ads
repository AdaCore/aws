
with AWS.Server;
with AWS.Status;
with AWS.Response;
pragma Elaborate_All (AWS.Server);

package Srv is

   use AWS;

   WS  : Server.HTTP;
   WS2 : Server.HTTP;

   function CB (Request : in Status.Data) return Response.Data;

end Srv;
