
package body Srv is

   use AWS;

   function CB (Request : in Status.Data) return Response.Data is
   begin
      return Response.Build ("text/html", "hello");
   end CB;

end Srv;
