with SOAP.Message.Payload;
with SOAP.Message.Response;

package SOAP.Client.Callback is

   --  Callback routine types
   type Pre_Call_CB is not null access
      procedure (Connection : AWS.Client.HTTP_Connection;
                 Request    : SOAP.Message.Payload.Object;
                 Schema     : SOAP.WSDL.Schema.Definition);

   type Post_Call_CB is not null access
      procedure (Connection : AWS.Client.HTTP_Connection;
                 Request    : SOAP.Message.Payload.Object;
                 Response   : SOAP.Message.Response.Object'Class;
                 Schema     : SOAP.WSDL.Schema.Definition);

   --  Default callback routines
   procedure Null_Pre_Call_Callback
      (Connection : AWS.Client.HTTP_Connection;
       Request    : SOAP.Message.Payload.Object;
       Schema     : SOAP.WSDL.Schema.Definition) is null;

   procedure Null_Post_Call_Callback
      (Connection : AWS.Client.HTTP_Connection;
       Request    : SOAP.Message.Payload.Object;
       Response   : SOAP.Message.Response.Object'Class;
       Schema     : SOAP.WSDL.Schema.Definition) is null;

end SOAP.Client.Callback;
