
with AWS.Response;

package SOAP.Message.Error is

   type Object is new Message.Object with private;

   function Image (P : in Object) return Unbounded_String;

   function Build (O : in Object) return AWS.Response.Data;

   function Build
     (Faultcode   : in String;
      Faultstring : in String)
     return Object;

   -----------------
   -- Fault Codes --
   -----------------

   function Version_Mismatch (Subname : in String := "") return String;

   function Must_Understand (Subname : in String := "") return String;

   function Client (Subname : in String := "") return String;

   function Server (Subname : in String := "") return String;


private

   type Object is new Message.Object with record
      Faultcode   : Unbounded_String;
      Faultstring : Unbounded_String;
   end record;

end SOAP.Message.Error;
