
with Ada.Strings.Unbounded;

with Input_Sources.Strings;
with Sax.Readers;
with Sax.Attributes;
with Unicode.CES.Basic_8bit;

with SOAP.Message.Payload;
with SOAP.Message.Response;
with SOAP.Message.Error;

package SOAP.Message.XML is

   use Ada.Strings.Unbounded;

   function Load_Payload  (XML : in String) return Message.Payload.Object;
   function Load_Response (XML : in String) return Object'Class;

   function Image (O : in Object'Class) return String;
   function Image (O : in Object'Class) return Unbounded_String;

private

   ---------------------------
   -- SAX reader definition --
   ---------------------------

   type State is
     (Start,   -- Nothing parsed yet
      S_Env,   -- Start of SOAP envelope
      S_Body,  -- Start of SOAP body
      S_Wrap,  -- Start of Wrapper (procedure or SOAP:Fault)
      P_Int,   -- Integer declaration found, next object will be the value
      P_Float, -- Float declaration found, next object will be the value
      E_Wrap,  -- End of Wrapper (procedure or SOAP:Fault)
      E_Body,  -- End of SOAP body
      E_Env);  -- End of SOAP envelope

   subtype State_Param is State
     range State'Succ (S_Wrap) .. State'Pred (E_Wrap);

   -----------------
   -- SOAP Reader --
   -----------------

   type SOAP_Reader is new Sax.Readers.Reader with record
      S : State := Start;

      Parameters   : SOAP.Parameters.Set;
      Last_Name    : Unbounded_String;
      Wrapper_Name : Unbounded_String;
   end record;

   procedure Characters
     (Handler : in out SOAP_Reader;
      Ch      : in     Unicode.CES.Byte_Sequence);

   procedure Set_Wrapper_Name
     (Handler : in out SOAP_Reader;
      Name    : in     String);

   procedure Start_Element
     (Handler       : in out SOAP_Reader;
      Namespace_URI : in     Unicode.CES.Byte_Sequence       := "";
      Local_Name    : in     Unicode.CES.Byte_Sequence       := "";
      Qname         : in     Unicode.CES.Byte_Sequence       := "";
      Atts          : in     Sax.Attributes.Attributes'Class);

   --------------------
   -- Payload Reader --
   --------------------

   type Payload_Reader is new SOAP_Reader with record
      Payload : Message.Payload.Object;
   end record;

   procedure End_Element
     (Handler       : in out Payload_Reader;
      Namespace_URI : in     Unicode.CES.Byte_Sequence := "";
      Local_Name    : in     Unicode.CES.Byte_Sequence := "";
      Qname         : in     Unicode.CES.Byte_Sequence := "");

   procedure Set_Wrapper_Name
     (Handler : in out Payload_Reader;
      Name    : in     String);

   ---------------------
   -- Response Reader --
   ---------------------

   type Response_Reader is new SOAP_Reader with record
      Is_Error : Boolean := False;
      Response : Message.Response.Object;
      Error    : Message.Error.Object;
   end record;

   procedure Set_Wrapper_Name
     (Handler : in out Response_Reader;
      Name    : in     String);

end SOAP.Message.XML;
