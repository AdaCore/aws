
package body SOAP.Message.Error is

   Version_Mismatch_Faultcode : constant String := "VersionMismatch";
   Must_Understand_Faultcode  : constant String := "MustUnderstand";
   Client_Faultcode           : constant String := "Client";
   Server_Faultcode           : constant String := "Server";

   Start_Fault_Env            : constant String := "<SOAP-ENV:Fault>";
   End_Fault_Env              : constant String := "</SOAP-ENV:Fault>";
   Start_Faultcode            : constant String := "<faultcode>";
   End_Faultcode              : constant String := "</faultcode>";
   Start_Faultstring          : constant String := "<faultstring>";
   End_Faultstring            : constant String := "</faultstring>";


   function Faultcode (Name, Subname : in String) return String;
   --  Returns the Faultcode for Name and Subname. If Subname is empty it
   --  returns Name otherwise it returns Name & '.' & Subname.

   -----------
   -- Build --
   -----------

   function Build (O : in Object) return AWS.Response.Data is
      K : AWS.Response.Data;
   begin
      return K;
   end Build;

   function Build
     (Faultcode   : in String;
      Faultstring : in String)
     return Object is
   begin
      return (Message.Object with
              To_Unbounded_String (Faultcode),
              To_Unbounded_String (Faultstring));
   end Build;

   ------------
   -- Client --
   ------------

   function Client (Subname : in String := "") return String is
   begin
      return Faultcode (Client_Faultcode, Subname);
   end Client;

   ---------------
   -- Faultcode --
   ---------------

   function Faultcode (Name, Subname : in String) return String is
   begin
      if Subname = "" then
         return Name;

      else
         return Name & '.' & Subname;
      end if;
   end Faultcode;

   -----------
   -- Image --
   -----------

   function Image (P : in Object) return Unbounded_String is
      NL           : constant String := ASCII.CR & ASCII.LF;
      Message_Body : Unbounded_String;
   begin
      --  Fault Env

      Append (Message_Body, Start_Fault_Env & NL);

      --  Faultcode

      Append (Message_Body, Start_Faultcode);
      Append (Message_Body, P.Faultcode);
      Append (Message_Body, End_Faultcode & NL);

      --  Faultstring

      Append (Message_Body, Start_Faultstring);
      Append (Message_Body, P.Faultstring);
      Append (Message_Body, End_Faultstring & NL);

      --  End Fault Env

      Append (Message_Body, End_Fault_Env & NL);

      return Message_Body;
   end Image;

   ---------------------
   -- Must_Understand --
   ---------------------

   function Must_Understand (Subname : in String := "") return String is
   begin
      return Faultcode (Must_Understand_Faultcode, Subname);
   end Must_Understand;

   ------------
   -- Server --
   ------------

   function Server (Subname : in String := "") return String is
   begin
      return Faultcode (Server_Faultcode, Subname);
   end Server;

   ----------------------
   -- Version_Mismatch --
   ----------------------

   function Version_Mismatch (Subname : in String := "") return String is
   begin
      return Faultcode (Version_Mismatch_Faultcode, Subname);
   end Version_Mismatch;

end SOAP.Message.Error;
