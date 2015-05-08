------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2015, AdaCore                     --
--                                                                          --
--  This library is free software;  you can redistribute it and/or modify   --
--  it under terms of the  GNU General Public License  as published by the  --
--  Free Software  Foundation;  either version 3,  or (at your  option) any --
--  later version. This library is distributed in the hope that it will be  --
--  useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                    --
--                                                                          --
--  As a special exception under Section 7 of GPL version 3, you are        --
--  granted additional permissions described in the GCC Runtime Library     --
--  Exception, version 3.1, as published by the Free Software Foundation.   --
--                                                                          --
--  You should have received a copy of the GNU General Public License and   --
--  a copy of the GCC Runtime Library Exception along with this program;    --
--  see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see   --
--  <http://www.gnu.org/licenses/>.                                         --
--                                                                          --
--  As a special exception, if other files instantiate generics from this   --
--  unit, or you link this unit with other files to produce an executable,  --
--  this  unit  does not  by itself cause  the resulting executable to be   --
--  covered by the GNU General Public License. This exception does not      --
--  however invalidate any other reasons why the executable file  might be  --
--  covered by the  GNU Public License.                                     --
------------------------------------------------------------------------------

with SOAP.Name_Space;
with SOAP.Types;
with SOAP.Utils;

package body SOAP.Message.Response.Error is

   package NS renames SOAP.Name_Space;

   Version_Mismatch_Faultcode : constant String := "VersionMismatch";
   Must_Understand_Faultcode  : constant String := "MustUnderstand";
   Client_Faultcode           : constant String := "Client";
   Server_Faultcode           : constant String := "Server";
   SOAPENV                    : constant String := NS.Name (NS.SOAPENV);
   Start_Fault_Env            : constant String := "<" & SOAPENV & ":Fault>";
   End_Fault_Env              : constant String := "</" & SOAPENV & ":Fault>";

   function Fault_Code (Name, Subname : String) return Faultcode;
   --  Returns the Faultcode for Name and Subname. If Subname is empty it
   --  returns Name otherwise it returns Name & '.' & Subname.

   -----------
   -- Build --
   -----------

   function Build
     (Faultcode   : Error.Faultcode;
      Faultstring : String)
      return Object
   is
      use SOAP.Types;
      use type SOAP.Parameters.List;

      O : Object;
      P : SOAP.Parameters.List;
   begin
      --  Set Wrapper Name

      Set_Wrapper_Name (O, "Fault");

      --  Set Faultcode and Faultstring

      P := P
        & S (String (Faultcode), "faultcode")
        & S (Faultstring, "faultstring");

      --  Set parameters for this error object

      Set_Parameters (O, P);

      return O;
   end Build;

   ------------
   -- Client --
   ------------

   function Client (Subname : String := "") return Faultcode is
   begin
      return Fault_Code (Client_Faultcode, Subname);
   end Client;

   ----------------
   -- Fault_Code --
   ----------------

   function Fault_Code (Name, Subname : String) return Faultcode is
   begin
      if Subname = "" then
         return Faultcode (Name);

      else
         return Faultcode (Name & '.' & Subname);
      end if;
   end Fault_Code;

   ----------
   -- From --
   ----------

   overriding function From (P : Message.Payload.Object) return Object is
      pragma Unreferenced (P);
      N : Object;
   begin
      return N;
   end From;

   --------------
   -- Is_Error --
   --------------

   overriding function Is_Error (E : Object) return Boolean is
      pragma Unreferenced (E);
   begin
      return True;
   end Is_Error;

   ---------------------
   -- Must_Understand --
   ---------------------

   function Must_Understand (Subname : String := "") return Faultcode is
   begin
      return Fault_Code (Must_Understand_Faultcode, Subname);
   end Must_Understand;

   ------------
   -- Server --
   ------------

   function Server (Subname : String := "") return Faultcode is
   begin
      return Fault_Code (Server_Faultcode, Subname);
   end Server;

   ----------------------
   -- Version_Mismatch --
   ----------------------

   function Version_Mismatch (Subname : String := "") return Faultcode is
   begin
      return Fault_Code (Version_Mismatch_Faultcode, Subname);
   end Version_Mismatch;

   ---------------
   -- XML_Image --
   ---------------

   overriding function XML_Image
     (E      : Object;
      Schema : WSDL.Schema.Definition :=
                 WSDL.Schema.Empty) return Unbounded_String
   is
      pragma Unreferenced (Schema);

      NL           : constant String := ASCII.CR & ASCII.LF;
      Message_Body : Unbounded_String;

   begin
      --  Close body

      Append (Message_Body, ">" & NL);

      --  Fault Env

      Append (Message_Body, Start_Fault_Env & NL);

      --  Fault's parameters

      declare
         P : constant SOAP.Parameters.List := Parameters (E);
      begin
         for K in 1 .. SOAP.Parameters.Argument_Count (P) loop
            declare
               P_K    : constant SOAP.Types.Object'Class :=
                          SOAP.Parameters.Argument (P, K);
               P_Name : constant String := SOAP.Types.Name (P_K);
            begin
               Append
                 (Message_Body,
                  "   "
                  & Utils.Tag (P_Name, Start => True)
                  & Types.Image (P_K)
                  & Utils.Tag (P_Name, Start => False)
                  & NL);
            end;
         end loop;
      end;

      --  End Fault Env

      Append (Message_Body, End_Fault_Env & NL);

      return Message_Body;
   end XML_Image;

end SOAP.Message.Response.Error;
