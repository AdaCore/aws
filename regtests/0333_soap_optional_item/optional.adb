------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                        Copyright (C) 2021, AdaCore                       --
--                                                                          --
--  This is free software;  you can redistribute it  and/or modify it       --
--  under terms of the  GNU General Public License as published  by the     --
--  Free Software  Foundation;  either version 3,  or (at your option) any  --
--  later version.  This software is distributed in the hope  that it will  --
--  be useful, but WITHOUT ANY WARRANTY;  without even the implied warranty --
--  of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU     --
--  General Public License for  more details.                               --
--                                                                          --
--  You should have  received  a copy of the GNU General  Public  License   --
--  distributed  with  this  software;   see  file COPYING3.  If not, go    --
--  to http://www.gnu.org/licenses for a complete copy of the license.      --
------------------------------------------------------------------------------

with Ada.Calendar;
with Ada.Integer_Text_IO;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with AWS.Config.Set;
with AWS.MIME;
with AWS.Net;
with AWS.Response;
with AWS.Server.Status;
with AWS.Status;
with AWS.Translator;
with SOAP.Client;
with SOAP.Message.Payload;
with SOAP.Message.Response;
with SOAP.Message.XML;

with Toptional.Client;
with Toptional.Server;
with Toptional.Types;

procedure Optional is

   use Ada;
   use Ada.Strings.Unbounded;
   use AWS;

   use Toptional.Types;

   H_Server : Server.HTTP;

   function "+" (Str : String) return Unbounded_String
     renames To_Unbounded_String;

   package FIO is new Text_IO.Float_IO (Float);

   ------------
   -- Output --
   ------------

   procedure Output (S : SOAPStruct_Type) is
   begin
      Integer_Text_IO.Put (S.varInt); Text_IO.New_Line;
      FIO.Put (S.VarFloat, Exp => 0, Aft => 2); Text_IO.New_Line;
      Text_IO.Put_Line (To_String (S.varString));
   end Output;

   ---------------------
   -- T_echoString_CB --
   ---------------------

   function T_echoString_CB (inputString : String) return String is
   begin
      return inputString;
   end T_echoString_CB;

   function echoString_CB is
      new Toptional.Server.echoString_CB (T_echoString_CB);

   ------------------
   -- T_echoString --
   ------------------

   procedure T_echoString is
      Res : constant String := Toptional.Client.echoString
              ("This is the real value for the string!");
   begin
      Text_IO.Put_Line ("Echo String");

      Text_IO.Put_Line (Res);
      Text_IO.New_Line;
   end T_echoString;

   ---------------------
   -- T_echoStruct_CB --
   ---------------------

   function T_echoStruct_CB
     (inputStruct : SOAPStruct_Type)
      return echoStruct_Result is
   begin
      return inputStruct;
   end T_echoStruct_CB;

   function echoStruct_CB is
     new Toptional.Server.echoStruct_CB (T_echoStruct_CB);

   ------------------
   -- T_echoStruct --
   ------------------

   procedure T_echoStruct is
      Struct : constant SOAPStruct_Type := (6, 6.6, +"666");
      Res    : constant echoStruct_Result :=
                 Toptional.Client.echoStruct (Struct);
   begin
      Text_IO.Put_Line ("Echo Struct");
      Output (Res);
      Text_IO.New_Line;
   end T_echoStruct;

   --------
   -- CB --
   --------

   function CB (Request : Status.Data) return Response.Data is
      SOAPAction : constant String := Status.SOAPAction (Request);
      P_Str      : aliased constant String := AWS.Status.Payload (Request);
      Payload    : constant SOAP.Message.Payload.Object :=
                     SOAP.Message.XML.Load_Payload
                       (P_Str, Schema => Toptional.Schema);
      Proc       : constant String :=
                     SOAP.Message.Payload.Procedure_Name (Payload);
   begin
      Text_IO.Put_Line ("@ " & Proc);

      if Proc = "echoString" then
         return echoString_CB (SOAPAction, Payload, Request);

      elsif Proc = "echoStruct" then
         return echoStruct_CB (SOAPAction, Payload, Request);

      else
         return Response.Build
           (MIME.Text_HTML, "Not a SOAP request, Proc=" & Proc);
      end if;
   end CB;

   CNF : Config.Object := Config.Get_Current;

begin
   Config.Set.Server_Name     (CNF, "WSDL Optional Server");
   Config.Set.Server_Host     (CNF, "localhost");
   Config.Set.Server_Port     (CNF, Toptional.Server.Port);
   Config.Set.Protocol_Family (CNF, "FAMILY_INET");

   Server.Start (H_Server, CB'Unrestricted_Access, CNF);

   if Net.IPv6_Available then
      Server.Add_Listening
        (H_Server, "localhost", Toptional.Server.Port, Net.FAMILY_INET6);
   end if;

   T_echoString;
   T_echoStruct;

   --  Now check struct with a xsi:nil (optional value)

   declare
      N   : constant String := String'(1 => ASCII.CR) & ASCII.LF;
      Mes : aliased constant String :=
               "<?xml version='1.0' encoding='UTF-8'?>" & N
               & "<soapenv:Envelope soapenv:encodingStyle=""http://schemas.xmlsoap.org/soap/encoding/"" xmlns:soap-enc=""http://schemas.xmlsoap.org/soap/encoding/"" xmlns:soapenv=""http://schemas.xmlsoap.org/soap/envelope/"" xmlns:xsd=""http://www.w3.org/2001/XMLSchema"" xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance"">" & N
               & "<soapenv:Body>" & N
               & "<ns2:echoStruct xmlns:ns2=""http://nsoptional.org/"""
               & " xmlns:ns1=""http://nsoptional.org/xsd"">" & N
               & "<ns1:inputStruct xsi:type=""ns1:SOAPStruct"">"
               &  "<ns1:varInt xsi:type=""xsd:int"">7</ns1:varInt>"
               &  "<ns1:varFloat xsi:type=""xsd:float"">7.0</ns1:varFloat>"
               &  "<ns1:varString xsi:type=""xsd:string"" xsi:nil=""true""/>"
               & "</ns1:inputStruct>"
               & "</ns2:echoStruct>" & N
               & "</soapenv:Body>" & N
               & "</soapenv:Envelope>";
      Pl : constant SOAP.Message.Payload.Object :=
             SOAP.Message.XML.Load_Payload (Mes);
      R  : constant SOAP.Message.Response.Object'Class :=
             SOAP.Client.Call
               (URL        => AWS.Server.Status.Local_URL (H_Server),
                P          => Pl,
                SOAPAction => "http://nsoptional.org/");
   begin
      Text_IO.Put_Line (SOAP.Message.XML.Image (Pl));
      Text_IO.Put_Line (SOAP.Message.XML.Image (R));
   end;

   Server.Shutdown (H_Server);
end Optional;
