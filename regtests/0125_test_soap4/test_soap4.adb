------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2014, AdaCore                     --
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

with Ada.Text_IO;
with Ada.Exceptions;

with AWS.Server.Status;
with AWS.Client;
with AWS.Status;
with AWS.MIME;
with AWS.Response;
with AWS.Parameters;
with AWS.Messages;

with SOAP.Client;
with SOAP.Message.Payload;
with SOAP.Message.Response;
with SOAP.Message.XML;
with SOAP.Parameters;
with SOAP.Types;

procedure Test_SOAP4 is

   use Ada;
   use Ada.Text_IO;
   use AWS;

   function CB      (Request : Status.Data) return Response.Data;
   function SOAP_CB (Request : Status.Data) return Response.Data;

   HTTP : AWS.Server.HTTP;

   --------
   -- CB --
   --------

   function CB (Request : Status.Data) return Response.Data is
      SOAP_Action : constant String := Status.SOAPAction (Request);
   begin
      if SOAP_Action = "/soap_accent" then
         return SOAP_CB (Request);

      else
         Put_Line ("This is not a known SOAP request : " & SOAP_Action);
         return Response.Build
           (MIME.Text_HTML, SOAP_Action & " not handler", Messages.S404);
      end if;
   end CB;

   -------------
   -- Request --
   -------------

   procedure Request (Str : String) is
      use SOAP.Types;
      use type SOAP.Parameters.List;

      P_Set   : constant SOAP.Parameters.List := +S (Str, "str");
      Payload : SOAP.Message.Payload.Object;
   begin
      Payload := SOAP.Message.Payload.Build ("echo", P_Set);

      declare
         Response     : constant SOAP.Message.Response.Object'Class :=
           SOAP.Client.Call
             (AWS.Server.Status.Local_URL (HTTP) & "/soap_demo", Payload,
              "/soap_accent");

         R_Parameters : constant SOAP.Parameters.List
           := SOAP.Message.Parameters (Response);

         Len    : constant Integer
           := SOAP.Parameters.Get (R_Parameters, "len");
         Result : constant String
           := SOAP.Parameters.Get (R_Parameters, "result");
      begin
         Text_IO.Put_Line
           ("Expexted = " & Integer'Image ((Str'Length))
              & " ; Result = " & Integer'Image (Len)
              & " [" & Result & ']');
         Text_IO.New_Line;
      end;
   end Request;

   -------------
   -- SOAP_CB --
   -------------

   function SOAP_CB (Request : Status.Data) return Response.Data is
      use SOAP.Types;
      use SOAP.Parameters;

      P_Str        : aliased constant String := AWS.Status.Payload (Request);
      Payload      : constant SOAP.Message.Payload.Object
        := SOAP.Message.XML.Load_Payload (P_Str);

      SOAP_Proc    : constant String
        := SOAP.Message.Payload.Procedure_Name (Payload);

      Parameters   : constant SOAP.Parameters.List
        := SOAP.Message.Parameters (Payload);

      Response     : SOAP.Message.Response.Object;
      R_Parameters : SOAP.Parameters.List;

   begin
      Text_IO.Put_Line ("Proc " & SOAP_Proc);

      Response := SOAP.Message.Response.From (Payload);

      declare
         Str : constant String := SOAP.Parameters.Get (Parameters, "str");
      begin
         Text_IO.Put_Line ("S: [" & Str & "]");

         R_Parameters := +I (Str'Length, "len")
           & S (Str, "result");
      end;

      SOAP.Message.Set_Parameters (Response, R_Parameters);

      return SOAP.Message.Response.Build (Response);
   end SOAP_CB;

begin
   Put_Line ("Start main, wait for server to start...");

   Server.Start
     (HTTP, "soap_demo", CB'Unrestricted_Access, Port => 0,
      Max_Connection => 5);

   Put_Line ("Server started");
   New_Line;

   Request ("A > B");
   Request ("B < A");
   Request ("A >= B and C <= A");
   Request ("A & B ? C # E $ * % &#FF;");
   Request ("<project>AWS</project>");
   Request ("<tag1 att1=""toto""><tag2 att2=""titi"" "
              & "att22=""tutu"">X</tag2></tag1>");
   Request ("<tag1><tag2>This is not XML</tag1></tag2>");
   Request ("<empty_project/>");

   Server.Shutdown (HTTP);

exception
   when E : others =>
      Put_Line ("Main Error " & Exceptions.Exception_Information (E));
      Server.Shutdown (HTTP);
end Test_SOAP4;
