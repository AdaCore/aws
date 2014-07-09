------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2004-2014, AdaCore                     --
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

--  Same as Test_SOAP but with a persistent connection. Check also that SOAP
--  and non-SOAP request are handled properly in this case.

with Ada.Exceptions;
with Ada.Text_IO;

with AWS.Client;
with AWS.Messages;
with AWS.MIME;
with AWS.Parameters;
with AWS.Response;
with AWS.Server.Status;
with AWS.Status;
with AWS.Utils;

with SOAP.Client;
with SOAP.Message.Payload;
with SOAP.Message.Response;
with SOAP.Message.XML;
with SOAP.Parameters;
with SOAP.Types;

package body S_Test_SOAP5_Pack is

   use Ada;
   use Ada.Text_IO;
   use AWS;

   function CB      (Request : Status.Data) return Response.Data;
   function SOAP_CB (Request : Status.Data) return Response.Data;

   HTTP       : AWS.Server.HTTP;

   Connection : Client.HTTP_Connection;

   --------
   -- CB --
   --------

   function CB (Request : Status.Data) return Response.Data is
      SOAPAction : constant String := Status.SOAPAction (Request);
   begin
      Put_Line ("SOAPAction : " & SOAPAction);

      if SOAPAction = "/add" or else SOAPAction = "/mul" then
         return SOAP_CB (Request);

      elsif SOAPAction = "" then
         return Response.Build
           (MIME.Text_HTML,
            "This is not a SOAP request : " & Status.URI (Request));

      else
         Put_Line ("This is not a known SOAP request : " & SOAPAction);
         return Response.Build
           (MIME.Text_HTML, SOAPAction & " not handler", Messages.S404);
      end if;
   end CB;

   -------------
   -- Request --
   -------------

   procedure Request
     (Proc       : String;
      X, Y       : Integer;
      SOAPAction : String)
   is
      use SOAP.Types;
      use type SOAP.Parameters.List;

      P_Set   : constant SOAP.Parameters.List := +I (X, "x") & I (Y, "y");
      Payload : SOAP.Message.Payload.Object;
   begin
      Payload := SOAP.Message.Payload.Build (Proc, P_Set);

      declare
         Response     : constant SOAP.Message.Response.Object'Class :=
           SOAP.Client.Call (Connection, SOAPAction, Payload);

         R_Parameters : constant SOAP.Parameters.List
           := SOAP.Message.Parameters (Response);

         Result : constant Integer
           := SOAP.Parameters.Get (R_Parameters, "result");
      begin
         Text_IO.Put_Line ("Result = " & Integer'Image (Result));
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
      Put_Line ("SOAP_Proc : " & SOAP_Proc);

      Response := SOAP.Message.Response.From (Payload);

      declare
         X : constant Integer := SOAP.Parameters.Get (Parameters, "x");
         Y : constant Integer := SOAP.Parameters.Get (Parameters, "y");
      begin
         if SOAP_Proc = "multProc" then
            R_Parameters := +I (X * Y, "result");
         elsif SOAP_Proc = "addProc" then
            R_Parameters := +I (X + Y, "result");
         end if;
      end;

      SOAP.Message.Set_Parameters (Response, R_Parameters);

      return SOAP.Message.Response.Build (Response);
   end SOAP_CB;

   ---------
   -- Run --
   ---------

   procedure Run (Security : Boolean) is
   begin
      Put_Line ("Start main, wait for server to start...");

      Server.Start
        (HTTP, "soap_demo",
         CB'Access,
         Security       => Security,
         Port           => 0,
         Max_Connection => 5);

      Put_Line ("Server started");
      New_Line;

      Client.Create
        (Connection, Server.Status.Local_URL (HTTP) & "/soap_demo");

      Request ("multProc", 2, 3, "/mul");
      Request ("multProc", 9, 9, "/mul");
      Request ("multProc", 10, 73, "/mul");

      declare
         Resp : Response.Data;
      begin
         Client.Get (Connection, Resp, "/this/resource");
         Text_IO.Put_Line ("Get : " & Response.Message_Body (Resp));
      end;

      Request ("addProc", 2, 3, "/add");
      Request ("addProc", 9, 9, "/add");
      Request ("addProc", 10, 73, "/add");

      Client.Close (Connection);

      Server.Shutdown (HTTP);

   exception
      when E : others =>
         Put_Line ("Main Error " & Exceptions.Exception_Information (E));
         Server.Shutdown (HTTP);
   end Run;

end S_Test_SOAP5_Pack;
