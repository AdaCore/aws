------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2003-2012, AdaCore                     --
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
with Ada.Strings.Unbounded;

with AWS.Client;
with AWS.Config.Set;
with AWS.Messages;
with AWS.MIME;
with AWS.Parameters;
with AWS.Response;
with AWS.Server.Status;
with AWS.Status;
with AWS.Utils;

with SOAP.Client;
with SOAP.Message.Payload;
with SOAP.Message.Response.Error;
with SOAP.Message.XML;
with SOAP.Parameters;
with SOAP.Types;
with SOAP.Dispatchers.Callback;

procedure Disp_SOAP_Proc (Protocol : String) is

   use Ada;
   use Ada.Text_IO;
   use AWS;

   function CB (Request : Status.Data) return Response.Data;

   function SOAP_CB
     (SOAPAction : String;
      Payload    : SOAP.Message.Payload.Object;
      Request    : AWS.Status.Data)
      return AWS.Response.Data;

   procedure Request
     (Proc       : String;
      X, Y       : Integer;
      SOAPAction : String := "soap_demo");

   HTTP : AWS.Server.HTTP;

   Connect : aliased AWS.Client.HTTP_Connection;

   Config : AWS.Config.Object := AWS.Config.Default_Config;

   Answer : AWS.Response.Data;

   --------
   -- CB --
   --------

   function CB (Request : Status.Data) return Response.Data is
   begin
      Put_Line ("This is not SOAP request.");

      return Response.Build
           (MIME.Text_HTML, "Not SOAP request.", Messages.S404);
   end CB;

   -------------
   -- Request --
   -------------

   procedure Request
     (Proc       : String;
      X, Y       : Integer;
      SOAPAction : String := "soap_demo")
   is
      use SOAP.Types;
      use type SOAP.Parameters.List;

      P_Set   : constant SOAP.Parameters.List := +I (X, "x") & I (Y, "y");
      Payload : SOAP.Message.Payload.Object;
   begin
      Payload := SOAP.Message.Payload.Build (Proc, P_Set);

      declare
         Response     : constant SOAP.Message.Response.Object'Class
            := SOAP.Client.Call (Connect, SOAPAction, Payload);

         R_Parameters : constant SOAP.Parameters.List
           := SOAP.Message.Parameters (Response);
      begin
         if SOAP.Message.Response.Is_Error (Response) then
            Text_IO.Put_Line
              ("Fault Code = "
               & SOAP.Parameters.Get (R_Parameters, "faultcode"));
            Text_IO.Put_Line
              ("Fault Message = "
               & SOAP.Parameters.Get (R_Parameters, "faultstring"));
         else
            Text_IO.Put_Line
              ("Result = "
               & Integer'Image (SOAP.Parameters.Get
                                  (R_Parameters, "result")));
         end if;
      end;
   end Request;

   -------------
   -- SOAP_CB --
   -------------

   function SOAP_CB
     (SOAPAction : String;
      Payload    : SOAP.Message.Payload.Object;
      Request    : AWS.Status.Data)
      return AWS.Response.Data
   is
      use SOAP.Types;
      use SOAP.Parameters;

      SOAP_Proc    : constant String
        := SOAP.Message.Payload.Procedure_Name (Payload);

      Parameters   : constant SOAP.Parameters.List
        := SOAP.Message.Parameters (Payload);

      Response     : SOAP.Message.Response.Object;
      R_Parameters : SOAP.Parameters.List;

   begin
      if SOAPAction /= "soap_demo" then
         return SOAP.Message.Response.Build
           (SOAP.Message.Response.Error.Build
              (SOAP.Message.Response.Error.Client,
               "SOAPAction '" & SOAPAction & "' is not expected."));
      end if;

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

begin
   Put_Line ("Start main, wait for server to start...");

   AWS.Config.Set.Server_Port (Config, 0);
   AWS.Config.Set.Security    (Config, Protocol = "https");

   AWS.Server.Start
     (HTTP,
      Config     => Config,
      Dispatcher => SOAP.Dispatchers.Callback.Create
                      (CB'Unrestricted_Access,
                       SOAP_CB'Unrestricted_Access));

   Put_Line ("Server started");
   New_Line;

   AWS.Client.Create
     (Connection => Connect, Host => AWS.Server.Status.Local_URL (HTTP));

   Request ("multProc", 2, 3);
   Request ("multProc", 9, 9);
   Request ("multProc", 10, 73);

   AWS.Client.Get (Connect, Answer, "soap_demo");
   Request ("addProc", 2, 3, SOAPAction => "Wrong");

   Request ("addProc", 2, 3);
   Request ("addProc", 9, 9);
   Request ("addProc", 10, 73);

   AWS.Server.Shutdown (HTTP);

   AWS.Client.Close (Connect);

exception
   when E : others =>
      Put_Line ("Main Error " & Exceptions.Exception_Information (E));
end Disp_SOAP_Proc;
