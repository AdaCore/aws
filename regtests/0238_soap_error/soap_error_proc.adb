------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                       Copyright (C) 2014, AdaCore                        --
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

with Ada.Exceptions;
with Ada.Text_IO;

with AWS.Messages;
with AWS.MIME;
with AWS.Parameters;
with AWS.Response;
with AWS.Server.Status;
with AWS.Status;

with SOAP.Client;
with SOAP.Message.Payload;
with SOAP.Message.Response.Error;
with SOAP.Parameters;
with SOAP.Types;

procedure SOAP_Error_Proc (Security : Boolean) is

   use Ada;
   use Ada.Text_IO;
   use AWS;

   function CB      (Request : Status.Data) return Response.Data;
   function SOAP_CB (Request : Status.Data) return Response.Data;

   HTTP : Server.HTTP;

   --------
   -- CB --
   --------

   function CB (Request : Status.Data) return Response.Data is
      SOAP_Action : constant String := Status.SOAPAction (Request);
   begin
      if SOAP_Action = "/soap_error" then
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

   procedure Request (Proc : String; X, Y : Integer) is
      use SOAP.Types;
      use type SOAP.Parameters.List;

      P_Set   : constant SOAP.Parameters.List := +I (X, "x") & I (Y, "y");
      Payload : SOAP.Message.Payload.Object;
   begin
      Payload := SOAP.Message.Payload.Build (Proc, P_Set);

      declare
         Response     : constant SOAP.Message.Response.Object'Class :=
                          SOAP.Client.Call
                            (AWS.Server.Status.Local_URL (HTTP)
                             & "/soap_error", Payload, "/soap_error");
         R_Parameters : constant SOAP.Parameters.List :=
                          SOAP.Message.Parameters (Response);
         Result       : constant String :=
                          SOAP.Parameters.Get (R_Parameters, "faultstring");
      begin
         Text_IO.Put_Line ("Result = " & Result);
      end;
   end Request;

   -------------
   -- SOAP_CB --
   -------------

   function SOAP_CB (Request : Status.Data) return Response.Data is
      R : constant Response.Data :=
            SOAP.Message.Response.Build
              (SOAP.Message.Response.Error.Build
                 (Faultcode   =>
                    Soap.Message.Response.Error.Server ("cannot respond"),
                  Faultstring => "testing HTTP code"));
   begin
      Text_IO.Put_Line
        ("HTTP status code : " & Messages.Image (Response.Status_Code (R)));
      return R;
   end SOAP_CB;

begin
   Put_Line ("Start main, wait for server to start...");

   Server.Start
     (HTTP, "soap_error", CB'Unrestricted_Access,
      Port           => 0,
      Max_Connection => 5,
      Security       => Security);

   Put_Line ("Server started");

   Request ("iserror", 2, 3);

   Server.Shutdown (HTTP);

exception
   when E : others =>
      Put_Line ("Main Error " & Exceptions.Exception_Information (E));
      Server.Shutdown (HTTP);
end SOAP_Error_Proc;
