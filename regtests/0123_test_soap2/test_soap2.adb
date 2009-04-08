------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2009, AdaCore                     --
--                                                                          --
--  This library is free software; you can redistribute it and/or modify    --
--  it under the terms of the GNU General Public License as published by    --
--  the Free Software Foundation; either version 2 of the License, or (at   --
--  your option) any later version.                                         --
--                                                                          --
--  This library is distributed in the hope that it will be useful, but     --
--  WITHOUT ANY WARRANTY; without even the implied warranty of              --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU       --
--  General Public License for more details.                                --
--                                                                          --
--  You should have received a copy of the GNU General Public License       --
--  along with this library; if not, write to the Free Software Foundation, --
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.          --
--                                                                          --
--  As a special exception, if other files instantiate generics from this   --
--  unit, or you link this unit with other files to produce an executable,  --
--  this  unit  does not  by itself cause  the resulting executable to be   --
--  covered by the GNU General Public License. This exception does not      --
--  however invalidate any other reasons why the executable file  might be  --
--  covered by the  GNU Public License.                                     --
------------------------------------------------------------------------------

with Ada.Text_IO;
with Ada.Exceptions;

with AWS.Server;
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

procedure Test_SOAP2 is

   use Ada;
   use Ada.Text_IO;
   use AWS;

   function CB      (Request : Status.Data) return Response.Data;
   function SOAP_CB (Request : Status.Data) return Response.Data;

   task Server is
      entry Start;
      entry Started;
      entry Stopped;
   end Server;

   HTTP : AWS.Server.HTTP;

   --------
   -- CB --
   --------

   function CB (Request : Status.Data) return Response.Data is
      SOAP_Action : constant String := Status.SOAPAction (Request);
   begin
      if SOAP_Action = "/soap_demo" then
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
      Payload := SOAP.Message.Payload.Build ("string_length", P_Set);

      declare
         Response     : constant SOAP.Message.Response.Object'Class :=
           SOAP.Client.Call
           ("http://localhost:6544/soap_demo", Payload, "/soap_demo");

         R_Parameters : constant SOAP.Parameters.List
           := SOAP.Message.Parameters (Response);

         Result : constant Integer
           := SOAP.Parameters.Get (R_Parameters, "result");
      begin
         Text_IO.Put_Line
           ("Expexted = " & Integer'Image ((Str'Length))
            & " ; Result = " & Integer'Image (Result));
         Text_IO.New_Line;
      end;
   end Request;

   ------------
   -- Server --
   ------------

   task body Server is
   begin
      accept Start;

      AWS.Server.Start
        (HTTP, "soap_demo",
         CB'Unrestricted_Access, Port => 6544, Max_Connection => 5);

      Put_Line ("Server started");
      New_Line;

      accept Started;

      select
         accept Stopped;
      or
         delay 5.0;
         Put_Line ("Too much time to do the job !");
      end select;

      AWS.Server.Shutdown (HTTP);
   exception
      when E : others =>
         Put_Line ("Server Error " & Exceptions.Exception_Information (E));
   end Server;

   -------------
   -- SOAP_CB --
   -------------

   function SOAP_CB (Request : Status.Data) return Response.Data is
      use SOAP.Types;
      use SOAP.Parameters;

      Payload      : constant SOAP.Message.Payload.Object
        := SOAP.Message.XML.Load_Payload (AWS.Status.Payload (Request));

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

         R_Parameters := +I (Str'Length, "result");
      end;

      SOAP.Message.Set_Parameters (Response, R_Parameters);

      return SOAP.Message.Response.Build (Response);
   end SOAP_CB;

begin
   Put_Line ("Start main, wait for server to start...");

   Server.Start;
   Server.Started;

   Request ("simple");
   Request (" simple");
   Request ("simple ");
   Request (" simple ");
   Request ("one_two");
   Request ("with two spaces");
   Request ("a string  with   many    space");
   Request (" a string  with   many    space ");

   Server.Stopped;

exception
   when E : others =>
      Put_Line ("Main Error " & Exceptions.Exception_Information (E));
end Test_SOAP2;
