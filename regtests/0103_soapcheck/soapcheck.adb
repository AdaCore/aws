------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                       Copyright (C) 2008, AdaCore                        --
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

with AWS.Config.Set;
pragma Elaborate_All (AWS.Config.Set);
with AWS.Server;
pragma Elaborate_All (AWS.Server);
with AWS.Status;
pragma Elaborate_All (AWS.Status);
with AWS.Response;
pragma Elaborate_All (AWS.Response);
with SOAP.Dispatchers.Callback;
pragma Elaborate_All (SOAP.Dispatchers.Callback);

with Pck_Service.CB;
pragma Elaborate_All (Pck_Service.CB);
with Pck_Service.Server;
pragma Elaborate_All (Pck_Service.Server);

with AWS.Client;
with AWS.Utils;

with SOAP.Client;
with SOAP.Message.Response;
with SOAP.Message.Payload;
with SOAP.Name_Space;
with SOAP.Parameters;
with SOAP.Types;

with Pck_Service.Client;
with Pck_Service.Types;
with Pck;

procedure Soapcheck is

   use Ada;
   use Ada.Exceptions;
   use AWS;
   use SOAP.Parameters;
   use SOAP.Types;

   Port : Integer := 9876;

   function URL return String;

   function URL return String is
   begin
      return "http://localhost:" & Utils.Image (Port);
   end URL;

   function CB (Request : in Status.Data) return Response.Data
   is
      R : Response.Data;
   begin
      return R;
   end CB;

   function To_SOAP_Object
     (R    : in Pck_Service.Types.Rec_Type;
      Name : in String := "item")
      return SOAP.Types.SOAP_Record
   is
      Result : SOAP.Types.SOAP_Record;
   begin
      Result := SOAP.Types.R
        ((+SOAP.Types.S (Integer'Image (R.V), "V"),
          +SOAP.Types.S (Integer'Image (Integer (R.S)), "S"),
          +SOAP.Types.E (Pck_Service.Types.Image (R.C), "Color", "C")),
         Name, "Rec");
      SOAP.Types.Set_Name_Space
        (Result,
         SOAP.Name_Space.Create
           ("n1",
            "http://soapaws/Pck_pkg/"));
      return Result;
   end To_SOAP_Object;

   ----------
   -- Call --
   ----------

   procedure Call
     (R        : in Pck_Service.Types.Rec_Type;
      Endpoint : in String := URL;
      Timeouts : in AWS.Client.Timeouts_Values := Pck_Service.Timeouts)
   is
      P_Set   : SOAP.Parameters.List;
      Payload : SOAP.Message.Payload.Object;
   begin
      --  Set parameters
      P_Set := +To_SOAP_Object (R, "R");
      Payload := SOAP.Message.Payload.Build
        ("Call", P_Set,
         SOAP.Name_Space.Create ("tns", "http://soapaws/Pck_def/"));

      declare
         Response : constant SOAP.Message.Response.Object'Class
           := SOAP.Client.Call
                (Endpoint, Payload, "Call",
                 Timeouts   => Timeouts);
         R_Param  : constant SOAP.Parameters.List
           := SOAP.Message.Parameters (Response);
      begin
         if SOAP.Message.Response.Is_Error (Response) then
            raise SOAP.SOAP_Error with
               SOAP.Parameters.Get (R_Param, "faultstring");
         end if;
      end;
   end Call;

   -----------
   -- Print --
   -----------

   procedure Print
     (X        : in Integer;
      Name     : in String;
      Endpoint : in String := URL;
      Timeouts : in AWS.Client.Timeouts_Values := Pck_Service.Timeouts)
   is
      P_Set   : SOAP.Parameters.List;
      Payload : SOAP.Message.Payload.Object;
   begin
      --  Set parameters
      P_Set := +SOAP.Types.S (Integer'Image (X), "X")
         & SOAP.Types.S (Name, "Name");
      Payload := SOAP.Message.Payload.Build
        ("Print", P_Set,
         SOAP.Name_Space.Create ("tns", "http://soapaws/Pck_def/"));

      declare
         Response : constant SOAP.Message.Response.Object'Class
           := SOAP.Client.Call
                (Endpoint, Payload, "Print",
                 Timeouts   => Timeouts);
         R_Param  : constant SOAP.Parameters.List
           := SOAP.Message.Parameters (Response);
      begin
         if SOAP.Message.Response.Is_Error (Response) then
            raise SOAP.SOAP_Error with
               SOAP.Parameters.Get (R_Param, "faultstring");
         end if;
      end;
   end Print;

   WS   : AWS.Server.HTTP;
   Conf : Config.Object;
   Disp : Pck_Service.CB.Handler;

begin
   Config.Set.Server_Port (Conf, Port);
   Disp := SOAP.Dispatchers.Callback.Create
     (CB'Unrestricted_Access,
      Pck_Service.CB.SOAP_CB'Access);

   AWS.Server.Start (WS, Disp, Conf);

   begin
      Call
        (Pck_Service.Types.Rec_Type'
           (V => 12,
            S => 2,
            C => Pck.Red));
   exception
      when E : others =>
         Text_IO.Put_Line (Exception_Message (E));
   end;

   begin
      Print (67, "toto");
   exception
      when E : others =>
         Text_IO.Put_Line (Exception_Message (E));
   end;

   AWS.Server.Shutdown (WS);
end Soapcheck;
