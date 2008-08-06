------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2003-2008, AdaCore                     --
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

--  SOAP/WSDL test

with Ada.Text_IO;

with AWS.MIME;
with AWS.Response;
with AWS.Server;
with AWS.Status;
with SOAP.Utils;

with R_Hello_Demo.Client;
with R_Hello_Demo.Server;

procedure Test_WSDL is

   use AWS;

   H_Server : Server.HTTP;

   procedure WSDL_Demo_Client is
      use Ada;
   begin
      Text_IO.Put_Line (R_Hello_Demo.Client.sayHello (Firstname => "AWS"));
   end WSDL_Demo_Client;

   function sayHello (Firstname : in String) return String;

   -------------
   -- SOAP_CB --
   -------------

   function SOAP_CB is new R_Hello_Demo.Server.sayHello_CB (sayHello);

   function SOAP_Wrapper is new SOAP.Utils.SOAP_Wrapper (SOAP_CB);

   --------
   -- CB --
   --------

   function CB (Request : in Status.Data) return Response.Data is
      SOAPAction : constant String := Status.SOAPAction (Request);
   begin
      if SOAPAction = "sayHello" then
         return SOAP_Wrapper (Request);
      else
         return Response.Build (MIME.Text_HTML, "<p>Not a SOAP request");
      end if;
   end CB;

   --------------
   -- sayHello --
   --------------

   function sayHello (Firstname : in String) return String is
   begin
      return "Hello " & Firstname & " and welcome!";
   end sayHello;

begin
   Server.Start
     (H_Server, "WSDL Hello demo",
      CB'Unrestricted_Access,
      Port => R_Hello_Demo.Server.Port);

   WSDL_Demo_Client;

   Server.Shutdown (H_Server);
end Test_WSDL;
