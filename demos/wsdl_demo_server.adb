------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                            Copyright (C) 2003                            --
--                                ACT-Europe                                --
--                                                                          --
--  Authors: Dmitriy Anisimkov - Pascal Obry                                --
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

--  $Id$

--  The famous Hello Word demo, using AWS/SOAP framework.
--
--  SOAP Server

with AWS.MIME;
with AWS.Response;
with AWS.Server;
with AWS.Status;

with Hello_Demo.Server;

procedure WSDL_Demo_Server is

   use AWS;

   function sayHello (Firstname : in String) return String;

   -------------
   -- SOAP_CB --
   -------------

   function SOAP_CB is new Hello_Demo.Server.sayHello_CB (sayHello);

   --------
   -- CB --
   --------

   function CB (Request : in Status.Data) return Response.Data is
      SOAPAction : constant String := Status.SOAPAction (Request);
   begin
      if SOAPAction = "sayHello" then
         return SOAP_CB (Request);
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

   H_Server : Server.HTTP;

begin
   Server.Start
     (H_Server, "WSDL Hello demo",
      CB'Unrestricted_Access,
      Port => Hello_Demo.Server.Port);

   Server.Wait (Server.Q_Key_Pressed);
end WSDL_Demo_Server;
