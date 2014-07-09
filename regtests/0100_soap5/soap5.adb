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

with Ada.Text_IO;

with SOAP.Message.Response;
with SOAP.Message.XML;

procedure SOAP5 is

   use Ada;

   Mess : aliased constant String :=
     "<env:Envelope xmlns:env=""http://schemas.xmlsoap.org/soap/envelope/"""
     & " xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance"""
     & " xmlns:soapenc=""http://schemas.xmlsoap.org/soap/encoding/"""
     & " xmlns:xsd=""http://www.w3.org/2001/XMLSchema"">"
     & "<env:Header/>"
     & "<env:Body"
     & " env:encodingStyle=""http://schemas.xmlsoap.org/soap/encoding/"">"
     & "<m:isUserInBlackListResponse"
     & " xmlns:m=""http://pfgui:11001/BlackListService"">"
     & "<result xsi:type=""xsd:boolean"">false</result>"
     & "</m:isUserInBlackListResponse></env:Body></env:Envelope>";

   Resp : constant SOAP.Message.Response.Object'Class
     := SOAP.Message.XML.Load_Response (Mess);

begin
   Text_IO.Put_Line (SOAP.Message.XML.Image (Resp));
end SOAP5;
