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

procedure SOAP6 is

   use Ada;

   Mess : aliased constant String :=
     "<soapenv:Envelope"
     & " xmlns:soapenv=""http://schemas.xmlsoap.org/soap/envelope/"""
     & " xmlns:xsd=""http://www.w3.org/1999/XMLSchema"""
     & " xmlns:xsi=""http://www.w3.org/1999/XMLSchema-instance"">"
     & "<soapenv:Header>"
     & "<ns2:sessionID soapenv:mustUnderstand=""0"""
     & " xmlns:ns1=""http://www.w3.org/2001/XMLSchema"""
     & " xmlns:ns2=""http://xml.apache.org/axis/session"""
     & " xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance"""
     & " xsi:type=""ns1:long"">-5990719852434337268"
     & "</ns2:sessionID>"
     & "</soapenv:Header>"
     & "<soapenv:Body>"
     & "<ns3:getLastResponse"
     & " soapenv:encodingStyle=""http://schemas.xmlsoap.org/soap/encoding/"""
     & " xmlns:ns3=""urn:datafeed.dukascopy.com"">"
     & " <ns3:getLastReturn soapenc:arrayType=""ns3:Candle[1]"""
     & " xmlns:soapenc=""http://schemas.xmlsoap.org/soap/encoding/"""
     & " xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance"""
     & " xsi:type=""soapenc:Array"">"
     & " <item href=""#id0""></item>"
     & "</ns3:getLastReturn>"
     & "</ns3:getLastResponse>"
     & "<multiRef id=""id0"" soapenc:root=""0"""
     & " soapenv:encodingStyle=""http://schemas.xmlsoap.org/soap/encoding/"""
     & " xmlns:ns4=""urn:datafeed.dukascopy.com"""
     & " xmlns:soapenc=""http://schemas.xmlsoap.org/soap/encoding/"""
     & " xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance"""
     & " xsi:type=""ns4:Candle"">"
     & "<artificial xmlns:ns5=""http://www.w3.org/2001/XMLSchema"""
     & " xsi:type=""ns5:boolean"">false</artificial>"
     & "<closePrice xmlns:ns6=""http://www.w3.org/2001/XMLSchema"""
     & " xsi:type=""ns6:double"">1.7465</closePrice>"
     & "<flat xmlns:ns7=""http://www.w3.org/2001/XMLSchema"""
     & " xsi:type=""ns7:boolean"">false</flat>"
     & "<id xmlns:ns8=""http://www.w3.org/2001/XMLSchema"""
     & " xsi:type=""ns8:int"">533</id>"
     & "<maxPrice xmlns:ns9=""http://www.w3.org/2001/XMLSchema"""
     & " xsi:type=""ns9:double"">1.7465</maxPrice>"
     & "<minPrice xmlns:ns10=""http://www.w3.org/2001/XMLSchema"""
     & " xsi:type=""ns10:double"">1.7459</minPrice>"
     & "<openPrice xmlns:ns11=""http://www.w3.org/2001/XMLSchema"""
     & " xsi:type=""ns11:double"">1.7465</openPrice>"
     & "<periodType xmlns:ns12=""http://www.w3.org/2001/XMLSchema"""
     & " xsi:type=""ns12:int"">0</periodType>"
     & "<time xmlns:ns13=""http://www.w3.org/2001/XMLSchema"""
     & " xsi:type=""ns13:long"">1089143140</time>"
     & "<timeIndex xmlns:ns14=""http://www.w3.org/2001/XMLSchema"""
     & " xsi:type=""ns14:int"">108914314</timeIndex>"
     & "<volume xmlns:ns15=""http://www.w3.org/2001/XMLSchema"""
     & " xsi:type=""ns15:long"">0</volume>"
     & "</multiRef>"
     & "</soapenv:Body>"
     & "</soapenv:Envelope>";

   Resp : constant SOAP.Message.Response.Object'Class
     := SOAP.Message.XML.Load_Response (Mess);

begin
   Text_IO.Put_Line (SOAP.Message.XML.Image (Resp));
end SOAP6;
