------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2004-2012, AdaCore                     --
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

procedure SOAP1 is

   use Ada;

   Mess : constant String :=
     "<?xml version=""1.0"" encoding=""UTF-8"" standalone=""no"" ?>"
     & "<soap:Envelope "
     & "xmlns:soap=""http://schemas.xmlsoap.org/soap/envelope/"">"
     & "<soap:Body>"
     & "<m:MultiplyResponse xmlns:m=""http://tempuri.org/message/"">"
     & "<Result>12 13</Result>"
     & "</m:MultiplyResponse>"
     & "</soap:Body>"
     & "</soap:Envelope>";

   Resp : constant SOAP.Message.Response.Object'Class
     := SOAP.Message.XML.Load_Response (Mess);

begin
   Text_IO.Put_Line (SOAP.Message.XML.Image (Resp));
end SOAP1;
