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

with SOAP.Message.XML;

procedure SOAP1 is

   use Ada;
   use SOAP.Message;

   Mess : aliased constant String :=
     "<?xml version=""1.0"" encoding=""UTF-8"" standalone=""no"" ?>"
     & "<soap:Envelope "
     & "xmlns:soap=""http://schemas.xmlsoap.org/soap/envelope/"">"
     & "<soap:Body>"
     & "<m:MultiplyResponse xmlns:m=""http://tempuri.org/message/"">"
     & "<Result>12 13</Result>"
     & "</m:MultiplyResponse>"
     & "</soap:Body>"
     & "</soap:Envelope>";

   XML_RPC : aliased constant String := "<jobInfo>"
     & "<id>750M0000000VL33IAG</id>"
     & "<operation>query</operation>"
     & "<object>Account</object>"
     & "<createdById>005D0000002SuzhIAC</createdById>"
     & "<createdDate>2014-10-17T02:18:43.000Z</createdDate>"
     & "<systemModstamp>2014-10-17T02:18:43.000Z</systemModstamp>"
     & "<state>Open</state>"
     & "<concurrencyMode>Parallel</concurrencyMode>"
     & "<contentType>CSV</contentType>"
     & "<numberBatchesQueued>0</numberBatchesQueued>"
     & "<numberBatchesInProgress>0</numberBatchesInProgress>"
     & "<numberBatchesCompleted>0</numberBatchesCompleted>"
     & "<numberBatchesFailed>0</numberBatchesFailed>"
     & "<numberBatchesTotal>0</numberBatchesTotal>"
     & "<numberRecordsProcessed>0</numberRecordsProcessed>"
     & "<numberRetries>0</numberRetries>"
     & "<apiVersion>32.0</apiVersion>"
     & "<error>"
        & "<exceptionCode>InvalidEntity</exceptionCode>"
        & "<exceptionMessage>Entity 'AcceptedEventRelation' is not supported"
                         & " by the Bulk API.</exceptionMessage>"
     & "</error>"
     & "<numberRecordsFailed>0</numberRecordsFailed>"
     & "<totalProcessingTime>0</totalProcessingTime>"
     & "<apiActiveProcessingTime>0</apiActiveProcessingTime>"
     & "<apexProcessingTime>0</apexProcessingTime></jobInfo>";

begin
   Text_IO.Put_Line (XML.Image (XML.Load_Response (Mess)));
   Text_IO.Put (XML.Image (XML.Load_Response (XML_RPC, Envelope => False)));
end SOAP1;
