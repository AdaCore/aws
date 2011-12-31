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

--  SOAP/WSDL test

with Ada.Calendar;
with Ada.Text_IO;

with AWS.MIME;
with AWS.Response;
with AWS.Server;
with AWS.Status;
with SOAP.Utils;

with StockQuoteService.Client;
with StockQuoteService.Server;
with StockQuoteService.Types;

procedure Test_WSDL2 is

   use Ada;
   use AWS;
   use StockQuoteService;
   use type StockQuoteService.Types.ArrayOfFloat_Type;

   H_Server : AWS.Server.HTTP;

   package FIO is new Text_IO.Float_IO (Float);

   function GetLastTradePrice
     (tickerSymbol : String;
      timePeriod   : Types.TimePeriod_Type)
      return Types.GetLastTradePrice_Result;

   -------------
   -- SOAP_CB --
   -------------

   function SOAP_CB is
      new StockQuoteService.Server.GetLastTradePrice_CB (GetLastTradePrice);

   function SOAP_Wrapper is new SOAP.Utils.SOAP_Wrapper (SOAP_CB);

   --------
   -- CB --
   --------

   function CB (Request : Status.Data) return Response.Data is
      SOAPAction : constant String := Status.SOAPAction (Request);
   begin
      if SOAPAction = "http://localhost/GetTradePrices" then
         return SOAP_Wrapper (Request);
      else
         return Response.Build (MIME.Text_HTML, "<p>Not a SOAP request");
      end if;
   end CB;

   -----------------------
   -- GetLastTradePrice --
   -----------------------

   function GetLastTradePrice
     (tickerSymbol : String;
      timePeriod   : Types.TimePeriod_Type)
      return Types.GetLastTradePrice_Result
   is
      use type Calendar.Time;

      D : constant Duration := timePeriod.endTime - timePeriod.startTime;
   begin
      Text_IO.Put_Line ("Symbol " & tickerSymbol);
      return (+Types.ArrayOfFloat_Type'(1.2, 2.3, 3.4), Float (D));
   end GetLastTradePrice;

   ----------------------
   -- WSDL_Demo_Client --
   ----------------------

   procedure WSDL_Demo_Client is
      use Ada;
      Result : Types.GetLastTradePrice_Result;
      TP     : Types.TimePeriod_Type;
   begin
      TP := (Calendar.Time_Of (2003, 2, 18),
             Calendar.Time_Of (2003, 3, 6));

      Result := StockQuoteService.Client.GetLastTradePrice ("ADA95", TP);

      Text_IO.Put ("Frequency : ");
      FIO.Put (Result.Frequency, Aft => 2, Exp => 0);
      Text_IO.New_Line;

      for K in Result.Result.Item'Range loop
         Text_IO.Put (Positive'Image (K) & " = ");
         FIO.Put (Result.Result.Item (K), Aft => 2, Exp => 0);
         Text_IO.New_Line;
      end loop;
   end WSDL_Demo_Client;

begin
   AWS.Server.Start
     (H_Server, "WSDL Stock Quote Server",
      CB'Unrestricted_Access,
      Port => StockQuoteService.Server.Port);

   WSDL_Demo_Client;

   AWS.Server.Shutdown (H_Server);
end Test_WSDL2;
