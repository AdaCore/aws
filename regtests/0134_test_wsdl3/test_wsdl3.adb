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

--  SOAP/WSDL test (same as test_wsdl2 but uses -a option)

with Ada.Calendar;
with Ada.Text_IO;

with AWS.MIME;
with AWS.Response;
with AWS.Server;
with AWS.Status;
with SOAP.Utils;

with Stock_Quote_Service.Client;
with Stock_Quote_Service.Server;
with Stock_Quote_Service.Types;

procedure Test_WSDL3 is

   use Ada;
   use AWS;
   use Stock_Quote_Service;
   use type Stock_Quote_Service.Types.Array_Of_Float_Type;

   H_Server : AWS.Server.HTTP;

   package FIO is new Text_IO.Float_IO (Float);

   function Get_Last_Trade_Price
     (Ticker_Symbol : in String;
      Time_Period   : in Types.Time_Period_Type)
      return Types.Get_Last_Trade_Price_Result;

   -------------
   -- SOAP_CB --
   -------------

   function SOAP_CB is new
     Stock_Quote_Service.Server.Get_Last_Trade_Price_CB (Get_Last_Trade_Price);

   function SOAP_Wrapper is new SOAP.Utils.SOAP_Wrapper (SOAP_CB);

   --------
   -- CB --
   --------

   function CB (Request : in Status.Data) return Response.Data is
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

   function Get_Last_Trade_Price
     (Ticker_Symbol : in String;
      Time_Period   : in Types.Time_Period_Type)
      return Types.Get_Last_Trade_Price_Result
   is
      use type Calendar.Time;

      D : constant Duration := Time_Period.End_Time - Time_Period.Start_Time;
   begin
      Text_IO.Put_Line ("Symbol " & Ticker_Symbol);
      return (+Types.Array_Of_Float_Type'(1.2, 2.3, 3.4), Float (D));
   end Get_Last_Trade_Price;

   ----------------------
   -- WSDL_Demo_Client --
   ----------------------

   procedure WSDL_Demo_Client is
      use Ada;
      Result : Types.Get_Last_Trade_Price_Result;
      TP     : Types.Time_Period_Type;
   begin
      TP := (Calendar.Time_Of (2003, 2, 18),
             Calendar.Time_Of (2003, 3, 6));

      Result := Stock_Quote_Service.Client.Get_Last_Trade_Price ("ADA95", TP);

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
      Port => Stock_Quote_Service.Server.Port);

   WSDL_Demo_Client;

   AWS.Server.Shutdown (H_Server);
end Test_WSDL3;
