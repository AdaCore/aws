------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                       Copyright (C) 2019, AdaCore                        --
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

with Ada.Strings.Unbounded;
with Ada.Text_IO;
with SOAP.Message.XML;
with SOAP.Message.Response;
with SOAP.WSDL.Schema;

procedure Nil_Values is

   procedure Load (S : String);
   --  Load a output the payload

   procedure Load (S : String) is
      use Ada.Strings.Unbounded;
      use Ada.Text_IO;

      use SOAP.Message.XML;

      O : constant SOAP.Message.Response.Object'Class :=
        Load_Response
          (To_Unbounded_String
             ("<X xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance"""
              & " xmlns:xsd=""http://www.w3.org/2001/XMLSchema"">"
              & "<Value xsi:type="""
              & S & """ xsi:nil=""true""/></X>"),
           False, SOAP.WSDL.Schema.Empty);
   begin
      Put_Line (Image (O));
   end Load;

begin
   Load ("xsd:string");
   Load ("xsd:boolean");
   Load ("xsd:int");
   Load ("xsd:dateTime");
   Load ("xsd:timeInstant");
end Nil_Values;
