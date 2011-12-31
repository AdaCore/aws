------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2012, AdaCore                     --
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

--  A simple SOAP client

with Ada.Text_IO;

with SOAP.Types;
with SOAP.Parameters;
with SOAP.Client;
with SOAP.Message.Payload;
with SOAP.Message.XML;
with SOAP.Message.Response.Error;

procedure SOAP_Client is

   use Ada;
   use SOAP;
   use SOAP.Types;
   use type SOAP.Parameters.List;

   procedure Error (E : Message.Response.Error.Object);
   --  Display SOAP Error

   ---------
   -- Bad --
   ---------

   procedure Bad is
      P_Set : Parameters.List := +I (10, "p");

      P     : Message.Payload.Object;

   begin
      P := Message.Payload.Build ("This_Proc", P_Set);

      declare
         R : constant Message.Response.Object'Class :=
           SOAP.Client.Call ("http://localhost:8080/soapdemo", P, "/soapdemo");
      begin
         if Message.Response.Is_Error (R) then
            Error (Message.Response.Error.Object (R));

         else
            Text_IO.Put_Line ("ERROR: This should be a SOAP Error Message");
         end if;
      end;
   end Bad;

   -----------
   -- Error --
   -----------

   procedure Error (E : Message.Response.Error.Object) is
      P : constant Parameters.List := SOAP.Message.Parameters (E);
   begin
      Text_IO.Put_Line ("Faultcode   : "
                        & SOAP.Parameters.Get (P, "faultcode"));

      Text_IO.Put_Line ("Faultstring : "
                        & SOAP.Parameters.Get (P, "faultstring"));
   end Error;

   ----------
   -- Good --
   ----------

   procedure Good is

      P_Set : Parameters.List := +I (10, "p1") & I (32, "p2") & F (12.4, "p3");

      P     : Message.Payload.Object;

   begin
      P := Message.Payload.Build ("This_Proc", P_Set);

      declare
         R : constant Message.Response.Object'Class :=
           SOAP.Client.Call ("http://localhost:8080/soapdemo", P, "/soapdemo");

         P : constant Parameters.List
           := SOAP.Message.Parameters (R);

         Myres : constant Integer := SOAP.Parameters.Get (P, "myres");
      begin
         Text_IO.Put_Line ("---------- XML Response ----------");
         Text_IO.Put_Line (Message.XML.Image (R));
         Text_IO.New_Line;
         Text_IO.Put_Line ("Myres = " & Integer'Image (Myres));
         Text_IO.New_Line;
      end;
   end Good;

begin
   Text_IO.Put_Line ("==> Call GOOD");
   Good;

   Text_IO.Put_Line ("==> Call BAD");
   Bad;
end SOAP_Client;
