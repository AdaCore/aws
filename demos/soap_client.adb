------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                            Copyright (C) 2001                            --
--                      Dmitriy Anisimov - Pascal Obry                      --
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

--  A simple SOAP client.

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

   -----------
   -- Error --
   -----------

   procedure Error (E : in Message.Response.Error.Object) is
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
           SOAP.Client.Call ("http://localhost:8080/soapdemo", P);

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
           SOAP.Client.Call ("http://localhost:8080/soapdemo", P);
      begin
         if Message.Response.Is_Error (R) then
            Error (Message.Response.Error.Object (R));

         else
            Text_IO.Put_Line ("ERROR: This should be a SOAP Error Message");
         end if;
      end;
   end Bad;

begin
   Text_IO.Put_Line ("==> Call GOOD");
   Good;

   Text_IO.Put_Line ("==> Call BAD");
   Bad;
end SOAP_Client;
