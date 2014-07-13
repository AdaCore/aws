------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2003-2014, AdaCore                     --
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
with SOAP.Message.Payload;
with SOAP.Message.Response.Error;
with SOAP.Parameters;
with SOAP.Types;

package body SOAP_Server_CB is

   use Ada;

   function SOAP_CB (Request : AWS.Status.Data) return AWS.Response.Data;

   --------
   -- CB --
   --------

   function CB (Request : AWS.Status.Data) return AWS.Response.Data is
      SOAPAction : constant String := AWS.Status.SOAPAction (Request);
   begin
      if SOAPAction = "/soapdemo" then
         return SOAP_CB (Request);

      else
         return AWS.Response.Build
           ("text/html",
            "<p>This is not a SOAP action !");
      end if;
   end CB;

   -------------
   -- SOAP_CB --
   -------------

   function SOAP_CB (Request : AWS.Status.Data) return AWS.Response.Data is
      use SOAP.Types;
      use SOAP.Parameters;

      Rq : aliased constant String := AWS.Status.Payload (Request);
      PL : constant SOAP.Message.Payload.Object
        := SOAP.Message.XML.Load_Payload (Rq);

      P  : constant SOAP.Parameters.List
        := SOAP.Message.Parameters (PL);

      R  : SOAP.Message.Response.Object;
      RP : SOAP.Parameters.List;

   begin
      --  Print SOAP request for the demo

      Text_IO.Put_Line ("SOAP Request");

      Text_IO.Put_Line
        ("   Procedure : " & SOAP.Message.Payload.Procedure_Name (PL));

      Text_IO.Put_Line ("  "
                        & Natural'Image (SOAP.Parameters.Argument_Count (P))
                        & " parameter(s)");

      --  Let's have a look at the parameters named, p1, p2, p3

      Text_IO.Put_Line
        ("   p1 = " & SOAP.Types.Image (SOAP.Parameters.Argument (P, "p1")));
      Text_IO.Put_Line
        ("   P2 = " & SOAP.Types.Image (SOAP.Parameters.Argument (P, "p2")));
      Text_IO.Put_Line
        ("   p3 = " & SOAP.Types.Image (SOAP.Parameters.Argument (P, "p3")));

      Text_IO.New_Line;

      --  Real job start here

      R := SOAP.Message.Response.From (PL);

      declare
         P1 : constant Integer := SOAP.Parameters.Get (P, "p1");
         P2 : constant Integer := SOAP.Parameters.Get (P, "p2");
      begin
         RP := +I (P1 + P2, "myres");
      end;

      SOAP.Message.Set_Parameters (R, RP);

      return SOAP.Message.Response.Build (R);

   exception
      when SOAP.Types.Data_Error =>
         --  Here we have a problem with some parameters, return a SOAP error
         Text_IO.New_Line;

         return SOAP.Message.Response.Build
           (SOAP.Message.Response.Error.Build
            (SOAP.Message.Response.Error.Client, "Parameter error"));
   end SOAP_CB;

end SOAP_Server_CB;
