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

with Ada.Text_IO;

with SOAP.Message.Response.Error;
with SOAP.Parameters;
with SOAP.Types;

package body SOAP_Server_Disp_CB is

   use Ada;

   --------
   -- CB --
   --------

   function CB (Request : AWS.Status.Data) return AWS.Response.Data is
      pragma Unreferenced (Request);
   begin
      return AWS.Response.Build
        ("text/html",
         "<p>This is not a SOAP action !");
   end CB;

   -------------
   -- SOAP_CB --
   -------------

   function SOAP_CB
     (SOAPAction : String;
      Payload    : SOAP.Message.Payload.Object;
      Request    : AWS.Status.Data)
      return AWS.Response.Data
   is
      pragma Unreferenced (Request);

      use SOAP.Types;
      use SOAP.Parameters;

      P  : constant SOAP.Parameters.List
        := SOAP.Message.Parameters (Payload);

      R  : SOAP.Message.Response.Object;
      RP : SOAP.Parameters.List;

   begin
      --  Print SOAP request for the demo

      Text_IO.Put_Line ("SOAP Request, SOAPAction " & SOAPAction);

      Text_IO.Put_Line
        ("   Procedure : " & SOAP.Message.Payload.Procedure_Name (Payload));

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

      R := SOAP.Message.Response.From (Payload);

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

end SOAP_Server_Disp_CB;
