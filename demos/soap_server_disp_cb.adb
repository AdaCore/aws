------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                            Copyright (C) 2003                            --
--                                ACT-Europe                                --
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

with Ada.Text_IO;

with SOAP.Message.Response.Error;
with SOAP.Parameters;
with SOAP.Types;

package body SOAP_Server_Disp_CB is

   use Ada;

   --------
   -- CB --
   --------

   function CB (Request : in AWS.Status.Data) return AWS.Response.Data is
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
     (SOAPAction : in String;
      Payload    : in SOAP.Message.Payload.Object;
      Request    : in AWS.Status.Data)
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
         --  Here we have a problem with some parameters, return a SOAP error.
         Text_IO.New_Line;

         return SOAP.Message.Response.Build
           (SOAP.Message.Response.Error.Build
            (SOAP.Message.Response.Error.Client, "Parameter error"));
   end SOAP_CB;

end SOAP_Server_Disp_CB;
