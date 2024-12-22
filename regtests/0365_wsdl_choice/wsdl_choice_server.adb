------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                       Copyright (C) 2024, AdaCore                        --
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
with Ada.Strings.Unbounded;

with AWS.MIME;
with SOAP.Message.Response.Error;

with WSDL_Choice.Server;
with WSDL_Choice.Types;

with urn.naws.WSDL_Choice.R1_Type_Pkg;
with urn.naws.WSDL_Choice.R2_Type_Pkg;

package body WSDL_Choice_Server is

   use Ada;
   use Ada.Strings.Unbounded;
   use SOAP;

   procedure Try
     (Param1 : WSDL_Choice.Types.R1_Type;
      Param2 : WSDL_Choice.Types.R2_Type);

   procedure Try
     (Param1 : WSDL_Choice.Types.R1_Type;
      Param2 : WSDL_Choice.Types.R2_Type)
   is
      use urn.naws.WSDL_Choice;
   begin
      --  Dump Param1
      Text_IO.Put_Line ("P1 " & Param1.C'Img);
      Text_IO.Put_Line ("   - state " & Param1.State'Img);
      case Param1.C is
         when R1_Type_Pkg.C0 =>
            null;
         when R1_Type_Pkg.C1 =>
            Text_IO.Put_Line
              ("   * percentComplete " & Param1.percentComplete'Img);
         when R1_Type_Pkg.C2 =>
            Text_IO.Put_Line
              ("   * statusMessage   " & To_String (Param1.statusMessage));
      end case;

      --  Dump Param2
      Text_IO.Put_Line ("P2 " & Param2.C'Img);
      Text_IO.Put_Line ("   - one   " & Param2.One'Img);
      Text_IO.Put_Line ("   - two   " & Param2.Two'Img);
      Text_IO.Put_Line ("   - state " & Param2.State'Img);
      case Param2.C is
         when R2_Type_Pkg.C0 =>
            null;
         when R2_Type_Pkg.C1 =>
            Text_IO.Put_Line
              ("   * percentComplete " & Param2.percentComplete'Img);
         when R2_Type_Pkg.C2 =>
            Text_IO.Put_Line
              ("   * statusMessage   " & To_String (Param2.statusMessage));
      end case;
   end Try;

   function Try_CB is new WSDL_Choice.Server.Try_CB (Try);

   -------------
   -- HTTP_CB --
   -------------

   function HTTP_CB (Request : Status.Data) return Response.Data is
   begin
      return Response.Build
        (MIME.Text_HTML, "No HTTP request should be called.");
   end HTTP_CB;

   -------------
   -- SOAP_CB --
   -------------

   function SOAP_CB
     (SOAPAction : String;
      Payload    : Message.Payload.Object;
      Request    : Status.Data)
      return Response.Data is
   begin
      if SOAPAction = "Try" then
         return Try_CB (SOAPAction, Payload, Request);

      else
         return Message.Response.Build
           (Message.Response.Error.Build
              (Message.Response.Error.Client,
               "Wrong SOAP action " & SOAPAction));
      end if;
   end SOAP_CB;

end WSDL_Choice_Server;
