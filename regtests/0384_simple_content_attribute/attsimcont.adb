------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                       Copyright (C) 2025, AdaCore                        --
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

pragma Ada_2022;

with Ada.Strings.Unbounded;
with Ada.Text_IO;

with AWS.Config.Set;
with AWS.MIME;
with AWS.Response;
with AWS.Server;
with AWS.Status;
with SOAP.Types;
with SOAP.Utils;
with SOAP.WSDL.Schema;

with AttSimContService.Client;
with AttSimContService.Server;
with AttSimContService.Types;
with simple.doc.net.sd.Exampletype_Type_Pkg;
with simple.doc.net.sd.Rec_Type_Pkg;
with simple.doc.net.sd.data_Type_Pkg;

procedure AttSimCont is

   use Ada;
   use Ada.Strings.Unbounded;

   use AWS;
   use simple.doc.net.sd.Exampletype_Type_Pkg;
   use simple.doc.net.sd.Rec_Type_Pkg;
   use simple.doc.net.sd.data_Type_Pkg;

   procedure Call (a : Float; rc : n1_Rec_Type);

   -------------
   -- SOAP_CB --
   -------------

   function SOAP_CB is new AttSimContService.Server.call_CB (Call);

   function SOAP_Wrapper is new SOAP.Utils.SOAP_Wrapper (SOAP_CB);

   --------
   -- CB --
   --------

   function CB (Request : Status.Data) return Response.Data is
      SOAPAction : constant String := Status.SOAPAction (Request);
   begin
      if SOAPAction = "http://me.here.net/call" then
         return SOAP_Wrapper (Request, AttSimContService.Schema);
      else
         return Response.Build (MIME.Text_HTML, "<p>Not a valid SOAP request");
      end if;
   end CB;

   ----------
   -- Call --
   ----------

   procedure Call (a : Float; rc : n1_Rec_Type) is
   begin
      Text_IO.Put_Line ("a : " & a'Image);
      Text_IO.Put_Line ("rc: " & rc'Image);
   end Call;

   H_Server : Server.HTTP;
   CNF      : Config.Object;
   P1       : constant n1_ExampleType_Type := (1, To_Unbounded_String ("cm"));
   P2       : constant n1_Rec_Type := (6, P1);

begin
   Config.Set.Server_Name     (CNF, "WSDL demo");
   Config.Set.Server_Port     (CNF, AttSimContService.Server.Port);
   Config.Set.Protocol_Family (CNF, "FAMILY_INET");
   Config.Set.Reuse_Address   (CNF, True);

   Server.Start (H_Server, CB'Unrestricted_Access, CNF);

   Text_IO.Put_Line ("P1:");
   Text_IO.Put_Line
     (SOAP.Types.XML_Image (To_SOAP_Object (P1, "ex"),
                            Encoding => SOAP.WSDL.Schema.Literal,
                            Schema   => AttSimContService.Schema));
   Text_IO.Put_Line ("P2:");
   Text_IO.Put_Line
     (SOAP.Types.XML_Image (To_SOAP_Object (P2, "data"),
                            Encoding => SOAP.WSDL.Schema.Literal,
                            Schema   => AttSimContService.Schema));

   AttSimContService.Client.Call (1.2, P2);

   Server.Shutdown (H_Server);
end AttsimCont;
