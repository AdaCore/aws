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

with Ada.Text_IO;
with Ada.Strings.Unbounded;

with AWS.MIME;
with SOAP.Message.Response.Error;

with Data_Service.Server;
with Data_Service.Types;
with Nested_Types;

with myCompany.myDataService.myDataService_Type_Pkg;
with myCompany.myDataService.myIncludedData_Type_Pkg;

package body Nested_Types_Server is

   use Ada;
   use Ada.Strings.Unbounded;
   use SOAP;

   function Store
     (P : Data_Service.Types.myDataService_Type)
     return Data_Service.Types.Store_Result;

   -----------
   -- Store --
   -----------

   function Store
     (P : Data_Service.Types.myDataService_Type)
     return Data_Service.Types.Store_Result
   is
      use myCompany.myDataService;

      R : Data_Service.Types.myDataService_Type (C => myDataService_Type_Pkg.C2);
      I : Data_Service.Types.Myincludeddata_Type (C => myIncludedData_Type_Pkg.C1);
   begin
      Nested_Types.Dump (P);

      I.intdInput := 4;
      R.myIncludedData := I;
--      R.myOutput := 5;

      return R;
   end Store;

   function Store_CB is new Data_Service.Server.Store_CB (Store);

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
      if SOAPAction = "store" then
         return Store_CB (SOAPAction, Payload, Request);

      else
         return Message.Response.Build
           (Message.Response.Error.Build
              (Message.Response.Error.Client,
               "Wrong SOAP action " & SOAPAction));
      end if;
   end SOAP_CB;

end Nested_Types_Server;
