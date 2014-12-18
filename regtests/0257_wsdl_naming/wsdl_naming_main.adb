------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                        Copyright (C) 2014, AdaCore                       --
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

with AWS.Client;
with AWS.Config.Set;
pragma Elaborate_All (AWS.Config.Set);
with AWS.Server;
pragma Elaborate_All (AWS.Server);
with AWS.Status;
pragma Elaborate_All (AWS.Status);
with AWS.Response;
pragma Elaborate_All (AWS.Response);
with SOAP.Dispatchers.Callback;
pragma Elaborate_All (SOAP.Dispatchers.Callback);

with API_Service.CB;
pragma Elaborate_All (API_Service.CB);
with API_Service.Server;
pragma Elaborate_All (API_Service.Server);

with Aroot.Data_Type_Pkg;
with NSans.WSDL_Naming_Pkg.Data_Type_Pkg;

procedure WSDL_Naming_Main is

   use Ada;
   use AWS;

   URL  : constant String := API_Service.URL;
   Disp : API_Service.CB.Handler;
   Tmp1 : Aroot.Data_Type_Pkg.Data_Type;
   Tmp2 : NSans.WSDL_Naming_Pkg.Data_Type_Pkg.Data_Type;

begin
   Text_IO.Put_Line ("Server built!");
end WSDL_Naming_Main;
