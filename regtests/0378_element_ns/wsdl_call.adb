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

--  SOAP/WSDL test

pragma Ada_2022;

with Ada.Strings.Unbounded;
with Ada.Text_IO;

with AWS.Config.Set;
with AWS.MIME;
with AWS.Net;
with AWS.Response;
with AWS.Server.Status;
with AWS.Status;

with SOAP.Types;
with SOAP.Utils;

with R_Call_Demo.Client;
with R_Call_Demo.Server;
with R_Call_Demo.Types;

procedure WSDL_Call is

   use Ada;
   use Ada.Strings.Unbounded;
   use AWS;

   use R_Call_Demo.Types;

   H_Server : Server.HTTP;
   CNF      : Config.Object;

   procedure WSDL_Demo_Client is
      use Ada;
      R : R_Call_Demo.Types.B_Rec_Type;
   begin
      -- R := (Code => 67,
      --       C1   => 1,
      --       C2   => 2,
      --       C3   => 3,
      --       D1   => (Code => 7, C => 67),
      --       E1   => (Code => 8, C => 68));
      R_Call_Demo.Client.Call (1, 2, 3, 4, (5, 6, [1], [2]), (7, 8));
--      R_Call_Demo.Client.Call (R);
   end WSDL_Demo_Client;

   procedure Call
     (S : R_Call_Demo.Types.B_Rec_Type);
   procedure Call
     (Code : Integer;
      C1   : R_Call_Demo.Types.B_Code_Type;
      C2   : R_Call_Demo.Types.C_Code_Type;
      C3   : Integer;
      D1   : R_Call_Demo.Types.D_Data_Type;
      E1   : R_Call_Demo.Types.E_Data_Type);

   -------------
   -- SOAP_CB --
   -------------

   function SOAP_CB is new R_Call_Demo.Server.Call_CB (Call);

   function SOAP_Wrapper is new SOAP.Utils.SOAP_Wrapper (SOAP_CB);

   --------
   -- CB --
   --------

   function CB (Request : Status.Data) return Response.Data is
      SOAPAction : constant String := Status.SOAPAction (Request);
   begin
      if SOAPAction = "Call" then
         return SOAP_Wrapper (Request, R_Call_Demo.Schema);
      else
         return Response.Build (MIME.Text_HTML, "<p>Not a SOAP request");
      end if;
   end CB;

   ----------
   -- Call --
   ----------

   procedure Call
     (S : R_Call_Demo.Types.B_Rec_Type) is
   begin
      Text_IO.Put_Line ("S: " & S'Image);
   end Call;

   procedure Call
     (Code : Integer;
      C1   : R_Call_Demo.Types.B_Code_Type;
      C2   : R_Call_Demo.Types.C_Code_Type;
      C3   : Integer;
      D1   : R_Call_Demo.Types.D_Data_Type;
      E1   : R_Call_Demo.Types.E_Data_Type) is
   begin
      Text_IO.Put_Line ("Code: " & Code'Image);
      Text_IO.Put_Line ("C1  : " & C1'Image);
      Text_IO.Put_Line ("C2  : " & C2'Image);
      Text_IO.Put_Line ("C3  : " & C3'Image);
      Text_IO.Put_Line ("D1  : " & D1'Image);
      Text_IO.Put_Line ("E1  : " & E1'Image);
   end Call;

begin
   Config.Set.Server_Name (CNF, "WSDL Call demo");
   Config.Set.Server_Host (CNF, "localhost");
   Config.Set.Server_Port (CNF, R_Call_Demo.Server.Port);
   Config.Set.Reuse_Address (CNF, True);
   Config.Set.HTTP2_Activated (CNF, True);

   Server.Start (H_Server, CB'Unrestricted_Access, CNF);

   if Net.IPv6_Available and then AWS.Server.Status.Is_IPv6 (H_Server) then
      --  Need to start second server on same port but on the different
      --  Protocol_Family because we do not know which family would client try
      --  to connect.

      Server.Add_Listening
        (H_Server, "localhost", R_Call_Demo.Server.Port, Net.FAMILY_INET);
   end if;

   WSDL_Demo_Client;

   Server.Shutdown (H_Server);
end WSDL_Call;
