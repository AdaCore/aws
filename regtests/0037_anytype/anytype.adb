------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2004-2020, AdaCore                     --
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

with Ada.Calendar;
with Ada.Command_Line;
with Ada.Text_IO;

with AWS.Config.Set;
with AWS.MIME;
with AWS.Net.Log.Callbacks;
with AWS.Response;
with AWS.Server;
with AWS.Status;
with AWS.Utils;
with SOAP.Types;
with SOAP.Utils;

with AnyType_Service.Client;
with AnyType_Service.Server;
with AnyType_Service.Types;

procedure AnyType is

   use Ada;
   use AWS;
   use SOAP.Types;

   use AnyType_Service;

   H_Server : AWS.Server.HTTP;
   CNF      : AWS.Config.Object;

   function Call
     (Param1 : Types.Set_Of_int_Type;
      Param2 : Types.Set_Of_x_Type) return Types.Call_Result;

   function Trim (Str : String) return String;
   --  Remove trailing 0, use to have output comparable to the Java one

   -------------
   -- SOAP_CB --
   -------------

   function SOAP_CB is new AnyType_Service.Server.Call_CB (Call);

   function SOAP_Wrapper is new SOAP.Utils.SOAP_Wrapper (SOAP_CB);

   ---------------------
   -- AnyType__Client --
   ---------------------

   procedure AnyType_Client is
      use Ada;
      Param1 : Types.Set_Of_int_Type := (12, 9);
      Param2 : Types.Set_Of_X_Type :=
                 (Any (I (45)), Any (I (12)), Any (D (Long_Float'(8.209))));
   begin
      declare
         Result : Types.Set_Of_x_Type := Client.Call (Param1, Param2);
      begin
         for K in Result'Range loop
            Text_IO.Put_Line
              (Utils.Image (K) & " - " & Trim (SOAP.Types.Image (Result (K))));
         end loop;
      end;
   end AnyType_Client;

   --------
   -- CB --
   --------

   function CB (Request : Status.Data) return Response.Data is
      SOAPAction : constant String := Status.SOAPAction (Request);
   begin
      if SOAPAction = "Call" then
         return SOAP_Wrapper (Request);
      else
         return Response.Build (MIME.Text_HTML, "<p>Not a SOAP request");
      end if;
   end CB;

   ----------
   -- Call --
   ----------

   function Call
     (Param1 : Types.Set_Of_int_Type;
      Param2 : Types.Set_Of_x_Type)
      return Types.Call_Result
   is
      R : Types.Call_Result := Param2;
      L : Positive := Param1'First;
   begin
      for K in R'Range loop
         if XML_Type (R (K)) = XML_Int then
            R (K) := Any (I (Get (R (K)) + Param1 (L)));
            L := L + 1;
         end if;
      end loop;

      return R;
   end Call;

   ----------
   -- Trim --
   ----------

   function Trim (Str : String) return String is
      K : Natural := Str'Last;
   begin
      if Str'Length > 4 and then Str (K - 3 .. K) = "E+00" then
         K := K - 4;
      end if;

      while Str (K) = '0' loop
         K := K - 1;
      end loop;
      return Str (Str'First .. K);
   end Trim;

begin
   Config.Set.Server_Name     (CNF, "AnyType Server");
   Config.Set.Protocol_Family (CNF, "FAMILY_INET");
   Config.Set.Server_Port     (CNF, AnyType_Service.Server.Port);

   AWS.Server.Start (H_Server, CB'Unrestricted_Access, CNF);

   AWS.Net.Log.Callbacks.Initialize
     ("anytype.netlog", AWS.Net.Log.Callbacks.Text'Access);

   if Ada.Command_Line.Argument_Count = 1
     and then Ada.Command_Line.Argument (1) = "-j"
   then
      AWS.Server.Wait (AWS.Server.Forever);
      AWS.Net.Log.Callbacks.Finalize;

   else
      AnyType_Client;

      AWS.Net.Log.Callbacks.Finalize;

      AWS.Server.Shutdown (H_Server);
   end if;
end AnyType;
