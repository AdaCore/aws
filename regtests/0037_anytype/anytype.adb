------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2004-2008, AdaCore                     --
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

--  SOAP/WSDL test

with Ada.Calendar;
with Ada.Command_Line;
with Ada.Text_IO;

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

   function Call
     (Param1 : in Types.Set_Of_int_Type;
      Param2 : in Types.Set_Of_x_Type)
      return Types.Call_Result;

   function Trim (Str : in String) return String;
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
      Param2 : Types.Set_Of_x_Type
        := (Any (I (45)), Any (I (12)), Any (D (8.209)));
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

   function CB (Request : in Status.Data) return Response.Data is
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
     (Param1 : in Types.Set_Of_int_Type;
      Param2 : in Types.Set_Of_x_Type)
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

   function Trim (Str : in String) return String is
      K : Natural := Str'Last;
   begin
      while Str (K) = '0' loop
         K := K - 1;
      end loop;
      return Str (Str'First .. K);
   end Trim;

begin
   AWS.Server.Start
     (H_Server, "AnyType Server",
      CB'Unrestricted_Access,
      Port => AnyType_Service.Server.Port);

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
