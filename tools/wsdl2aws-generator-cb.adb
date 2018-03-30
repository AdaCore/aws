------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2003-2018, AdaCore                     --
--                                                                          --
--  This library is free software;  you can redistribute it and/or modify   --
--  it under terms of the  GNU General Public License  as published by the  --
--  Free Software  Foundation;  either version 3,  or (at your  option) any --
--  later version. This library is distributed in the hope that it will be  --
--  useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                    --
--                                                                          --
--  As a special exception under Section 7 of GPL version 3, you are        --
--  granted additional permissions described in the GCC Runtime Library     --
--  Exception, version 3.1, as published by the Free Software Foundation.   --
--                                                                          --
--  You should have received a copy of the GNU General Public License and   --
--  a copy of the GCC Runtime Library Exception along with this program;    --
--  see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see   --
--  <http://www.gnu.org/licenses/>.                                         --
--                                                                          --
--  As a special exception, if other files instantiate generics from this   --
--  unit, or you link this unit with other files to produce an executable,  --
--  this  unit  does not  by itself cause  the resulting executable to be   --
--  covered by the GNU General Public License. This exception does not      --
--  however invalidate any other reasons why the executable file  might be  --
--  covered by the  GNU Public License.                                     --
------------------------------------------------------------------------------

pragma Ada_2012;

separate (WSDL2AWS.Generator)
package body CB is

   Tmp_Adb  : Text_IO.File_Type; -- Temporary file with callback definitions
   Proc_Adb : Text_IO.File_Type; -- Temporary file with SOAPAction definitions

   -----------------
   -- End_Service --
   -----------------

   procedure End_Service
     (O    : in out Object;
      Name : String)
   is
      U_Name : constant String := To_Unit_Name (Format_Name (O, Name));
      Buffer : String (1 .. 1_024);
      Last   : Natural;
   begin
      --  Spec

      Text_IO.New_Line (CB_Ads);
      Text_IO.Put_Line (CB_Ads, "end " & U_Name & ".CB;");

      --  Copy Is_SOAPAction_Defined now

      Text_IO.Reset (Proc_Adb, Text_IO.In_File);

      while not Text_IO.End_Of_File (Proc_Adb) loop
         Text_IO.Get_Line (Proc_Adb, Buffer, Last);
         Text_IO.Put_Line (CB_Adb, Buffer (1 .. Last));
      end loop;

      Text_IO.Put_Line (CB_Adb, "      else");
      Text_IO.Put_Line (CB_Adb, "         return False;");
      Text_IO.Put_Line (CB_Adb, "      end if;");
      Text_IO.Put_Line (CB_Adb, "   end Is_SOAPAction_Defined;");

      --  Copy SOAP_CB definition now

      Text_IO.Reset (Tmp_Adb, Text_IO.In_File);

      while not Text_IO.End_Of_File (Tmp_Adb) loop
         Text_IO.Get_Line (Tmp_Adb, Buffer, Last);
         Text_IO.Put_Line (CB_Adb, Buffer (1 .. Last));
      end loop;

      --  End SOAP_CB

      Text_IO.New_Line (CB_Adb);
      Text_IO.Put_Line (CB_Adb, "      else");
      Text_IO.Put_Line (CB_Adb, "         return Message.Response.Build");
      Text_IO.Put_Line (CB_Adb, "           (Message.Response.Error.Build");
      Text_IO.Put_Line
        (CB_Adb, "             (Message.Response.Error.Client,");
      Text_IO.Put_Line
        (CB_Adb, "              ""Wrong SOAP action "" & SOAPAction));");
      Text_IO.Put_Line (CB_Adb, "      end if;");

      --  Exception handler

      Text_IO.Put_Line (CB_Adb, "   exception");
      Text_IO.Put_Line (CB_Adb, "      when E : others =>");
      Text_IO.Put_Line (CB_Adb, "         return Message.Response.Build");
      Text_IO.Put_Line (CB_Adb, "           (Message.Response.Error.Build");
      Text_IO.Put_Line
        (CB_Adb, "             (Message.Response.Error.Client,");
      Text_IO.Put_Line
        (CB_Adb,
         "              ""Error in SOAP_CB for SOAPAction "" & SOAPAction");
      Text_IO.Put_Line
        (CB_Adb,
         "                & "" ("" & Exception_Information (E) & "")""));");
      Text_IO.Put_Line (CB_Adb, "   end SOAP_CB;");

      --  Body

      Text_IO.New_Line (CB_Adb);
      Text_IO.Put_Line (CB_Adb, "end " & U_Name & ".CB;");

      Text_IO.Close (Tmp_Adb);
      Text_IO.Close (Proc_Adb);
   end End_Service;

   -------------------
   -- New_Procedure --
   -------------------

   procedure New_Procedure
     (O             : in out Object;
      Proc          : String;
      Documentation : String;
      SOAPAction    : String;
      Wrapper_Name  : String;
      Namespace     : SOAP.Name_Space.Object;
      Input         : WSDL.Parameters.P_Set;
      Output        : WSDL.Parameters.P_Set;
      Fault         : WSDL.Parameters.P_Set)
   is
      pragma Unreferenced (Wrapper_Name, Namespace, Documentation);
      pragma Unreferenced (Input, Output, Fault);

      L_Proc : constant String := Format_Name (O, Proc);
   begin
      Text_IO.New_Line (CB_Adb);

      Text_IO.Put_Line (CB_Adb, "   function " & Proc & "_CB is");
      Text_IO.Put_Line (CB_Adb, "     new " & To_String (O.Unit)
                        & ".Server." & L_Proc & "_CB ("
                        & Procs_Spec (O) & "." & Proc & ");");

      --  Write SOAP_CB body

      if O.First_Proc then
         Text_IO.Put (Tmp_Adb, "      if ");
         Text_IO.Put (Proc_Adb, "      if ");
         O.First_Proc := False;
      else
         Text_IO.Put (Tmp_Adb, "      elsif ");
         Text_IO.Put (Proc_Adb, "      elsif ");
      end if;

      Text_IO.Put_Line
        (Tmp_Adb, "SOAPAction = """
         & To_String (O.Prefix) & SOAPAction & """ then");
      Text_IO.Put_Line (Tmp_Adb, "         return " & Proc
                          & "_CB (SOAPAction, Payload, Request);");
      Text_IO.New_Line (Tmp_Adb);

      Text_IO.Put_Line
        (Proc_Adb,
         "SOAPAction = """ & To_String (O.Prefix) & Proc & """ then");
      Text_IO.Put_Line (Proc_Adb, "         return True;");
      Text_IO.New_Line (Proc_Adb);
   end New_Procedure;

   -------------------
   -- Start_Service --
   -------------------

   procedure Start_Service
     (O                  : in out Object;
      Name               : String;
      Root_Documentation : String;
      Documentation      : String;
      Location           : String)
   is
      pragma Unreferenced (Location, Root_Documentation, Documentation);

      U_Name : constant String := To_Unit_Name (Format_Name (O, Name));
   begin
      Text_IO.Create (Tmp_Adb, Text_IO.Out_File);
      Text_IO.Create (Proc_Adb, Text_IO.Out_File);

      --  Spec

      Text_IO.Put_Line (CB_Ads, "with AWS.Response;");
      Text_IO.Put_Line (CB_Ads, "with AWS.Status;");
      Text_IO.New_Line (CB_Ads);
      Text_IO.Put_Line (CB_Ads, "with SOAP.Dispatchers.Callback;");
      Text_IO.Put_Line (CB_Ads, "with SOAP.Message.Payload;");
      Text_IO.New_Line (CB_Ads);
      Text_IO.Put_Line (CB_Ads, "package " & U_Name & ".CB is");
      Text_IO.New_Line (CB_Ads);
      Text_IO.Put_Line (CB_Ads, "   use AWS;");
      Text_IO.Put_Line (CB_Ads, "   use SOAP;");
      Text_IO.New_Line (CB_Ads);
      Text_IO.Put_Line (CB_Ads, "   pragma Style_Checks (Off);");
      Text_IO.New_Line (CB_Ads);
      Text_IO.Put_Line
        (CB_Ads,
         "   subtype Handler is SOAP.Dispatchers.Callback.Handler;");

      Text_IO.New_Line (CB_Ads);
      Text_IO.Put_Line (CB_Ads, "   function Is_SOAPAction_Defined");
      Text_IO.Put_Line (CB_Ads, "     (SOAPAction : String) return Boolean;");
      Text_IO.Put_Line
        (CB_Ads, "   --  Returns True if SOAPAction handled by SOAP_CB below");

      Text_IO.New_Line (CB_Ads);
      Text_IO.Put_Line (CB_Ads, "   function SOAP_CB");
      Text_IO.Put_Line (CB_Ads, "     (SOAPAction : String;");
      Text_IO.Put_Line (CB_Ads,
                        "      Payload    : Message.Payload.Object;");
      Text_IO.Put_Line (CB_Ads, "      Request    : AWS.Status.Data)");
      Text_IO.Put_Line (CB_Ads, "      return Response.Data;");

      --  Body

      Text_IO.Put_Line (CB_Adb, "with Ada.Exceptions;");
      Text_IO.New_Line (CB_Adb);
      Text_IO.Put_Line (CB_Adb, "with SOAP.Message.Response.Error;");
      Text_IO.New_Line (CB_Adb);
      Text_IO.Put_Line
        (CB_Adb, "with " & Types_Spec (O, With_Clause => True) & ";");

      if Types_Spec (O) /= Procs_Spec (O) then
         Text_IO.Put_Line
           (CB_Adb, "with " & Procs_Spec (O, With_Clause => True) & ";");
      end if;

      Text_IO.New_Line (CB_Adb);
      Text_IO.Put_Line (CB_Adb, "with " & U_Name & ".Server;");
      Text_IO.Put_Line (CB_Adb, "with " & U_Name & ".Types;");
      Text_IO.New_Line (CB_Adb);
      Text_IO.Put_Line (CB_Adb, "package body " & U_Name & ".CB is");
      Text_IO.New_Line (CB_Adb);
      Text_IO.Put_Line (CB_Adb, "   use Ada.Exceptions;");
      Text_IO.Put_Line (CB_Adb, "   use SOAP;");
      Text_IO.New_Line (CB_Adb);
      Text_IO.Put_Line (CB_Adb,
                        "   pragma Warnings (Off, " & U_Name & ".Server);");
      Text_IO.Put_Line (CB_Adb,
                        "   pragma Warnings (Off, " & U_Name & ".Types);");
      Text_IO.New_Line (CB_Adb);
      Text_IO.Put_Line (CB_Adb, "   pragma Style_Checks (Off);");

      --  Tmp body

      Text_IO.New_Line (Tmp_Adb);
      Text_IO.Put_Line (Tmp_Adb, "   -------------");
      Text_IO.Put_Line (Tmp_Adb, "   -- SOAP_CB --");
      Text_IO.Put_Line (Tmp_Adb, "   -------------");
      Text_IO.New_Line (Tmp_Adb);
      Text_IO.Put_Line (Tmp_Adb, "   function SOAP_CB");
      Text_IO.Put_Line (Tmp_Adb, "     (SOAPAction : String;");
      Text_IO.Put_Line (Tmp_Adb,
                        "      Payload    : Message.Payload.Object;");
      Text_IO.Put_Line (Tmp_Adb, "      Request    : AWS.Status.Data)");
      Text_IO.Put_Line (Tmp_Adb, "      return Response.Data is");
      Text_IO.Put_Line (Tmp_Adb, "   begin");

      Text_IO.New_Line (Proc_Adb);
      Text_IO.Put_Line (Proc_Adb, "   ---------------------------");
      Text_IO.Put_Line (Proc_Adb, "   -- Is_SOAPAction_Defined --");
      Text_IO.Put_Line (Proc_Adb, "   ---------------------------");
      Text_IO.New_Line (Proc_Adb);
      Text_IO.Put_Line (Proc_Adb, "   function Is_SOAPAction_Defined");
      Text_IO.Put_Line
        (Proc_Adb, "     (SOAPAction : String) return Boolean is");
      Text_IO.Put_Line (Proc_Adb, "   begin");
   end Start_Service;

end CB;
