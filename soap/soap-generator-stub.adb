------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                          Copyright (C) 2003-2004                         --
--                                ACT-Europe                                --
--                                                                          --
--  Authors: Dmitriy Anisimkov - Pascal Obry                                --
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

with SOAP.Client;

separate (SOAP.Generator)
package body Stub is

   -----------------
   -- End_Service --
   -----------------

   procedure End_Service
     (O    : in out Object;
      Name : in     String)
   is
      U_Name : constant String := To_Unit_Name (Format_Name (O, Name));
   begin
      --  Spec

      Text_IO.New_Line (Stub_Ads);
      Text_IO.Put_Line (Stub_Ads, "end " & U_Name & ".Client;");

      --  Body

      Text_IO.New_Line (Stub_Adb);
      Text_IO.Put_Line (Stub_Adb, "end " & U_Name & ".Client;");
   end End_Service;

   -------------------
   -- New_Procedure --
   -------------------

   procedure New_Procedure
     (O          : in out Object;
      Proc       : in     String;
      SOAPAction : in     String;
      Namespace  : in     String;
      Input      : in     WSDL.Parameters.P_Set;
      Output     : in     WSDL.Parameters.P_Set;
      Fault      : in     WSDL.Parameters.P_Set)
   is
      use type SOAP.WSDL.Parameters.P_Set;

      procedure Output_Parameter
        (K      : in Positive;
         Prefix : in String;
         N      : in WSDL.Parameters.P_Set);
      --  Ouptut parameter

      procedure Output_Simple
        (K      : in Positive;
         Prefix : in String;
         N      : in WSDL.Parameters.P_Set);
      --  Output a simple parameter

      procedure Output_Derived
        (K      : in Positive;
         Prefix : in String;
         N      : in WSDL.Parameters.P_Set);
      --  Output a derived type parameter

      procedure Output_Enumeration
        (K      : in Positive;
         Prefix : in String;
         N      : in WSDL.Parameters.P_Set);
      --  Output an enumeration type parameter

      procedure Output_Record
        (K      : in Positive;
         Prefix : in String;
         N      : in WSDL.Parameters.P_Set);
      --  Output a record parameter

      procedure Output_Array
        (K      : in Positive;
         Prefix : in String;
         N      : in WSDL.Parameters.P_Set);
      --  Output an array parameter

      procedure Output_Result (N : in WSDL.Parameters.P_Set);

      ------------------
      -- Output_Array --
      ------------------

      procedure Output_Array
        (K      : in Positive;
         Prefix : in String;
         N      : in WSDL.Parameters.P_Set) is
      begin
         if Prefix = "" then
            --  Not inside a record
            Text_IO.Put
              (Stub_Adb, "A (To_Object_Set ("
                 & Format_Name (O, To_String (N.Name))
                 & "), """ & To_String (N.Name) & """)");

         else
            Text_IO.Put
              (Stub_Adb, "A (To_Object_Set ("
                 & Prefix & Format_Name (O, To_String (N.Name))
                 & ".Item.all" & "), """ & To_String (N.Name) & """)");
         end if;

         Output_Parameter (K + 1, Prefix, N.Next);
      end Output_Array;

      --------------------
      -- Output_Derived --
      --------------------

      procedure Output_Derived
        (K      : in Positive;
         Prefix : in String;
         N      : in WSDL.Parameters.P_Set)
      is
         use type WSDL.Parameter_Type;
      begin
         if Prefix /= "" then
            --  Inside a record
            Text_IO.Put
              (Stub_Adb,
               WSDL.Set_Routine (N.P_Type, Context => WSDL.Component));
         else
            Text_IO.Put (Stub_Adb, WSDL.Set_Routine (N.Parent_Type));
         end if;

         Text_IO.Put
           (Stub_Adb,
            " (" & WSDL.To_Ada (N.Parent_Type) & " ("
              & Prefix & Format_Name (O, To_String (N.Name))
              & "), """ & To_String (N.Name) & """)");

         if Prefix /= "" and then N.Next /= null then
            Text_IO.Put (Stub_Adb, ",");
         end if;

         Output_Parameter (K + 1, Prefix, N.Next);
      end Output_Derived;

      ------------------------
      -- Output_Enumeration --
      ------------------------

      procedure Output_Enumeration
        (K      : in Positive;
         Prefix : in String;
         N      : in WSDL.Parameters.P_Set)
      is
         use type WSDL.Parameter_Type;
      begin
         Text_IO.Put (Stub_Adb, "SOAP.Types.E");

         Text_IO.Put
           (Stub_Adb,
            " (Types.Image ("
              & Prefix & Format_Name (O, To_String (N.Name))
              & "), """
              & To_String (N.E_Name)
              & """, """ & To_String (N.Name) & """)");

         if Prefix /= "" and then N.Next /= null then
            Text_IO.Put (Stub_Adb, ",");
         end if;

         Output_Parameter (K + 1, Prefix, N.Next);
      end Output_Enumeration;

      ----------------------
      -- Output_Parameter --
      ----------------------

      procedure Output_Parameter
        (K      : in Positive;
         Prefix : in String;
         N      : in WSDL.Parameters.P_Set)
      is
         use Ada.Strings.Fixed;
         use type WSDL.Parameters.Kind;
      begin
         if N /= null then
            if K = 1 then
               Text_IO.Put (Stub_Adb, "+");
            else
               Text_IO.New_Line (Stub_Adb);
               Text_IO.Put (Stub_Adb, ((K - 1) * 3) * ' ');

               if Prefix = "" then
                  Text_IO.Put (Stub_Adb, "      & ");
               else
                  Text_IO.Put (Stub_Adb, "      +");
               end if;
            end if;

            case N.Mode is
               when WSDL.Parameters.K_Simple =>
                  Output_Simple (K, Prefix, N);

               when WSDL.Parameters.K_Derived =>
                  Output_Derived (K, Prefix, N);

               when WSDL.Parameters.K_Enumeration =>
                  Output_Enumeration (K, Prefix, N);

               when WSDL.Parameters.K_Array =>
                  Output_Array (K, Prefix, N);

               when WSDL.Parameters.K_Record =>
                  Output_Record (K + 1, Prefix, N);
            end case;
         end if;
      end Output_Parameter;

      -------------------
      -- Output_Record --
      -------------------

      procedure Output_Record
        (K      : in Positive;
         Prefix : in String;
         N      : in WSDL.Parameters.P_Set)
      is
         use Ada.Strings.Fixed;
      begin
         Text_IO.Put (Stub_Adb, "To_SOAP_Object (");

         if Prefix = "" then
            Text_IO.Put
              (Stub_Adb,
               Format_Name (O, To_String (N.Name))
                 & ", """ & To_String (N.Name) & """)");
         else
            Text_IO.Put
              (Stub_Adb, Prefix & "." & Format_Name (O, To_String (N.Name))
                 & ", """ & To_String (N.Name) & """)");
         end if;

         Output_Parameter (K + 1, Prefix, N.Next);
      end Output_Record;

      -------------------
      -- Output_Result --
      -------------------

      procedure Output_Result (N : in WSDL.Parameters.P_Set) is
         use type WSDL.Parameters.Kind;
         use type WSDL.Parameter_Type;
      begin
         if N.Mode = WSDL.Parameters.K_Array then
            declare
               Name : constant String := Format_Name (O, To_String (N.T_Name));
            begin
               Text_IO.Put_Line
                 (Stub_Adb,
                  "+To_" & Name & "_Type (V ");
               Text_IO.Put_Line
                 (Stub_Adb,
                  "                       "
                    & "(SOAP.Types.SOAP_Array'(SOAP.Parameters.Get");
               Text_IO.Put
                 (Stub_Adb,
                  "                         "
                    & "(R_Param, """ & To_String (N.Name) & """))))");
            end;

         else

            if N.Mode = WSDL.Parameters.K_Simple
              and then N.P_Type = WSDL.P_String
            then
               --  First call operator to convert the string to an unbounded
               --  string.
               Text_IO.Put (Stub_Adb, "+");
            end if;

            Text_IO.Put
              (Stub_Adb,
               "SOAP.Parameters.Get (R_Param, """
                 & To_String (N.Name) & """)");
         end if;
      end Output_Result;

      -------------------
      -- Output_Simple --
      -------------------

      procedure Output_Simple
        (K      : in Positive;
         Prefix : in String;
         N      : in WSDL.Parameters.P_Set)
      is
         use type WSDL.Parameter_Type;
      begin
         if Prefix /= "" then
            --  Inside a record
            Text_IO.Put
              (Stub_Adb,
               WSDL.Set_Routine (N.P_Type, Context => WSDL.Component));
         else
            Text_IO.Put (Stub_Adb, WSDL.Set_Routine (N.P_Type));
         end if;

         Text_IO.Put
           (Stub_Adb,
            " (" & Prefix & Format_Name (O, To_String (N.Name))
              & ", """ & To_String (N.Name) & """)");

         if Prefix /= "" and then N.Next /= null then
            Text_IO.Put (Stub_Adb, ",");
         end if;

         Output_Parameter (K + 1, Prefix, N.Next);
      end Output_Simple;

      L_Proc : constant String := Format_Name (O, Proc);

      use type WSDL.Parameters.Kind;
      use type WSDL.Parameter_Type;

   begin
      --  Spec

      Text_IO.New_Line (Stub_Ads);

      Text_IO.Put (Stub_Ads, "   ");
      Put_Header (Stub_Ads, O, Proc, Input, Output);

      Text_IO.Put_Line (Stub_Ads, ";");
      Text_IO.Put_Line
        (Stub_Ads, "   --  Raises SOAP.SOAP_Error if the procedure fails");

      --  Body

      Text_IO.New_Line (Stub_Adb);
      Header_Box (O, Stub_Adb, Format_Name (O, Proc));
      Text_IO.New_Line (Stub_Adb);

      Text_IO.Put (Stub_Adb, "   ");
      Put_Header (Stub_Adb, O, Proc, Input, Output);

      if Input /= null then
         Text_IO.New_Line (Stub_Adb);
         Text_IO.Put (Stub_Adb, "  ");
      end if;

      Text_IO.Put_Line (Stub_Adb, " is");
      Text_IO.Put_Line
        (Stub_Adb, "      P_Set   : SOAP.Parameters.List;");
      Text_IO.Put_Line
        (Stub_Adb, "      Payload : SOAP.Message.Payload.Object;");
      Text_IO.Put_Line (Stub_Adb, "   begin");
      Text_IO.Put_Line (Stub_Adb, "      --  Set parameters");

      --  Set parameters

      if Input /= null then
         Text_IO.Put (Stub_Adb, "      P_Set := ");

         Output_Parameter (1, "", Input);
         Text_IO.Put_Line (Stub_Adb, ";");
      end if;

      Text_IO.Put_Line
        (Stub_Adb, "      Payload := SOAP.Message.Payload.Build");
      Text_IO.Put
        (Stub_Adb, "        (""" & Proc & """, P_Set");

      if Namespace = "" then
         Text_IO.Put_Line (Stub_Adb, ");");
      else
         Text_IO.Put_Line (Stub_Adb, ", """ & Namespace & """);");
      end if;

      Text_IO.New_Line (Stub_Adb);
      Text_IO.Put_Line (Stub_Adb, "      declare");
      Text_IO.Put_Line
        (Stub_Adb,
         "         Response : constant SOAP.Message.Response.Object'Class");
      Text_IO.Put_Line
        (Stub_Adb,
         "           := SOAP.Client.Call");
      Text_IO.Put_Line
        (Stub_Adb,
         "                (""" & To_String (O.Location) & """,");
      Text_IO.Put_Line
        (Stub_Adb,
         "                 Payload,");
      Text_IO.Put
        (Stub_Adb,
         "                 """ & SOAPAction & """");

      --  Check if we need to generate proxy authentication

      if O.Proxy = SOAP.Client.Not_Specified then
         Text_IO.Put_Line (Stub_Adb, ");");

      else
         Text_IO.Put_Line (Stub_Adb, ",");
         Text_IO.Put_Line
           (Stub_Adb,
            "                 Proxy      => """
              & To_String (O.Proxy) & """,");
         Text_IO.Put_Line
           (Stub_Adb,
            "                 Proxy_User => """
              & To_String (O.P_User) & """,");
         Text_IO.Put_Line
           (Stub_Adb,
            "                 Proxy_Pwd  => """
              & To_String (O.P_Pwd) & """);");
      end if;

      Text_IO.Put_Line
        (Stub_Adb,
         "         R_Param  : constant SOAP.Parameters.List");

      Text_IO.Put_Line
        (Stub_Adb,
         "           := SOAP.Message.Parameters (Response);");

      Text_IO.Put_Line (Stub_Adb, "      begin");
      Text_IO.Put_Line
        (Stub_Adb,
         "         if SOAP.Message.Response.Is_Error (Response) then");
      Text_IO.Put_Line
        (Stub_Adb,
         "            Ada.Exceptions.Raise_Exception");
      Text_IO.Put_Line
        (Stub_Adb,
         "              (SOAP.SOAP_Error'Identity,");

      if WSDL.Parameters.Length (Fault) = 1 then
         Text_IO.Put_Line
           (Stub_Adb,
            "               SOAP.Parameters.Get (R_Param, "
              & '"' & To_String (Fault.Name) & """));");
      else
         Text_IO.Put_Line
           (Stub_Adb,
            "               SOAP.Parameters.Get (R_Param, "
              & """faultstring""));");
      end if;

      if Output /= null then
         Text_IO.Put_Line
           (Stub_Adb,
            "         else");
         Text_IO.Put_Line
           (Stub_Adb,
            "            declare");

         Text_IO.Put
           (Stub_Adb,
            "               Result : constant ");

         Text_IO.Put_Line (Stub_Adb, Result_Type (O, Proc, Output));

         if WSDL.Parameters.Length (Output) = 1 then
            --  A single parameter is returned

            case Output.Mode is

               when WSDL.Parameters.K_Simple =>

                  if Output.P_Type = WSDL.P_B64 then
                     Text_IO.Put_Line
                       (Stub_Adb,
                        "                 "
                          & ":= V (SOAP_Base64'(SOAP.Parameters.Get "
                          & "(R_Param, """
                          & To_String (Output.Name) & """)));");
                  else
                     Text_IO.Put_Line
                       (Stub_Adb,
                        "                 := SOAP.Parameters.Get (R_Param, """
                          & To_String (Output.Name)
                          & """);");
                  end if;

               when WSDL.Parameters.K_Derived =>

                  if  Output.Parent_Type = WSDL.P_B64 then
                     Text_IO.Put_Line
                       (Stub_Adb,
                        "                 "
                          & ":= V (SOAP_Base64'(SOAP.Parameters.Get "
                          & "(R_Param, """
                          & To_String (Output.Name) & """)));");
                  else
                     Text_IO.Put_Line
                       (Stub_Adb,
                        "                 := "
                          & Result_Type (O, Proc, Output));
                     Text_IO.Put_Line
                       (Stub_Adb,
                        "                   ("
                          & WSDL.To_Ada (Output.Parent_Type)
                          & "'(SOAP.Parameters.Get (R_Param, """
                          & To_String (Output.Name)
                          & """)));");
                  end if;

               when WSDL.Parameters.K_Enumeration =>

                  Text_IO.Put_Line
                    (Stub_Adb,
                     "                 := "
                       & Result_Type (O, Proc, Output));
                  Text_IO.Put_Line
                    (Stub_Adb,
                     "                   ("
                       & To_String (Output.E_Name) & "_Type'Value"
                       & " (SOAP.Utils.Get"
                       & " (SOAP.Parameters.Argument (R_Param, """
                       & To_String (Output.Name)
                       & """))));");

               when WSDL.Parameters.K_Array =>
                  Text_IO.Put_Line
                    (Stub_Adb,
                     "                 "
                       & ":= To_"
                       & Format_Name (O, To_String (Output.T_Name)) & "_Type");
                  Text_IO.Put_Line
                    (Stub_Adb,
                     "                 "
                       & "(V (SOAP_Array'(SOAP.Parameters.Get (R_Param, """
                       & To_String (Output.Name)
                       & """))));");

               when WSDL.Parameters.K_Record =>
                  Text_IO.Put_Line
                    (Stub_Adb,
                     "                 "
                       & ":= To_"
                       & Format_Name (O, To_String (Output.T_Name)) & "_Type");
                  Text_IO.Put_Line
                    (Stub_Adb,
                     "                 "
                       & "(SOAP_Record'(SOAP.Parameters.Get (R_Param, """
                       & To_String (Output.Name)
                       & """)));");
            end case;

         else
            Text_IO.Put
              (Stub_Adb,
               "                 := (");

            declare
               N : WSDL.Parameters.P_Set := Output;
            begin
               while N /= null loop
                  if N /= Output then
                     Text_IO.Put (Stub_Adb, ",");
                     Text_IO.New_Line (Stub_Adb);
                     Text_IO.Put (Stub_Adb, "                     ");
                  end if;

                  Output_Result (N);

                  N := N.Next;
               end loop;
            end;

            Text_IO.Put_Line (Stub_Adb, ");");
         end if;

         Text_IO.Put_Line (Stub_Adb, "            begin");
         Text_IO.Put_Line (Stub_Adb, "               return Result;");
         Text_IO.Put_Line (Stub_Adb, "            end;");
      end if;

      Text_IO.Put_Line (Stub_Adb, "         end if;");
      Text_IO.Put_Line (Stub_Adb, "      end;");

      Text_IO.Put_Line (Stub_Adb, "   end " & L_Proc & ';');
   end New_Procedure;

   -------------------
   -- Start_Service --
   -------------------

   procedure Start_Service
     (O             : in out Object;
      Name          : in     String;
      Documentation : in     String;
      Location      : in     String)
   is
      pragma Unreferenced (Location, Documentation);

      U_Name : constant String := To_Unit_Name (Format_Name (O, Name));
   begin
      --  Spec

      Text_IO.Put_Line (Stub_Ads, "pragma Warnings (Off);");
      Text_IO.New_Line (Stub_Ads);
      Text_IO.Put_Line (Stub_Ads, "with Ada.Calendar;");
      Text_IO.New_Line (Stub_Ads);
      Text_IO.Put_Line (Stub_Ads, "with " & U_Name & ".Types;");
      Text_IO.New_Line (Stub_Ads);
      Text_IO.Put_Line (Stub_Ads, "package " & U_Name & ".Client is");
      Text_IO.New_Line (Stub_Ads);
      Text_IO.Put_Line (Stub_Ads, "   use " & U_Name & ".Types;");

      --  Body

      Text_IO.Put_Line (Stub_Adb, "pragma Warnings (Off);");
      Text_IO.New_Line (Stub_Adb);
      Text_IO.Put_Line (Stub_Adb, "with Ada.Exceptions;");
      Text_IO.New_Line (Stub_Adb);
      Text_IO.Put_Line (Stub_Adb, "with SOAP.Client;");
      Text_IO.Put_Line (Stub_Adb, "with SOAP.Message.Payload;");
      Text_IO.Put_Line (Stub_Adb, "with SOAP.Message.Response;");
      Text_IO.Put_Line (Stub_Adb, "with SOAP.Parameters;");
      Text_IO.Put_Line (Stub_Adb, "with SOAP.Types;");
      Text_IO.Put_Line (Stub_Adb, "with SOAP.Utils;");
      Text_IO.New_Line (Stub_Adb);
      Text_IO.Put_Line (Stub_Adb, "package body " & U_Name & ".Client is");
      Text_IO.New_Line (Stub_Adb);
      Text_IO.Put_Line (Stub_Adb, "   use SOAP.Types;");
      Text_IO.Put_Line (Stub_Adb, "   use type SOAP.Parameters.List;");
      Text_IO.New_Line (Stub_Adb);
      Text_IO.Put_Line (Stub_Adb, "   pragma Style_Checks (Off);");
   end Start_Service;

end Stub;
