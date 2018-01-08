------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2003-2017, AdaCore                     --
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

with SOAP.Client;

separate (WSDL2AWS.Generator)
package body Stub is

   -----------------
   -- End_Service --
   -----------------

   procedure End_Service
     (O    : in out Object;
      Name : String)
   is
      U_Name : constant String := To_Unit_Name (Format_Name (O, Name));
   begin
      --  Spec

      Text_IO.New_Line (Stub_Ads);
      Text_IO.Put_Line (Stub_Ads, "private");
      Text_IO.New_Line (Stub_Ads);
      Text_IO.Put_Line
        (Stub_Ads, "   Connection : constant AWS.Client.HTTP_Connection :=");
      Text_IO.Put_Line
        (Stub_Ads, "                  AWS.Client.Create");
      Text_IO.Put_Line
        (Stub_Ads, "                    (URL,");
      Text_IO.Put
        (Stub_Ads, "                     Timeouts   => Timeouts");

      --  Check if we need to generate proxy authentication

      if O.Proxy = SOAP.Client.Not_Specified then
         Text_IO.Put_Line (Stub_Ads, ");");

      else
         Text_IO.Put_Line (Stub_Ads, ",");
         Text_IO.Put_Line
           (Stub_Ads,
            "                     Proxy      => """
              & To_String (O.Proxy) & """,");
         Text_IO.Put_Line
           (Stub_Ads,
            "                     Proxy_User => """
              & To_String (O.P_User) & """,");
         Text_IO.Put_Line
           (Stub_Ads,
            "                     Proxy_Pwd  => """
              & To_String (O.P_Pwd) & """);");
      end if;

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
     (O             : in out Object;
      Proc          : String;
      Documentation : String;
      SOAPAction    : String;
      Namespace     : SOAP.Name_Space.Object;
      Input         : WSDL.Parameters.P_Set;
      Output        : WSDL.Parameters.P_Set;
      Fault         : WSDL.Parameters.P_Set)
   is
      use type WSDL.Parameters.P_Set;

      procedure Output_Parameter
        (K      : Positive;
         Prefix : String;
         N      : WSDL.Parameters.P_Set);
      --  Ouptut parameter

      procedure Output_Simple
        (K      : Positive;
         Prefix : String;
         N      : WSDL.Parameters.P_Set);
      --  Output a simple parameter

      procedure Output_Derived
        (K      : Positive;
         Prefix : String;
         N      : WSDL.Parameters.P_Set);
      --  Output a derived type parameter

      procedure Output_Enumeration
        (K      : Positive;
         Prefix : String;
         N      : WSDL.Parameters.P_Set);
      --  Output an enumeration type parameter

      procedure Output_Record
        (K      : Positive;
         Prefix : String;
         N      : WSDL.Parameters.P_Set);
      --  Output a record parameter

      procedure Output_Array
        (K      : Positive;
         Prefix : String;
         N      : WSDL.Parameters.P_Set);
      --  Output an array parameter

      procedure Output_Result (N : WSDL.Parameters.P_Set);

      ------------------
      -- Output_Array --
      ------------------

      procedure Output_Array
        (K      : Positive;
         Prefix : String;
         N      : WSDL.Parameters.P_Set) is
      begin
         if Prefix = "" then
            --  Not inside a record
            Text_IO.Put
              (Stub_Adb,
               (if WSDL.Parameters.Is_Uniq (N.all)
                then "SOAP_Array" else "SOAP_Set")
               & "'(A (To_Object_Set ("
               & Format_Name (O, To_String (N.Name))
               & "), """ & To_String (N.Name) & """, """
               & WSDL.Types.Name (N.Typ, True) & """))");

         else
            Text_IO.Put
              (Stub_Adb,
               (if WSDL.Parameters.Is_Uniq (N.all)
                then "SOAP_Array" else "SOAP_Set")
               & "'(A (To_Object_Set ("
               & Prefix & Format_Name (O, To_String (N.Name))
               & ".Item.all" & "), """ & To_String (N.Name) & """, """
               & WSDL.Types.Name (N.Typ, True) & """))");
         end if;

         Output_Parameter (K + 1, Prefix, N.Next);
      end Output_Array;

      --------------------
      -- Output_Derived --
      --------------------

      procedure Output_Derived
        (K      : Positive;
         Prefix : String;
         N      : WSDL.Parameters.P_Set)  is
      begin
         Text_IO.Put
           (Stub_Adb,
            WSDL.Parameters.To_SOAP
              (P         => N.all,
               Object    => Format_Name (O, To_String (N.Name)),
               Name      => Format_Name (O, To_String (N.Name)),
               Type_Name => WSDL.Types.Name (N.Typ, NS => True)));

         if Prefix /= "" and then N.Next /= null then
            Text_IO.Put (Stub_Adb, ",");
         end if;

         Output_Parameter (K + 1, Prefix, N.Next);
      end Output_Derived;

      ------------------------
      -- Output_Enumeration --
      ------------------------

      procedure Output_Enumeration
        (K      : Positive;
         Prefix : String;
         N      : WSDL.Parameters.P_Set) is
      begin
         Text_IO.Put (Stub_Adb, "SOAP.Types.E");

         Text_IO.Put
           (Stub_Adb,
            " (Types.Image ("
              & Prefix & Format_Name (O, To_String (N.Name))
              & "), """
              & WSDL.Types.Name (N.Typ)
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
        (K      : Positive;
         Prefix : String;
         N      : WSDL.Parameters.P_Set)
      is
         use Ada.Strings.Fixed;
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
               when WSDL.Types.K_Simple =>
                  Output_Simple (K, Prefix, N);

               when WSDL.Types.K_Derived =>
                  Output_Derived (K, Prefix, N);

               when WSDL.Types.K_Enumeration =>
                  Output_Enumeration (K, Prefix, N);

               when WSDL.Types.K_Array =>
                  Output_Array (K, Prefix, N);

               when WSDL.Types.K_Record =>
                  Output_Record (K + 1, Prefix, N);
            end case;
         end if;
      end Output_Parameter;

      -------------------
      -- Output_Record --
      -------------------

      procedure Output_Record
        (K      : Positive;
         Prefix : String;
         N      : WSDL.Parameters.P_Set) is
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

      procedure Output_Result (N : WSDL.Parameters.P_Set) is
         use all type SOAP.WSDL.Parameter_Type;
         use type WSDL.Types.Kind;
      begin
         if N.Mode = WSDL.Types.K_Array then
            declare
               Name : constant String :=
                        Format_Name (O, WSDL.Types.Name (N.Typ));
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

            if N.Mode = WSDL.Types.K_Simple
                 and then
               SOAP.WSDL.To_Type (WSDL.Types.Name (N.Typ)) = P_String
            then
               --  First call operator to convert the string to an unbounded
               --  string.
               Text_IO.Put (Stub_Adb, "+");
            end if;

            if N.Mode = WSDL.Types.K_Derived then
               Text_IO.Put
                 (Stub_Adb,
                  WSDL.Parameters.From_SOAP
                    (N.all,
                     Object       => "SOAP.Parameters.Get (R_Param, """
                                     & To_String (N.Name) & """)",
                     Is_SOAP_Type => True));
            else
               Text_IO.Put
                 (Stub_Adb,
                  "SOAP.Parameters.Get (R_Param, """
                  & To_String (N.Name) & """)");
            end if;
         end if;
      end Output_Result;

      -------------------
      -- Output_Simple --
      -------------------

      procedure Output_Simple
        (K      : Positive;
         Prefix : String;
         N      : WSDL.Parameters.P_Set)
      is
         P_Type : constant SOAP.WSDL.Parameter_Type :=
                    SOAP.WSDL.To_Type (WSDL.Types.Name (N.Typ));
      begin
         if Prefix /= "" then
            --  Inside a record
            Text_IO.Put
              (Stub_Adb,
               SOAP.WSDL.Set_Routine (P_Type, Constrained => True));
         else
            Text_IO.Put (Stub_Adb, SOAP.WSDL.Set_Routine (P_Type));
         end if;

         Text_IO.Put
           (Stub_Adb,
            " (" & Prefix & Format_Name (O, To_String (N.Name))
              & ", """ & To_String (N.Name) & ""","
              & " Type_Name => """ & WSDL.Types.Name (N.Typ, True) & """)");

         if Prefix /= "" and then N.Next /= null then
            Text_IO.Put (Stub_Adb, ",");
         end if;

         Output_Parameter (K + 1, Prefix, N.Next);
      end Output_Simple;

      L_Proc : constant String := Format_Name (O, Proc);

      use all type SOAP.WSDL.Parameter_Type;
      use type SOAP.Name_Space.Object;

   begin
      --  Spec

      Text_IO.New_Line (Stub_Ads);

      Text_IO.Put (Stub_Ads, "   ");
      Put_Header (Stub_Ads, O, Proc, Input, Output, Mode => Stub_Spec);
      Put_Header (Stub_Ads, O, Proc, Input, Output, Mode => C_Stub_Spec);

      Output_Comment (Stub_Ads, Documentation, Indent => 3);

      Text_IO.Put_Line
        (Stub_Ads, "   --  Raises SOAP.SOAP_Error if the procedure fails");

      --  Body

      Text_IO.New_Line (Stub_Adb);
      Header_Box (O, Stub_Adb, Format_Name (O, Proc));
      Text_IO.New_Line (Stub_Adb);

      Put_Header (Stub_Adb, O, Proc, Input, Output, Mode => C_Stub_Body);

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
        (Stub_Adb, "        (""" & To_String (O.Prefix) & Proc & """, P_Set");

      if Namespace = SOAP.Name_Space.No_Name_Space then
         Text_IO.Put_Line (Stub_Adb, ");");
      else
         Text_IO.Put_Line (Stub_Adb, ",");
         Text_IO.Put_Line
           (Stub_Adb, "         SOAP.Name_Space.Create ("""
            & SOAP.Name_Space.Name (Namespace) & """, """
            & SOAP.Name_Space.Value (Namespace) & """));");
      end if;

      if O.Debug then
         Text_IO.New_Line (Stub_Adb);
         Text_IO.Put_Line
           (Stub_Adb,
            "      Put_Line (""[CLIENT/" & L_Proc & "] Payload : """);
         Text_IO.Put_Line
           (Stub_Adb,
            "                & SOAP.Message.XML.Image (Payload, Schema));");
      end if;

      if O.Traces then
         Text_IO.Put_Line
            (Stub_Adb,
             "      Pre_Call_Callback (Connection, Payload, Schema);");
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
         "                (Connection, """
         & To_String (O.Prefix) & SOAPAction & """,");
      Text_IO.Put_Line
        (Stub_Adb,
         "                 Payload, Schema => Schema);");

      Text_IO.Put_Line
        (Stub_Adb,
         "         R_Param  : constant SOAP.Parameters.List");

      Text_IO.Put_Line
        (Stub_Adb,
         "           := SOAP.Message.Parameters (Response);");

      Text_IO.Put_Line (Stub_Adb, "      begin");

      if O.Debug then
         Text_IO.Put_Line
           (Stub_Adb,
            "         Put_Line (""[CLIENT/" & L_Proc & "] Response : """);
         Text_IO.Put_Line
           (Stub_Adb,
            "                   "
            & "& SOAP.Message.XML.Image (Response, Schema));");
         Text_IO.New_Line (Stub_Adb);
      end if;

      if O.Traces then
         Text_IO.Put_Line
            (Stub_Adb,
             "         Post_Call_Callback (Connection," &
             " Payload, Response, Schema);");
      end if;

      Text_IO.Put_Line
        (Stub_Adb,
         "         if SOAP.Message.Response.Is_Error (Response) then");
      Text_IO.Put_Line
        (Stub_Adb,
         "            raise SOAP.SOAP_Error with");

      if WSDL.Parameters.Length (Fault) = 1 then
         Text_IO.Put_Line
           (Stub_Adb,
            "               SOAP.Parameters.Get (R_Param, "
              & '"' & To_String (Fault.Name) & """);");
      else
         Text_IO.Put_Line
           (Stub_Adb,
            "               SOAP.Parameters.Get (R_Param, "
              & """faultstring"");");
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

            declare
               T_Name : constant String := WSDL.Types.Name (Output.Typ);
            begin
               Text_IO.Put (Stub_Adb, "                 := ");

               case Output.Mode is

                  when WSDL.Types.K_Simple =>

                     if SOAP.WSDL.To_Type (T_Name) = P_B64 then
                        Text_IO.Put_Line
                          (Stub_Adb,
                           "V (SOAP_Base64'(SOAP.Parameters.Get "
                           & "(R_Param, """
                           & To_String (Output.Name) & """)));");

                     elsif SOAP.WSDL.To_Type (T_Name) = P_Character then
                        Text_IO.Put
                          (Stub_Adb,
                           " SOAP.Utils.Get "
                           & "(SOAP.Parameters.Argument (R_Param, """);
                        Text_IO.Put      (Stub_Adb, To_String (Output.Name));
                        Text_IO.Put_Line (Stub_Adb, """));");

                     else
                        Text_IO.Put_Line
                          (Stub_Adb,
                           "SOAP.Parameters.Get"
                           & " (R_Param, """
                           & To_String (Output.Name)
                           & """);");
                     end if;

                  when WSDL.Types.K_Derived =>
                     Text_IO.Put
                       (Stub_Adb,
                        WSDL.Parameters.From_SOAP
                          (Output.all,
                           Object => "SOAP.Parameters.Get (R_Param, """
                                     & To_String (Output.Name) & '"',
                           Is_SOAP_Type => True));
                     Text_IO.Put_Line (Stub_Adb, ");");

                  when WSDL.Types.K_Enumeration =>

                     Text_IO.Put_Line
                       (Stub_Adb, Result_Type (O, Proc, Output));
                     Text_IO.Put_Line
                       (Stub_Adb,
                        "                   ("
                        & T_Name & "_Type'Value"
                        & " (SOAP.Utils.Get"
                        & " (SOAP.Parameters.Argument (R_Param, """
                        & To_String (Output.Name)
                        & """))));");

                  when WSDL.Types.K_Array =>
                     Text_IO.Put_Line
                       (Stub_Adb,
                        "To_" & Format_Name (O, T_Name) & "_Type");
                     Text_IO.Put_Line
                       (Stub_Adb,
                        "                 "
                        & "(V (SOAP_Array'(SOAP.Parameters.Get (R_Param, """
                        & To_String (Output.Name)
                        & """))));");

                  when WSDL.Types.K_Record =>
                     Text_IO.Put_Line
                       (Stub_Adb,
                        "To_" & Format_Name (O, T_Name) & "_Type");
                     Text_IO.Put_Line
                       (Stub_Adb,
                        "                 "
                        & "(SOAP_Record'(SOAP.Parameters.Get (R_Param, """
                        & To_String (Output.Name)
                        & """)));");
               end case;
            end;

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

      --  Body stub based

      Text_IO.New_Line (Stub_Adb);

      Text_IO.Put (Stub_Adb, "   ");
      Put_Header (Stub_Adb, O, Proc, Input, Output, Mode => Stub_Body);

      Text_IO.Put_Line
        (Stub_Adb, "      Connection : AWS.Client.HTTP_Connection;");
      Text_IO.Put_Line
        (Stub_Adb, "   begin");
      Text_IO.Put_Line
        (Stub_Adb, "      AWS.Client.Create");
      Text_IO.Put_Line
        (Stub_Adb, "        (Connection, Endpoint,");
      Text_IO.Put_Line
        (Stub_Adb, "         Persistent => False,");
      Text_IO.Put
        (Stub_Adb, "         Timeouts   => Timeouts");

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

      if Output /= null then
         Text_IO.Put_Line (Stub_Adb, "      declare");
         Text_IO.Put_Line
           (Stub_Adb, "         Result : constant "
            & Result_Type (O, Proc, Output) & " :=");
         Text_IO.Put (Stub_Adb, "              ");
      end if;

      Text_IO.Put (Stub_Adb, "      Client." & L_Proc & " (Connection");

      declare
         N  : WSDL.Parameters.P_Set := Input;
      begin
         while N /= null loop
            declare
               Name : constant String := Format_Name (O, To_String (N.Name));
            begin
               Text_IO.Put (Stub_Adb, ", " & Name);
            end;
            N := N.Next;
         end loop;
      end;

      if O.Traces then
         Text_IO.Put
           (Stub_Adb, ", Pre_Call_Callback, Post_Call_Callback");
      end if;

      Text_IO.Put_Line (Stub_Adb, ");");

      if Output /= null then
         Text_IO.Put_Line (Stub_Adb, "      begin");
         Text_IO.Put (Stub_Adb, "   ");
      end if;

      Text_IO.Put_Line (Stub_Adb, "      AWS.Client.Close (Connection);");

      if Output /= null then
         Text_IO.Put_Line (Stub_Adb, "         return Result;");
         Text_IO.Put_Line (Stub_Adb, "      end;");
      end if;

      Text_IO.Put_Line (Stub_Adb, "   exception");
      Text_IO.Put_Line (Stub_Adb, "      when others =>");
      Text_IO.Put_Line (Stub_Adb, "         AWS.Client.Close (Connection);");
      Text_IO.Put_Line (Stub_Adb, "         raise;");
      Text_IO.Put_Line (Stub_Adb, "   end " & L_Proc & ';');
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
      pragma Unreferenced (Root_Documentation, Location);

      U_Name : constant String := To_Unit_Name (Format_Name (O, Name));
   begin
      --  Spec

      Text_IO.Put_Line (Stub_Ads, "pragma Warnings (Off);");
      Text_IO.New_Line (Stub_Ads);
      With_Unit (Stub_Ads, "Ada.Calendar", Elab => Off);
      Text_IO.New_Line (Stub_Ads);
      if O.Traces then
         With_Unit (Stub_Ads, "SOAP.Client.Callback", Elab => Off);
      end if;
      With_Unit (Stub_Ads, "SOAP.Types", Elab => Children);
      Text_IO.New_Line (Stub_Ads);
      With_Unit (Stub_Ads, U_Name & ".Types", Elab => Off);
      Text_IO.New_Line (Stub_Ads);

      Output_Comment (Stub_Ads, Documentation, Indent => 0);
      Text_IO.New_Line (Stub_Ads);

      Text_IO.Put_Line (Stub_Ads, "package " & U_Name & ".Client is");
      Text_IO.New_Line (Stub_Ads);
      Text_IO.Put_Line (Stub_Ads, "   use " & U_Name & ".Types;");

      if O.Traces then
         Text_IO.Put_Line (Stub_Ads, "   use SOAP.Client.Callback;");
      end if;

      Text_IO.New_Line (Stub_Ads);
      Text_IO.Put_Line
        (Stub_Ads, "   Connection : constant AWS.Client.HTTP_Connection;");

      --  Body

      Text_IO.Put_Line (Stub_Adb, "pragma Warnings (Off);");
      Text_IO.New_Line (Stub_Adb);

      if O.Debug then
         With_Unit (Stub_Adb, "Ada.Text_IO", Elab => Off);
         With_Unit (Stub_Adb, "SOAP.Message.XML");
      end if;

      With_Unit (Stub_Adb, "SOAP.Client");
      With_Unit (Stub_Adb, "SOAP.Message.Payload", Elab => Children);
      With_Unit (Stub_Adb, "SOAP.Message.Response");
      With_Unit (Stub_Adb, "SOAP.Message.XML");
      With_Unit (Stub_Adb, "SOAP.Name_Space");
      With_Unit (Stub_Adb, "SOAP.Parameters");
      With_Unit (Stub_Adb, "SOAP.Utils");
      Text_IO.New_Line (Stub_Adb);
      Text_IO.Put_Line (Stub_Adb, "package body " & U_Name & ".Client is");
      Text_IO.New_Line (Stub_Adb);

      if O.Debug then
         Text_IO.Put_Line (Stub_Adb, "   use Ada.Text_IO;");
      end if;

      Text_IO.Put_Line (Stub_Adb, "   use SOAP.Types;");
      Text_IO.Put_Line (Stub_Adb, "   use type SOAP.Parameters.List;");
      Text_IO.New_Line (Stub_Adb);
      Text_IO.Put_Line (Stub_Adb, "   pragma Style_Checks (Off);");
      Text_IO.New_Line (Stub_Adb);
   end Start_Service;

end Stub;
