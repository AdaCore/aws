------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                            Copyright (C) 2003                            --
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

with AWS.URL;
with AWS.Utils;

separate (SOAP.Generator)
package body Skel is

   -----------------
   -- End_Service --
   -----------------

   procedure End_Service
     (O    : in out Object;
      Name : in     String)
   is
      L_Name : constant String := Format_Name (O, Name);
   begin
      --  Spec

      Text_IO.New_Line (Skel_Ads);
      Text_IO.Put_Line (Skel_Ads, "end " & L_Name & ".Server;");

      --  Body

      Text_IO.New_Line (Skel_Adb);
      Text_IO.Put_Line (Skel_Adb, "end " & L_Name & ".Server;");
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
      pragma Unreferenced (SOAPAction);
      pragma Unreferenced (Namespace);
      pragma Unreferenced (Fault);

      use Ada.Strings.Fixed;
      use type SOAP.WSDL.Parameters.P_Set;
      use type SOAP.WSDL.Parameters.Kind;

      procedure Output_Parameters (N : in WSDL.Parameters.P_Set);
      --  Output parameters

      L_Proc  : constant String := Format_Name (O, Proc);
      Max_Len : Positive := 1;

      N       : WSDL.Parameters.P_Set;

      -----------------------
      -- Output_Parameters --
      -----------------------

      procedure Output_Parameters (N : in WSDL.Parameters.P_Set) is
         use type WSDL.Parameters.Kind;
         R : WSDL.Parameters.P_Set;
      begin
         Text_IO.Put      (Skel_Adb, "           ");

         if N.Mode = WSDL.Parameters.K_Composite then

            if Utils.Is_Array (To_String (N.C_Name)) then
               raise Constraint_Error;

            else
               Text_IO.Put (Skel_Adb, " := (");

               R := N.P;

               while R /= null loop
                  Text_IO.Put
                    (Skel_Adb, "SOAP.Parameters.Get ("
                       & Format_Name (O, To_String (N.C_Name)) & "_Record, """
                       & Format_Name (O, To_String (R.Name)) & """)");

                  if R.Next /= null then
                     Text_IO.Put_Line (Skel_Adb, ",");
                     Text_IO.Put      (Skel_Adb, "                ");
                  else
                     Text_IO.Put_Line (Skel_Adb, ");");
                  end if;

                  R := R.Next;
               end loop;
            end if;

         else
            Text_IO.Put      (Skel_Adb, " := SOAP.Parameters.Get (Params, """);
            Text_IO.Put      (Skel_Adb, To_String (N.Name));
            Text_IO.Put_Line (Skel_Adb, """);");
         end if;
      end Output_Parameters;

   begin
      --  Spec

      Text_IO.New_Line (Skel_Ads);
      Text_IO.Put_Line (Skel_Ads, "   generic");
      Text_IO.Put      (Skel_Ads, "      with ");

      Put_Header       (Skel_Ads, O, Proc, Input, Output);
      Text_IO.Put_Line (Skel_Ads, ";");

      Text_IO.Put_Line
        (Skel_Ads, "   function " & L_Proc & "_CB");
      Text_IO.Put_Line
        (Skel_Ads, "      (Request : in AWS.Status.Data)");
      Text_IO.Put_Line
        (Skel_Ads, "       return AWS.Response.Data;");

      --  Body

      Text_IO.New_Line (Skel_Adb);
      Header_Box (O, Skel_Adb, L_Proc & "_CB");

      Text_IO.New_Line (Skel_Adb);
      Text_IO.Put_Line
        (Skel_Adb, "   function " & L_Proc & "_CB");
      Text_IO.Put_Line
        (Skel_Adb, "      (Request : in AWS.Status.Data)");
      Text_IO.Put_Line
        (Skel_Adb, "       return AWS.Response.Data");
      Text_IO.Put_Line (Skel_Adb, "   is");
      Text_IO.Put_Line
        (Skel_Adb, "      Payload  : constant SOAP.Message.Payload.Object");
      Text_IO.Put
        (Skel_Adb, "               := SOAP.Message.XML.Load_Payload");
      Text_IO.Put_Line
        (Skel_Adb, " (AWS.Status.Payload (Request));");
      Text_IO.Put_Line
        (Skel_Adb, "      Params   : constant SOAP.Parameters.List");
      Text_IO.Put_Line
        (Skel_Adb, "               := SOAP.Message.Parameters (Payload);");
      Text_IO.Put_Line
        (Skel_Adb, "      Response : SOAP.Message.Response.Object;");
      Text_IO.Put_Line
        (Skel_Adb, "      R_Params : SOAP.Parameters.List;");
      Text_IO.Put_Line (Skel_Adb, "   begin");
      Text_IO.Put_Line
        (Skel_Adb, "      Response := SOAP.Message.Response.From (Payload);");
      Text_IO.New_Line (Skel_Adb);

      Text_IO.Put_Line
        (Skel_Adb, "      declare");

      --  Find maximum parameter name length to align them

      N := Input;

      while N /= null loop
         Max_Len := Positive'Max
           (Max_Len, Format_Name (O, To_String (N.Name))'Length);
         N := N.Next;
      end loop;

      --  Input parameters

      N := Input;

      while N /= null loop
         Text_IO.Put      (Skel_Adb, "         ");

         if N.Mode = WSDL.Parameters.K_Composite then

            if Utils.Is_Array (To_String (N.C_Name)) then
               Text_IO.Put_Line
                 (Skel_Adb,
                  Format_Name (O, To_String (N.C_Name)) & "_Array : "
                    & "constant SOAP.Types.SOAP_Array");
               Text_IO.Put_Line
                 (Skel_Adb,
                  "            := SOAP.Parameters.Get (Params, """
                    & To_String (N.Name) & """);");
               Text_IO.Put      (Skel_Adb, "         ");

            else
               Text_IO.Put_Line
                 (Skel_Adb,
                  Format_Name (O, To_String (N.C_Name)) & "_Record : "
                    & "constant SOAP.Types.SOAP_Record");
               Text_IO.Put_Line
                 (Skel_Adb,
                  "            := SOAP.Parameters.Get (Params, """
                    & To_String (N.Name) & """);");
               Text_IO.Put      (Skel_Adb, "         ");
            end if;
         end if;

         declare
            Name : constant String
              := Format_Name (O, To_String (N.Name));
         begin
            Text_IO.Put   (Skel_Adb, Name);
            Text_IO.Put   (Skel_Adb, (Max_Len - Name'Length) * ' ');
         end;

         Text_IO.Put      (Skel_Adb, " : constant ");

         if N.Mode = WSDL.Parameters.K_Simple then
            Text_IO.Put_Line (Skel_Adb, WSDL.To_Ada (N.P_Type));
            Output_Parameters (N);
         else

            if Utils.Is_Array (To_String (N.C_Name)) then
               Text_IO.Put_Line
                 (Skel_Adb, Format_Name (O, To_String (N.C_Name)));
               Text_IO.Put_Line
                 (Skel_Adb, "            := To_"
                    & Format_Name (O, To_String (N.C_Name)) & " (V ("
                    & Format_Name (O, To_String (N.C_Name)) & "_Array));");

            else
               Text_IO.Put_Line
                 (Skel_Adb, Format_Name (O, To_String (N.C_Name)) & "_Type");
               Text_IO.Put_Line
                 (Skel_Adb, "            := To_"
                    & Format_Name (O, To_String (N.C_Name)) & "_Type ("
                    & Format_Name (O, To_String (N.C_Name)) & "_Record);");
            end if;
         end if;


         N := N.Next;
      end loop;

      --  Output parameters

      Text_IO.Put
        (Skel_Adb, "         Result : constant ");

      if Output.Next = null
        and then Output.Mode = WSDL.Parameters.K_Simple
      then
         Text_IO.Put_Line
           (Skel_Adb, WSDL.To_Ada (Output.P_Type));
      else
         Text_IO.Put_Line
           (Skel_Adb, L_Proc & "_Result");
      end if;

      Text_IO.Put_Line
        (Skel_Adb, "                := " & L_Proc);

      N := Input;

      while N /= null loop
         Text_IO.Put (Skel_Adb, "                     ");

         if N = Input then
            Text_IO.Put (Skel_Adb, "(");
         else
            Text_IO.Put (Skel_Adb, " ");
         end if;

         Text_IO.Put (Skel_Adb, Format_Name (O, To_String (N.Name)));

         if N.Next = null then
            Text_IO.Put_Line (Skel_Adb, ");");
         else
            Text_IO.Put_Line (Skel_Adb, ",");
         end if;

         N := N.Next;
      end loop;

      --  Set SOAP results

      Text_IO.Put_Line
        (Skel_Adb, "      begin");

      Text_IO.Put_Line
        (Skel_Adb, "         R_Params :=");
      Text_IO.Put
        (Skel_Adb, "            +");

      N := Output;

      while N /= null loop
         if N /= Output then
            Text_IO.Put
              (Skel_Adb, "            & ");
         end if;

         if N.Mode = WSDL.Parameters.K_Simple then
            Text_IO.Put (Skel_Adb, SOAP_Constructor (N.P_Type));

            if Output.Next = null then
               --  A single simple parameter as return

               Text_IO.Put
                 (Skel_Adb, " (Result, """ & To_String (N.Name) & """)");

            else
               --  Multiple value returned, this is a record

               Text_IO.Put
                 (Skel_Adb, " (Result."
                    & Format_Name (O, To_String (N.Name))
                    & ", """ & To_String (N.Name) & """)");
            end if;

         else
            if Utils.Is_Array (To_String (N.C_Name)) then

               if Output.Next = null then
                  --  A single array as returned parameter
                  Text_IO.Put
                    (Skel_Adb, " SOAP.Types.A (To_Object_Set (Result)"
                       &  ", """ & To_String (N.Name) & """)");

               else
                  --  Array here is part of an array
                  Text_IO.Put
                    (Skel_Adb, " SOAP.Types.A (To_Object_Set (Result."
                       & To_String (N.Name) & ".Item.all), """
                       & To_String (N.Name) & """)");
               end if;

            else
               Text_IO.Put
                 (Skel_Adb, " To_SOAP_Object (Result, """
                    &  To_String (N.Name) & """)");
            end if;
         end if;

         if N.Next = null then
            Text_IO.Put_Line (Skel_Adb, ";");
         else
            Text_IO.New_Line (Skel_Adb);
         end if;

         N := N.Next;
      end loop;

      Text_IO.Put_Line
        (Skel_Adb, "      end;");
      Text_IO.New_Line (Skel_Adb);

      Text_IO.Put_Line
        (Skel_Adb, "      SOAP.Message.Set_Parameters (Response, R_Params);");

      Text_IO.Put_Line
        (Skel_Adb, "      return SOAP.Message.Response.Build (Response);");

      Text_IO.Put_Line
        (Skel_Adb, "   exception");

      Text_IO.Put_Line
        (Skel_Adb, "      when SOAP.Types.Data_Error =>");

      Text_IO.Put_Line
        (Skel_Adb, "         --  Here we have a problem with some"
           & " parameters, return a SOAP error.");

      Text_IO.Put_Line
        (Skel_Adb, "         return SOAP.Message.Response.Build");
      Text_IO.Put_Line
        (Skel_Adb, "            (SOAP.Message.Response.Error.Build");
      Text_IO.Put_Line
        (Skel_Adb, "               (SOAP.Message.Response.Error.Client, """
           & "Parameter error""));");

      Text_IO.Put_Line (Skel_Adb, "   end " & L_Proc & "_CB;");
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
      pragma Unreferenced (Documentation);

      L_Name : constant String         := Format_Name (O, Name);
      URL    : constant AWS.URL.Object := AWS.URL.Parse (Location);
   begin
      --  Spec

      Text_IO.Put_Line (Skel_Ads, "with AWS.Status;");
      Text_IO.Put_Line (Skel_Ads, "with AWS.Response;");
      Text_IO.New_Line (Skel_Ads);
      Text_IO.Put_Line (Skel_Ads, "with " & L_Name & ".Types;");
      Text_IO.New_Line (Skel_Ads);
      Text_IO.Put_Line (Skel_Ads, "package " & L_Name & ".Server is");
      Text_IO.New_Line (Skel_Ads);
      Text_IO.Put_Line (Skel_Ads, "   use " & L_Name & ".Types;");
      Text_IO.New_Line (Skel_Ads);
      Text_IO.Put_Line
        (Skel_Ads,
         "   Port : constant := "
           & AWS.Utils.Image (AWS.URL.Port (URL)) & ';');


      --  Body

      Text_IO.Put_Line (Skel_Adb, "with SOAP.Message.Payload;");
      Text_IO.Put_Line (Skel_Adb, "with SOAP.Message.Response.Error;");
      Text_IO.Put_Line (Skel_Adb, "with SOAP.Message.XML;");
      Text_IO.Put_Line (Skel_Adb, "with SOAP.Parameters;");
      Text_IO.Put_Line (Skel_Adb, "with SOAP.Types;");
      Text_IO.New_Line (Skel_Adb);
      Text_IO.Put_Line (Skel_Adb, "package body " & L_Name & ".Server is");
      Text_IO.New_Line (Skel_Adb);
      Text_IO.Put_Line (Skel_Adb, "   use SOAP.Types;");
      Text_IO.Put_Line (Skel_Adb, "   use type SOAP.Parameters.List;");
   end Start_Service;

end Skel;
