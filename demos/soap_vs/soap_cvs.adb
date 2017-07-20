------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                    Copyright (C) 2000-2017, AdaCore                      --
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

--  A simple SOAP client that call each test on the SOAP 1.1 Validation Suite.
--  (see http://soap.weblogs.com/validator1). This can be used with soap_svs
--  the SOAP server that implements all Web Services for the validation.

with Ada.Text_IO;
with Ada.Calendar;

with SOAP.Types;
with SOAP.Parameters;
with SOAP.Client;
with SOAP.Message.Payload;
with SOAP.Message.Response;

procedure SOAP_CVS is

   use Ada;
   use SOAP;
   use SOAP.Types;
   use type SOAP.Parameters.List;

   Server     : constant String := "http://localhost:8080/validator1";
   SOAPAction : constant String := "/validator1";

   ----------------------
   -- countTheEntities --
   ----------------------

   procedure countTheEntities is

      P_Set : constant Parameters.List := +S ("un<><<&'", "s");

      P     : Message.Payload.Object;

   begin
      P := Message.Payload.Build ("countTheEntities", P_Set);

      declare
         R : constant Message.Response.Object'Class
           := SOAP.Client.Call (Server, P, SOAPAction);

         P : constant Parameters.List
           := SOAP.Message.Parameters (R);

         Answer : constant SOAP_Record := SOAP.Parameters.Get (P, "answer");
      begin
         Text_IO.Put_Line ("---------- countTheEntities ----------");
         Text_IO.Put_Line
           ("ctLeftAngleBrackets = "
            & Integer'Image (Get (V (Answer, "ctLeftAngleBrackets"))));
         Text_IO.New_Line;
      end;
   end countTheEntities;

   --------------------
   -- easyStructTest --
   --------------------

   procedure easyStructTest is

      P_Set : Parameters.List :=
        +R ((+I (12, "moe"), +I (45, "larry"), +I (10, "curly")),
            "stooges");

      P     : Message.Payload.Object;
   begin
      P := Message.Payload.Build ("easyStructTest", P_Set);

      declare
         R : constant Message.Response.Object'Class
           := SOAP.Client.Call (Server, P, SOAPAction);

         P : constant Parameters.List
           := SOAP.Message.Parameters (R);

         Answer : constant Integer := SOAP.Parameters.Get (P, "answer");
      begin
         Text_IO.Put_Line ("---------- easyStructTest ----------");
         Text_IO.Put_Line ("sum = " & Integer'Image (Answer));
         Text_IO.New_Line;
      end;
   end easyStructTest;

   --------------------
   -- echoStructTest --
   --------------------

   procedure echoStructTest is

      P_Set : Parameters.List :=
        +R ((+I (12, "moe"), +I (45, "larry"), +I (10, "curly")),
            "myStruct");

      P     : Message.Payload.Object;
   begin
      P := Message.Payload.Build ("echoStructTest", P_Set);

      declare
         R : constant Message.Response.Object'Class
           := SOAP.Client.Call (Server, P, SOAPAction);

         P : constant Parameters.List
           := SOAP.Message.Parameters (R);

         Answer : constant SOAP_Record := SOAP.Parameters.Get (P, "myStruct");
      begin
         Text_IO.Put_Line ("---------- echoStructTest ----------");
         Text_IO.Put_Line (Types.Image (Answer));
         Text_IO.New_Line;
      end;
   end echoStructTest;

   -------------------
   -- manyTypesTest --
   -------------------

   procedure manyTypesTest is

      use Ada.Calendar;

      P_Set : Parameters.List :=
        +I (12, "num") & B (True, "bool") & S ("Just me", "state")
        & F (3.8, "doub") & T (Clock, "dat")
        & S ("%*kkk/:", "bin");

      P     : Message.Payload.Object;
   begin
      P := Message.Payload.Build ("manyTypesTest", P_Set);

      declare
         R : constant Message.Response.Object'Class
           := SOAP.Client.Call (Server, P, SOAPAction);

         P : constant Parameters.List
           := SOAP.Message.Parameters (R);

         Answer : constant SOAP_Array := SOAP.Parameters.Get (P, "answer");
      begin
         Text_IO.Put_Line ("---------- manyTypesTest ----------");
         Text_IO.Put_Line (Types.Image (Answer));
         Text_IO.New_Line;
      end;
   end manyTypesTest;

   ----------------------------
   -- moderateSizeArrayCheck --
   ----------------------------

   procedure moderateSizeArrayCheck is

      P_Set : Parameters.List := +A ((+S ("str 1"),
                                     +S ("str 2"),
                                     +S ("str 3"),
                                     +S ("str 4")),
                                    "myArray");

      P     : Message.Payload.Object;
   begin
      P := Message.Payload.Build ("moderateSizeArrayCheck", P_Set);

      declare
         R : constant Message.Response.Object'Class
           := SOAP.Client.Call (Server, P, SOAPAction);

         P : constant Parameters.List
           := SOAP.Message.Parameters (R);

         Answer : constant String := SOAP.Parameters.Get (P, "answer");

      begin
         Text_IO.Put_Line ("---------- moderateSizeArrayCheck ----------");
         Text_IO.Put_Line (Answer);
         Text_IO.New_Line;
      end;
   end moderateSizeArrayCheck;

   ----------------------
   -- nestedStructTest --
   ----------------------

   procedure nestedStructTest is

      S1 : constant Object_Set :=
        (+I (1, "un"), +I (2, "deux"));

      S2 : constant Object_Set :=
        (+I (1, "moe"), +I (27, "larry"), +I (76, "curly"));

      P_Set : Parameters.List
        := +R
        ((+R ((1 => +R (S1, "test")), "year1999"),
          +R ((1 => +R ((1 => +R (S2, "day01")), "month04")), "year2000")),
         "myStruct");

      P     : Message.Payload.Object;
   begin
      P := Message.Payload.Build ("nestedStructTest", P_Set);

      declare
         R : constant Message.Response.Object'Class
           := SOAP.Client.Call (Server, P, SOAPAction);

         P : constant Parameters.List
           := SOAP.Message.Parameters (R);

         Answer : constant Integer := SOAP.Parameters.Get (P, "answer");

      begin
         Text_IO.Put_Line ("---------- nestedStructTest ----------");
         Text_IO.Put_Line (Integer'Image (Answer));
         Text_IO.New_Line;
      end;
   end nestedStructTest;

   ----------------------------
   -- simpleStructReturnTest --
   ----------------------------

   procedure simpleStructReturnTest is
      P_Set : Parameters.List := +I (162, "myNumber");
      P     : Message.Payload.Object;
   begin
      P := Message.Payload.Build ("simpleStructReturnTest", P_Set);

      declare
         R : constant Message.Response.Object'Class
           := SOAP.Client.Call (Server, P, SOAPAction);

         P : constant Parameters.List
           := SOAP.Message.Parameters (R);

         Answer : constant SOAP_Record := SOAP.Parameters.Get (P, "answer");

         V1 : constant Integer := Get (V (Answer, "times10"));
         V2 : constant Integer := Get (V (Answer, "times100"));
         V3 : constant Integer := Get (V (Answer, "times1000"));

      begin
         Text_IO.Put_Line ("---------- simpleStructReturnTest ----------");
         Text_IO.Put_Line (Integer'Image (V1));
         Text_IO.Put_Line (Integer'Image (V2));
         Text_IO.Put_Line (Integer'Image (V3));
         Text_IO.New_Line;
      end;
   end simpleStructReturnTest;

   ------------------
   -- whichToolkit --
   ------------------

   procedure whichToolkit is
      use SOAP.Parameters;

      P_Set : Parameters.List;
      P     : Message.Payload.Object;
   begin
      P := Message.Payload.Build ("whichToolkit", P_Set);

      declare
         R : constant Message.Response.Object'Class
           := SOAP.Client.Call (Server, P, SOAPAction);

         P : constant Parameters.List := SOAP.Message.Parameters (R);

         Answer : constant SOAP_Record := SOAP.Parameters.Get (P, "answer");

         toolkitDocsUrl : constant String
           := Get (V (Answer, "toolkitDocsUrl"));

         toolkitName    : constant String
           := Get (V (Answer, "toolkitName"));

         toolkitVersion : constant String
           := Get (V (Answer, "toolkitVersion"));

         toolkitOS      : constant String
           := Get (V (Answer, "toolkitOperatingSystem"));

      begin
         Text_IO.Put_Line ("---------- whichToolkit ----------");
         Text_IO.Put_Line (toolkitDocsUrl);
         Text_IO.Put_Line (toolkitName);
         Text_IO.Put_Line (toolkitVersion);
         Text_IO.Put_Line (toolkitOS);
         Text_IO.New_Line;
      end;
   end whichToolkit;

begin
   Text_IO.Put_Line ("==> Call countTheEntities");
   countTheEntities;

   Text_IO.Put_Line ("==> Call easyStructTest");
   easyStructTest;

   Text_IO.Put_Line ("==> Call echoStructTest");
   echoStructTest;

   Text_IO.Put_Line ("==> Call manyTypesTest");
   manyTypesTest;

   Text_IO.Put_Line ("==> Call moderateSizeArrayCheck");
   moderateSizeArrayCheck;

   Text_IO.Put_Line ("==> Call nestedStructTest");
   nestedStructTest;

   Text_IO.Put_Line ("==> Call simpleStructReturnTest");
   simpleStructReturnTest;

   Text_IO.Put_Line ("==> Call whichToolkit");
   whichToolkit;
end SOAP_CVS;
