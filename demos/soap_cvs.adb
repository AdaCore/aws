------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                            Copyright (C) 2001                            --
--                      Dmitriy Anisimov - Pascal Obry                      --
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

--  A simple SOAP client that call each test on the SOAP 1.1 Validation Suite.
--  (see http://soap.weblogs.com/validator1). This can be used with soap_svs
--  the SOAP server that implements all Web Services for the validation.

with Ada.Text_IO;
with Ada.Calendar;

with SOAP.Types;
with SOAP.Parameters;
with SOAP.Client;
with SOAP.Message.Payload;
with SOAP.Message.Response.Error;

procedure SOAP_CVS is

   use Ada;
   use SOAP;
   use SOAP.Types;
   use type SOAP.Parameters.Set;

   -----------
   -- Error --
   -----------

   procedure Error (E : in Message.Response.Error.Object) is
      P : constant Parameters.Set := SOAP.Message.Parameters (E);
   begin
      Text_IO.Put_Line ("Faultcode   : "
                        & SOAP.Parameters.Get (P, "faultcode"));

      Text_IO.Put_Line ("Faultstring : "
                        & SOAP.Parameters.Get (P, "faultstring"));
   end Error;

   ----------------------
   -- countTheEntities --
   ----------------------

   procedure countTheEntities is

      P_Set : constant Parameters.Set := +S ("un<><<&'", "s");

      P     : Message.Payload.Object;

   begin
      P := Message.Payload.Build ("countTheEntities", P_Set);

      declare
         R : constant Message.Response.Object'Class :=
           SOAP.Client.Call
           ("http://pascal:8080/validator1", P);

         P : constant Parameters.Set
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

      P_Set : Parameters.Set :=
        +R ((+I (12, "moe"), +I (45, "larry"), +I (10, "curly")),
            "stooges");

      P     : Message.Payload.Object;
   begin
      P := Message.Payload.Build ("easyStructTest", P_Set);

      declare
         R : constant Message.Response.Object'Class :=
           SOAP.Client.Call
           ("http://pascal:8080/validator1", P);

         P : constant Parameters.Set
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

      P_Set : Parameters.Set :=
        +R ((+I (12, "moe"), +I (45, "larry"), +I (10, "curly")),
            "myStruct");

      P     : Message.Payload.Object;
   begin
      P := Message.Payload.Build ("echoStructTest", P_Set);

      declare
         R : constant Message.Response.Object'Class :=
           SOAP.Client.Call
           ("http://pascal:8080/validator1", P);

         P : constant Parameters.Set
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

      P_Set : Parameters.Set :=
        +I (12, "num") & B (True, "bool") & S ("Just me", "state")
        & F (3.8, "doub") & T (Clock, "dat", 3) & S ("%*kkk/:", "bin");

      P     : Message.Payload.Object;
   begin
      P := Message.Payload.Build ("manyTypesTest", P_Set);

      declare
         R : constant Message.Response.Object'Class :=
           SOAP.Client.Call
           ("http://pascal:8080/validator1", P);

         P : constant Parameters.Set
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

      P_Set : Parameters.Set := +A ((+S ("str 1"),
                                     +S ("str 2"),
                                     +S ("str 3"),
                                     +S ("str 4")),
                                    "myArray");

      P     : Message.Payload.Object;
   begin
      P := Message.Payload.Build ("moderateSizeArrayCheck", P_Set);

      declare
         R : constant Message.Response.Object'Class :=
           SOAP.Client.Call
           ("http://pascal:8080/validator1", P);

         P : constant Parameters.Set
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

      P_Set : Parameters.Set
        := +R
        ((+R ((1 => +R (S1, "test")), "year1999"),
          +R ((1 => +R ((1 => +R (S2, "day01")), "month04")), "year2000")),
         "myStruct");

      P     : Message.Payload.Object;
   begin
      P := Message.Payload.Build ("nestedStructTest", P_Set);

      declare
         R : constant Message.Response.Object'Class :=
           SOAP.Client.Call
           ("http://pascal:8080/validator1", P);

         P : constant Parameters.Set
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
      P_Set : Parameters.Set := +I (162, "myNumber");
      P     : Message.Payload.Object;
   begin
      P := Message.Payload.Build ("simpleStructReturnTest", P_Set);

      declare
         R : constant Message.Response.Object'Class :=
           SOAP.Client.Call
           ("http://pascal:8080/validator1", P);

         P : constant Parameters.Set
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

      P_Set : Parameters.Set;
      P     : Message.Payload.Object;
   begin
      P := Message.Payload.Build ("whichToolkit", P_Set);

      declare
         R : constant Message.Response.Object'Class :=
           SOAP.Client.Call
           ("http://pascal:8080/validator1", P);

         P : constant Parameters.Set := SOAP.Message.Parameters (R);

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
