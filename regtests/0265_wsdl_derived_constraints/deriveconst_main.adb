------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                       Copyright (C) 2015, AdaCore                        --
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

with Ada.Strings.Unbounded;
with Ada.Text_IO;

with AWS.Server.Status;
with AWS.URL;

with SOAP.Client;
with SOAP.Message.Payload;
with SOAP.Message.Response.Error;
with SOAP.Name_Space;
with SOAP.Parameters;
with SOAP.Types;

with CB;
with Deriveconst_Demo.Client;
with Deriveconst_Demo.Types;

procedure Deriveconst_Main is

   use Ada;
   use Ada.Strings.Unbounded;

   use AWS;
   use SOAP;
   use SOAP.Parameters;
   use SOAP.Types;

   use Deriveconst_Demo.Types;

   function "+"
     (Str : String)
      return Unbounded_String
      renames To_Unbounded_String;

   procedure Error (E : Message.Response.Error.Object);

   procedure Call
     (Id    : Integer;
      One   : Integer;
      Two   : Integer;
      Three : Long_Float;
      Four  : String;
      Six   : String;
      Seven : String;
      Eight : String);
   --  Send a Call display return error is any

   HTTP : Server.HTTP;
   URL  : AWS.URL.Object;

   ----------
   -- Call --
   ----------

   procedure Call
     (Id    : Integer;
      One   : Integer;
      Two   : Integer;
      Three : Long_Float;
      Four  : String;
      Six   : String;
      Seven : String;
      Eight : String)
   is
      TNS   : SOAP.Name_Space.Object := SOAP.Name_Space.Create
                ("tns", "http://www.ecerami.com/wsdl/DeriveconstService.wsdl");
      O_Set : Object_Set := (+S ("00000000", "item", "tns:Name"),
                             +S ((if Id = 3 then "x" else "abcdefgh"),
                                 "item", "tns:Name"));
      P_Set : Parameters.List :=
                +R ((+I (Id, "id", "xs:int"),
                     +I (One, "one", "tns:PercentCompleteInteger"),
                     +I (Two, "two", "tns:NonNegativeInt"),
                     +D (Three, "three", "tns:NonNegativeFloat"),
                     +S (Four, "four", "tns:Name"),
                     +A (O_Set, "five", "tns:ArrayOfName"),
                     +S (Six, "six", "tns:Address"),
                     +S (Seven, "seven", "tns:Code1"),
                     +S (Eight, "eight", "tns:Code2")),
                     "params", "tns:big", TNS);
      P     : Message.Payload.Object :=
                Message.Payload.Build
                  ("call", P_Set,
                    SOAP.Name_Space.Create
                      ("ns1", "urn:examples:deriveconstservice"));
      R     : constant Message.Response.Object'Class :=
                SOAP.Client.Call (AWS.URL.URL (URL), P, "call",
                                  Schema => Deriveconst_Demo.Schema);
   begin
      if R.Is_Error then
         Text_IO.Put_Line ("Id" & Integer'Image (Id));
         Error (Message.Response.Error.Object (R));
      else
         if Id = 2 then
            Text_IO.Put_Line ("OK");
         else
            Text_IO.Put_Line ("NOK");
         end if;
      end if;
   end Call;

   -----------
   -- Error --
   -----------

   procedure Error (E : Message.Response.Error.Object) is
      P : constant Parameters.List := SOAP.Message.Parameters (E);
   begin
      Text_IO.Put_Line
        ("Faultcode   : " & SOAP.Parameters.Get (P, "faultcode"));

      Text_IO.Put_Line
        ("Faultstring : " & SOAP.Parameters.Get (P, "faultstring"));
   end Error;

   A  : constant ArrayOfName_Type := (1 => "00000000", 2 => "abcdefgh");
   B1 : constant Big_Type := (1, 1, 1, 1.0, "abcdefgh", +A,
                              To_Address_Type (+"0987654"),
                              To_Code1_Type (+"A2"),
                              "A3b");

begin
   Text_IO.Put_Line ("Run OK");

   Server.Start (HTTP, "name", CB.CB'Access, Port => 0);
   URL := AWS.URL.Parse (Server.Status.Local_URL (HTTP));

   Deriveconst_Demo.Client.Call (B1, Endpoint => AWS.URL.URL (URL));

   --  One call with respected constraints

   Call (2, 99, 7, 9.0, "12345678", "12345", "A2", "A9z");

   --  Some calls with constraints error

   Call (3, 88, 7, 9.0, "12345678", "12345", "A2", "A9z");
   --  fails because of array

   Call (4, 101, 7, 9.0, "12345678", "12345", "A2", "A9z");
   Call (5, 99, 7, 9.0, "12345", "12345", "A2", "A9z");
   Call (6, 99, 7, -0.1, "12345678", "12345", "A2", "A9z");
   Call (7, 99, -7, 9.0, "12345678", "12345", "A2", "A9z");
   Call (8, 99, -7, 9.0, "12345678", "145", "A2", "A9z");
   Call (9, 99, -7, 9.0, "12345678", "145000000099887", "A2", "A9z");
   Call (10, 99, 7, 9.0, "12345678", "12345", "Ab", "A9z");
   Call (11, 99, 7, 9.0, "12345678", "12345", "Ab", "A#@");

   Server.Shutdown (HTTP);
end Deriveconst_Main;
