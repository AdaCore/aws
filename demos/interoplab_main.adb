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

--  Test from the Interoplab Web site.
--  http://www.whitemesa.com/interop/proposal2.html
--
--  This program can be runned to do a simple validation of the SOAP/WSDL
--  implementation.

with Ada.Calendar;
with Ada.Integer_Text_IO;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with AWS.Translator;

with interopLab.Client;
with interopLab.Types;

procedure interopLab_Main is

   use Ada;
   use Ada.Strings.Unbounded;

   use interopLab.Types;

   function "+"
     (Str : in String)
      return Unbounded_String
      renames To_Unbounded_String;

   package LFIO is new Text_IO.Float_IO (Long_Float);

   ------------
   -- Output --
   ------------

   procedure Output (S : in SOAPStruct_Type) is
   begin
      Integer_Text_IO.Put (S.varInt); Text_IO.New_Line;
      LFIO.Put (S.varFloat, Exp => 0, Aft => 2); Text_IO.New_Line;
      Text_IO.Put_Line (To_String (S.varString));
   end Output;

   ------------------
   -- T_echoBase64 --
   ------------------

   procedure T_echoBase64 is
      V   : constant String
        := AWS.Translator.Base64_Encode ("AWS SOAP WSDL implementation");
      Res : constant String := interopLab.Client.echoBase64 (V);
   begin
      Text_IO.Put_Line ("Echo Base64");

      Text_IO.Put_Line (V);
      Text_IO.Put_Line (Res);
      Text_IO.New_Line;
   end T_echoBase64;

   -------------------
   -- T_echoBoolean --
   -------------------

   procedure T_echoBoolean is
      Res : Boolean;
   begin
      Text_IO.Put_Line ("Echo Boolean");

      Res := interopLab.Client.echoBoolean (True);
      Text_IO.Put_Line (Boolean'Image (Res));

      Res := interopLab.Client.echoBoolean (False);
      Text_IO.Put_Line (Boolean'Image (Res));
      Text_IO.New_Line;
   end T_echoBoolean;

   ----------------
   -- T_echoDate --
   ----------------

   procedure T_echoDate is
      use type Ada.Calendar.Time;

      T   : constant Ada.Calendar.Time
        := Ada.Calendar.Time_Of (2003, 3, 12, 39482.0);
      Res : Ada.Calendar.Time;
   begin
      Text_IO.Put_Line ("Echo Date");
      Res := interopLab.Client.echoDate (T);

      if Res = T then
         Text_IO.Put_Line ("ok");
      else
         Text_IO.Put_Line ("nok");
      end if;
      Text_IO.New_Line;
   end T_echoDate;

   -----------------
   -- T_echoFloat --
   -----------------

   procedure T_echoFloat is
   begin
      Text_IO.Put_Line ("Echo Float");
      LFIO.Put (interopLab.Client.echoFloat (2.345), Exp => 0);
      Text_IO.New_Line;
      LFIO.Put (interopLab.Client.echoFloat (456.8765), Exp => 0);
      Text_IO.New_Line;
      Text_IO.New_Line;
   end T_echoFloat;

   -------------------
   -- T_echoInteger --
   -------------------

   procedure T_echoInteger is
   begin
      Text_IO.Put_Line ("Echo Integer");
      Integer_Text_IO.Put (interopLab.Client.echoInteger (12));
      Text_IO.New_Line;
      Integer_Text_IO.Put (interopLab.Client.echoInteger (9876543));
      Text_IO.New_Line;
      Text_IO.New_Line;
   end T_echoInteger;

   ------------------------
   -- T_echoIntegerArray --
   ------------------------

   procedure T_echoIntegerArray is
      Arr : constant ArrayOfint := (34, 67, 98, 54, 78, 65, 1);

      Res : constant ArrayOfint
        := interopLab.Client.echoIntegerArray (Arr);
   begin
      Text_IO.Put_Line ("Echo ArrayOfint");
      for K in Res'Range loop
         Text_IO.Put (K'Img & " = ");
         Integer_Text_IO.Put (Res (K));
         Text_IO.New_Line;
      end loop;
      Text_IO.New_Line;
   end T_echoIntegerArray;

   ------------------
   -- T_echoString --
   ------------------

   procedure T_echoString is
      Res : constant String := interopLab.Client.echoString
        ("This is the real value for the string!");
   begin
      Text_IO.Put_Line ("Echo String");

      Text_IO.Put_Line (Res);
      Text_IO.New_Line;
   end T_echoString;

   -----------------------
   -- T_echoStringArray --
   -----------------------

   procedure T_echoStringArray is
      Arr : constant ArrayOfstring := (+"first", +"second", +"third");

      Res : constant ArrayOfstring
        := interopLab.Client.echoStringArray (Arr);
   begin
      Text_IO.Put_Line ("Echo ArrayOfstring");
      for K in Res'Range loop
         Text_IO.Put_Line (K'Img & " = " & To_String (Res (K)));
      end loop;
      Text_IO.New_Line;
   end T_echoStringArray;

   ------------------
   -- T_echoStruct --
   ------------------

   procedure T_echoStruct is
      Struct : constant SOAPStruct_Type
        := (6, 6.6, +"666");

      Res : constant echoStruct_Result
        := interopLab.Client.echoStruct (Struct);
   begin
      Text_IO.Put_Line ("Echo Struct");
      Output (Res);
      Text_IO.New_Line;
   end T_echoStruct;

   -----------------------
   -- T_echoStructArray --
   -----------------------

   procedure T_echoStructArray is
      A_Struct : constant ArrayOfSOAPStruct
        := ((1, 1.1, +"one"), (2, 2.2, +"two"), (3, 3.3, +"three"));

      Res : constant ArrayOfSOAPStruct
        := interopLab.Client.echoStructArray (A_Struct);
   begin
      Text_IO.Put_Line ("Echo ArrayOfStruct");

      for K in Res'Range loop
         Output (Res (K));
      end loop;

      Text_IO.New_Line;
   end T_echoStructArray;

   ----------------
   -- T_echoVoid --
   ----------------

   procedure T_echoVoid is
   begin
      Text_IO.Put_Line ("Echo Void");
      interopLab.Client.echoVoid;
   end T_echoVoid;

begin
   T_echoVoid;
   T_echoStringArray;
   T_echoInteger;
   T_echoFloat;
   T_echoIntegerArray;
   T_echoStruct;
   T_echoDate;
   T_echoBoolean;
   T_echoString;
   T_echoBase64;
   T_echoStructArray;
end interopLab_Main;
