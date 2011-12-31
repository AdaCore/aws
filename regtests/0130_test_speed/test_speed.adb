------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2004-2012, AdaCore                     --
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

with Ada.Text_IO;
with Ada.Calendar;
with Ada.Unchecked_Deallocation;

with SOAP.Types;
with SOAP.Parameters;
with SOAP.Message;
with SOAP.Message.Payload;

procedure Test_Speed is

   use Ada.Text_IO;
   use Ada.Calendar;

   use SOAP.Types;
   use SOAP.Parameters;

   N_Test : constant := 1_000;

   Soap_Updated   : constant String := "Updated";
   Soap_Wo        : constant String := "Workorder";
   Soap_WoId      : constant String := "Woid";
   Soap_State     : constant String := "State";
   Soap_Package   : constant String := "Package";
   Soap_Recipe    : constant String := "Recipe";
   Soap_Week      : constant String := "Week";
   Soap_Orderline : constant String := "Orderline";
   Soap_TwelveNc  : constant String := "Twelvenc";
   Soap_Device    : constant String := "Device";
   Soap_Type      : constant String := "Type";
   Soap_Priority  : constant String := "Priority";
   Soap_Label     : constant String := "Label";
   Soap_Quantity  : constant String := "Quantity";
   Soap_Packing   : constant String := "Packing";
   Soap_Marking   : constant String := "Marking";
   Soap_Orient    : constant String := "Orientation";
   Soap_Produced  : constant String := "Produced";
   Soap_TapeSwap  : constant String := "Tapeswap";

   Max_Time       : Duration;
   --  Time allowed to complete the test. This is set below

   --------------------------
   -- Check_Computer_Speed --
   --------------------------

   procedure Check_Computer_Speed is

      Start   : Time;
      Finish  : Time;
      Elapsed : Duration;

      subtype String_80 is String (1 .. 80);

      type A_Str is access String_80;

      procedure Free is new Ada.Unchecked_Deallocation (String_80, A_Str);

      S : A_Str;

      procedure Call (K : Positive) is
         I : Integer := K;
      begin
         I := I + 78;
         I := I / 5678;
         I := I - 34;
         I := I * 89;
      end Call;

   begin
      Start := Clock;

      for K in 1 .. 1_000_000 loop
         Call (K);
      end loop;

      for K in 1 .. 40_000 loop
         S := new String_80;
         Free (S);
      end loop;

      Finish := Clock;

      Elapsed := Finish - Start;

      --  60 found empirically

      Max_Time := Elapsed * 60;
   end Check_Computer_Speed;

   --------
   -- Ol --
   --------

   function Ol
     (Tnc     : String  := "123456781234";
      Device  : String  := "BC847B";
      Quant   : Integer := 3000;
      Pass    : Integer := 1;
      Prio    : Integer := 1;
      Marking : String  := "")
      return SOAP_Record is
   begin
      return R ((+S ("idle", Soap_State),
                 +S ("Wo1", Soap_WoId),
                 +S (Tnc, Soap_TwelveNc),
                 +S (Device, Soap_Device),
                 +I (Pass, Soap_Type),
                 +I (Prio, Soap_Priority),
                 +S ("E6", Soap_Label),
                 +I (Quant, Soap_Quantity),
                 +I (1000, Soap_Packing),
                 +S (Marking, Soap_Marking),
                 +B (True, Soap_TapeSwap),
                 +I (1, Soap_Orient),
                 +I (0, Soap_Produced)), Soap_Orderline);
   end Ol;

   Start   : Time;
   Finish  : Time;
   Elapsed : Duration;

begin
   Check_Computer_Speed;

   Start := Clock;

   for X in 1 .. N_Test loop
      declare
         Payload : SOAP.Message.Payload.Object'Class :=
            SOAP.Message.Payload.Build
               ("Workorder",
                +R ((+S ("Wo1", Soap_WoId),
                     +T (Clock, Soap_Updated),
                     +S ("idle", Soap_State),
                     +S ("sot23", Soap_Package),
                     +S ("bc847", Soap_Recipe),
                     +I (233, Soap_Week),
                     +A ((+Ol ("123456781234", "BC847B", 1E7, 2, 26, ""),
                          +Ol ("123456781234", "BC847B", 2000, 2, 1, "3kl"),
                          +Ol ("123456781234", "BC847C", 1000, 1, 1, "3kl"),
                          +Ol ("123456781234", "BC847B", 2000, 2, 1, "-"),
                          +Ol ("123456781234", "BC847C", 1000, 1, 1, "3kl")),
                         Soap_Orderline)
                    ), Soap_Wo)
               );
         pragma Unreferenced (Payload);
      begin
         null;
      end;
   end loop;

   Finish := Clock;

   Elapsed := Finish - Start;

   if Elapsed < 0.5 then
      --  Computers are certainly faster now, update the check here
      Put_Line ("Really quick now, should update the regtest!");

   elsif  Elapsed < Max_Time then
      Put_Line ("Ok");

   else
      Put_Line
        ("Nok - Allowed "
           & Duration'Image (Max_Time)
           & "; took " & Duration'Image (Elapsed));
   end if;
end Test_Speed;
