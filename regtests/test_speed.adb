
--  $Id$

with Ada.Text_IO;      use Ada.Text_IO;
with Ada.Calendar;     use Ada.Calendar;
with SOAP.Types;       use SOAP.Types;
with SOAP.Parameters;  use SOAP.Parameters;
with SOAP.Message;
with SOAP.Message.Payload;

procedure Test_Speed is

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
   Start := Clock;

   for X in 1 .. 1_000 loop
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

   if Elapsed < 1.0 then
      --  Computers are certainly faster now, update the check here.
      Put_Line ("Really quick now, should update the regtest!");

   elsif  Elapsed < 5.0 then
      Put_Line ("Ok");

   else
      Put_Line ("Nok - took " & Duration'Image (Elapsed));
   end if;
end Test_Speed;
