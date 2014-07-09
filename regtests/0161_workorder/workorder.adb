------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2008-2014, AdaCore                     --
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

with Ada.Text_IO;      use Ada.Text_IO;
with Ada.Calendar;     use Ada.Calendar;

with SOAP.Types;       use SOAP.Types;
with SOAP.Parameters;  use SOAP.Parameters;
with SOAP.Message;
with SOAP.Message.Payload;
with SOAP.Message.XML;

with Tc_Soap_Names;    use Tc_Soap_Names;

procedure Workorder is

   A_Time : constant Time := Time_Of (2002, 3, 24, 56600.5);

   Payload : SOAP.Message.Payload.Object'Class :=
     SOAP.Message.Payload.Build
     ("Workorder",
      +R ((+S ("Wo1", Soap_WoId),
           +T (A_Time, Soap_Updated),
           +S ("idle", Soap_State),
           +S ("sot23", Soap_Package),
           +S ("bc847", Soap_Recipe),
           +A ((+R ((+S ("idle", Soap_State),
                     +S ("Wo1", Soap_WoId),
                     +S ("123456781234", Soap_TwelveNc),
                     +I (2, Soap_Type),
                     +I (1, Soap_Priority),
                     +S ("E6", Soap_Label),
                     +I (18000, Soap_Quantity),
                     +I (3000, Soap_Packing),
                     +S ("3kl", Soap_Marking),
                     +S ("1", Soap_Orient),
                     +I (0, Soap_Produced)
                    ), "Orderline"),
                +R ((+S ("idle", Soap_State),
                     +S ("Wo1", Soap_WoId),
                     +S ("123456781235", Soap_TwelveNc),
                     +I (1, Soap_Type),
                     +I (2, Soap_Priority),
                     +S ("E6", Soap_Label),
                     +I (18000, Soap_Quantity),
                     +I (3000, Soap_Packing),
                     +S ("3kl", Soap_Marking),
                     +S ("1", Soap_Orient),
                     +I (0, Soap_Produced)
                    ),  Soap_Orderline)
               ), Soap_Orderline)
          ), Soap_Wo)
     );

   Parms    : List := SOAP.Message.Parameters (Payload);
   Obj      : Object'Class := Argument (Parms, Soap_Wo);
   Img      : aliased constant String := SOAP.Message.XML.Image (Payload);
   Payload2 : SOAP.Message.Payload.Object'Class
     := SOAP.Message.XML.Load_Payload (Img);

   Parms2   : List := SOAP.Message.Parameters (Payload2);
   Obj2     : Object'Class := Argument (Parms2, Soap_Wo);
   Img2     : String := SOAP.Message.XML.Image (Payload2);

begin
   Put_Line (Img);
   New_Line;

   if Img = Img2 then
      Put_Line ("Payload coding/decoding is ok");
   else
      Put_Line ("Payload coding/decoding failed");
      Put_Line (Img2);
      Put_Line (Soap_Image (Obj));
      Put_Line (Soap_Image (Obj2));
   end if;
end Workorder;
