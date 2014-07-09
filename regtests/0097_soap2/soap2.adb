------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2002-2014, AdaCore                     --
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

with Ada.Calendar;     use Ada.Calendar;
with Ada.Text_IO;      use Ada.Text_IO;

with SOAP.Message;
with SOAP.Message.Payload;
with SOAP.Message.XML;
with SOAP.Parameters;  use SOAP.Parameters;
with SOAP.Types;       use SOAP.Types;

procedure SOAP2 is

   Payload : SOAP.Message.Payload.Object'Class :=
     SOAP.Message.Payload.Build
     ("Workorder",
      +R ((+S ("Wo1", "name1"),
           +S ("idle", "name2"),
           +A ((1 => +R ((+S ("idle", "name3"),
                          +S ("Wo1", "name4")),
                         "my_record")),
               "my_array")),
          "my_record_out"));

   Parms    : List := SOAP.Message.Parameters (Payload);
   Obj      : Object'Class := Argument (Parms, "my_record_out");
   Img      : aliased String := SOAP.Message.XML.Image (Payload);
   Payload2 : SOAP.Message.Payload.Object'Class
     := SOAP.Message.XML.Load_Payload (Img);

   Parms2   : List := SOAP.Message.Parameters (Payload2);
   Obj2     : Object'Class := Argument (Parms2, "my_record_out");
   Img2     : String := SOAP.Message.XML.Image (Payload2);

begin
   Put_Line (Img);
   New_Line;

   if Img = Img2 then
      Put_Line ("Payload coding/decoding is ok");
   else
      Put_Line ("Payload coding/decoding failed");
      Put_Line (Img2);
      Put_Line (XML_Image (Obj));
      Put_Line (XML_Image (Obj2));
   end if;
end SOAP2;
