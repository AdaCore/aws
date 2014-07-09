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

with Ada.Text_IO;      use Ada.Text_IO;
with Ada.Calendar;     use Ada.Calendar;

with SOAP.Types;       use SOAP.Types;
with SOAP.Parameters;  use SOAP.Parameters;
with SOAP.Name_Space;
with SOAP.Message;
with SOAP.Message.Payload;
with SOAP.Message.XML;

procedure SOAP3 is

   CRLF : constant String := ASCII.CR & ASCII.LF;

   Payload1 : constant SOAP.Message.Payload.Object'Class :=
     SOAP.Message.Payload.Build
     ("Workorder",
      +B64 ("", "file"),
      SOAP.Name_Space.Create ("tns", "http://dummyns.org"));

   Payload2 : constant SOAP.Message.Payload.Object'Class :=
     SOAP.Message.Payload.Build
     ("Workorder",
      +S ("", "string"));

   Img1 : aliased constant String := SOAP.Message.XML.Image (Payload1);
   Img2 : aliased constant String := SOAP.Message.XML.Image (Payload2);

   B_Payload1 : constant SOAP.Message.Payload.Object'Class
     := SOAP.Message.XML.Load_Payload (Img1);

   B_Payload2 : constant SOAP.Message.Payload.Object'Class
     := SOAP.Message.XML.Load_Payload (Img2);

begin
   Put_Line ("Ok, payloads parsed");
   New_Line;
   Put_Line (Img1);
   Put_Line (Img2);
end SOAP3;
