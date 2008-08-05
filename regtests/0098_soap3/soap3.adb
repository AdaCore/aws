------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2002-2008, AdaCore                     --
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

   Img1 : constant String := SOAP.Message.XML.Image (Payload1);
   Img2 : constant String := SOAP.Message.XML.Image (Payload2);

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
