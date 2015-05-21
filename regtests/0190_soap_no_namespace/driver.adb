------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2008-2015, AdaCore                     --
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

with SOAP.Message.Payload;
with SOAP.Message.XML;
with SOAP.Parameters;
with SOAP.Name_Space;
with SOAP.Types;

procedure Driver is
   use Ada;
   use Ada.Strings.Unbounded;
   use SOAP;
   use SOAP.Types;
   use type SOAP.Parameters.List;

   NS    : constant SOAP.Name_Space.Object := SOAP.Name_Space.Create ("", "x");
   P_Set : constant Parameters.List := +S ("simple_string");
   P     : Message.Payload.Object;
begin
   P := Message.Payload.Build ("whatever", P_Set, Name_Space => NS);
   Text_IO.Put_Line (To_String (Message.XML.Image (P)));
end Driver;
