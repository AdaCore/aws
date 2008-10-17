------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                        Copyright (C) 2008, AdaCore                       --
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

with Ada.Strings.Unbounded;
with Ada.Text_IO;

with SOAP.Message.Payload;
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
   Text_IO.Put_Line (To_String (Message.XML_Image (Message.Object (P))));
end Driver;
