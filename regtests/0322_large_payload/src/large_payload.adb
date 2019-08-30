------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                       Copyright (C) 2019, AdaCore                        --
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

with AWS.Client;
with SOAP.Client;
with SOAP.Message.Payload;
with SOAP.Message.Response;
with SOAP.Parameters;
with SOAP.Types;

procedure Large_Payload is
   use Ada;
   use Ada.Strings.Unbounded;

   use AWS.Client;
   use SOAP.Client;
   use SOAP.Message.Payload;
   use SOAP.Parameters;
   use SOAP.Types;

begin
   declare
      Connection : constant HTTP_Connection :=
                     Create ("http://localhost:3456/");
      Large_Data : constant Unbounded_String :=
                     To_Unbounded_String (3_000_000);
      R          : constant SOAP.Message.Response.Object'Class :=
                     Call (Connection, "Some_Action",
                           Build ("Some_Proc",
                                  +S (Large_Data, "Large_Data")
                                  & S ("", "Dummy")));
      Par_Out    : constant List := SOAP.Message.Parameters (R);
   begin
      null;
   end;

   Text_IO.Put_Line ("OK");

exception
   when others =>
      Text_IO.Put_Line ("NOK");
end Large_Payload;
