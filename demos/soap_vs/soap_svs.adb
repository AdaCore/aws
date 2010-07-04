------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2001                          --
--                                ACT-Europe                                --
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

--  A SOAP server that implements all Web Services on the following validation
--  suite: http://soap.weblogs.com/validator1. This implementation has passed
--  all the tests on:
--
--     Never at this point.

with Ada.Text_IO;
with Ada.Command_Line;

with AWS.Server;

with SOAP_SVS_CB;

procedure SOAP_SVS is

   use Ada;

   WS : AWS.Server.HTTP;

begin
   if Command_Line.Argument_Count = 1
     and then Command_Line.Argument (1) = "-t"
   then
      SOAP_SVS_CB.Quiet := False;
   end if;

   AWS.Server.Start (WS, "SOAP Validation Suite",
                     Max_Connection => 5,
                     Callback       => SOAP_SVS_CB.CB'Access);

   Text_IO.Put_Line ("SOAP SVS - hit a key to exit");

   --  Wait a charcter to exit

   declare
      C : Character;
   begin
      Text_IO.Get_Immediate (C);
   end;

   AWS.Server.Shutdown (WS);
end SOAP_SVS;
