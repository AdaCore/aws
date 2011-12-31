------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2012, AdaCore                     --
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
