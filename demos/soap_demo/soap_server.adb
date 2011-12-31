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

--  A simple SOAP server.

with Ada.Text_IO;

with AWS.Server;

with SOAP_Server_CB;

procedure SOAP_Server is

   use Ada;

   WS : AWS.Server.HTTP;

begin
   AWS.Server.Start (WS, "SOAP demo",
                     Max_Connection => 5,
                     Callback       => SOAP_Server_CB.CB'Access);

   Text_IO.Put_Line ("SOAP Server - hit a key to exit");

   --  Wait a charcter to exit

   declare
      C : Character;
   begin
      Text_IO.Get_Immediate (C);
   end;

end SOAP_Server;
