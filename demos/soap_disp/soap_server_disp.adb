------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2003-2012, AdaCore                     --
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

--  A simple SOAP server. Same as SOAP_Server but using SOAP dispatchers

with Ada.Text_IO;

with AWS.Server;
with AWS.Config;

with SOAP.Dispatchers.Callback;

with SOAP_Server_Disp_CB;

procedure SOAP_Server_Disp is

   use Ada;

   WS : AWS.Server.HTTP;

begin
   AWS.Server.Start
     (WS,
      Dispatcher => SOAP.Dispatchers.Callback.Create
                      (SOAP_Server_Disp_CB.CB'Access,
                       SOAP_Server_Disp_CB.SOAP_CB'Access),
      Config     => AWS.Config.Default_Config);

   Text_IO.Put_Line ("SOAP Server - hit a key to exit");

   --  Wait a charcter to exit

   declare
      C : Character;
   begin
      Text_IO.Get_Immediate (C);
   end;

end SOAP_Server_Disp;
