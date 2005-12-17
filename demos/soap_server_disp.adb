------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                            Copyright (C) 2003                            --
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
