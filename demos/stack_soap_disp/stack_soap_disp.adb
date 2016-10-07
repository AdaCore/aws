------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2016, CNRS                         --
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

with Ada.Text_IO;

with AWS.Config;
with AWS.Services.Dispatchers.Stack;
with AWS.Server;

with Pages;

with SOAP_Server_Disp_CB;
with SOAP.Dispatchers.Stack;

procedure Stack_Soap_Disp is
   Stack : AWS.Services.Dispatchers.Stack.Handler;

   Page_1 : Pages.First_Page;
   Page_2 : Pages.Second_Page;

   Soap_Handling : AWS.Services.Dispatchers.Stack.Item_Interface'Class :=
     SOAP.Dispatchers.Stack.Create (SOAP_Server_Disp_CB.SOAP_CB'Access);

   WS : AWS.Server.HTTP;

begin

   Stack.Append (Soap_Handling);
   Stack.Append (Page_1);
   Stack.Append (Page_2);

   AWS.Server.Start
     (WS,
      Dispatcher => Stack,
      Config     => AWS.Config.Default_Config);

   Ada.Text_IO.Put_Line ("Stack dispatcher Server - hit a key to exit");

   --  Wait a charcter to exit

   declare
      C : Character;
   begin
      Ada.Text_IO.Get_Immediate (C);
      Ada.Text_IO.Put_Line (C & "");
   end;

end Stack_Soap_Disp;
