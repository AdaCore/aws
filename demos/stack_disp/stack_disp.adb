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

with AWS.Config.Set;
with AWS.Dispatchers.Stacks;
with AWS.Server;

with Pages;

procedure Stack_Disp is
   Stack : AWS.Dispatchers.Stacks.Handler;

   Page_1 : Pages.First_Page;
   Page_2 : Pages.Second_Page;

   WS : AWS.Server.HTTP;

   Config : AWS.Config.Object := AWS.Config.Default_Config;

begin

   AWS.Config.Set.Reuse_Address (Config, True);

   Stack.Append_Distpatch_Item (Page_1);
   Stack.Append_Distpatch_Item (Page_2);

   AWS.Server.Start
     (WS,
      Dispatcher => Stack,
      Config     => Config);

   Ada.Text_IO.Put_Line ("Stack dispatcher Server - hit a key to exit");

   --  Wait a charcter to exit

   declare
      C : Character;
   begin
      Ada.Text_IO.Get_Immediate (C);
      Ada.Text_IO.Put_Line (C & "");
   end;

end Stack_Disp;
