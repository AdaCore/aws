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

with Ada.Text_IO;
with Ada.Command_Line;

with AWS.Server;
with AWS.Response;

with Auth_CB;

--
--  Usage : auth [any|basic|digest]
--

procedure Auth is

   use Ada;

   WS : AWS.Server.HTTP;

begin
   Text_IO.Put_Line ("AWS " & AWS.Version);
   Text_IO.Put_Line ("Kill me when you want me to stop...");

   if Command_Line.Argument_Count = 1 then
      Auth_CB.Auth_Mode := AWS.Response.Authentication_Mode'Value
        (Command_Line.Argument (1));
   end if;

   AWS.Server.Start (WS, "Auth demo",
                     Port           => 1234,
                     Max_Connection => 10,
                     Callback       => Auth_CB.Get'Access);

   AWS.Server.Wait (AWS.Server.Q_Key_Pressed);

exception
   when others =>
      Text_IO.New_Line;
      Text_IO.Put_Line ("Usage : auth [any|basic|digest]");
      Text_IO.New_Line;
end Auth;
