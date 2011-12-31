------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2009-2012, AdaCore                     --
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
with Ada.Integer_Text_IO;

with API_Service.Client;
with API_Service.CB;
with API_Service.Server;

with API_Srv_Main;

procedure API_Main is

   use Ada;

   V : Natural;

begin
   API_Srv_Main.Start;

   --  Std

   API_Service.Client.Empty;
   API_Service.Client.Callme (19);
   API_Service.Client.Callme_Message (24, "this is AWS");

   V := API_Service.Client.Get_Value;
   Integer_Text_IO.Put (V); Text_IO.New_Line;

   V := API_Service.Client.Length ("toto");
   Integer_Text_IO.Put (V); Text_IO.New_Line;

   --  With connection

   API_Service.Client.Empty (API_Service.Client.Connection);
   API_Service.Client.Callme (API_Service.Client.Connection, 19);
   API_Service.Client.Callme_Message
     (API_Service.Client.Connection, 24, "this is AWS");

   V := API_Service.Client.Get_Value (API_Service.Client.Connection);
   Integer_Text_IO.Put (V); Text_IO.New_Line;

   V := API_Service.Client.Length (API_Service.Client.Connection, "toto");
   Integer_Text_IO.Put (V); Text_IO.New_Line;

   API_Srv_Main.Stop;
end API_Main;
