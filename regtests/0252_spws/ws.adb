------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                    Copyright (C) 2014-2019, AdaCore                      --
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

--  Example of Server Push with WebSocket protocol usage.

with Ada.Exceptions;
with Ada.Text_IO;

with AWS.Config.Set;
with AWS.Net.Log;
with AWS.Server.Status;

with WS_CB;

procedure WS is
   use Ada;
   Config : AWS.Config.Object;

   procedure On_Error (Socket : AWS.Net.Socket_Type'Class; Message : String);

   --------------
   -- On_Error --
   --------------

   procedure On_Error (Socket : AWS.Net.Socket_Type'Class; Message : String) is
   begin
      Text_IO.Put_Line ("# " & Message);
   end On_Error;

begin
   AWS.Config.Set.Reuse_Address (Config, True);
   AWS.Config.Set.Server_Host (Config, "127.0.0.1");
   AWS.Config.Set.Server_Port (Config, 0);
   AWS.Config.Set.Server_Name (Config, "Test");
   AWS.Config.Set.Max_Connection (Config, 5);

   AWS.Net.Log.Start (null, Error => On_Error'Unrestricted_Access);

   AWS.Server.Start
     (WS_CB.WS,
      Config   => Config,
      Callback => WS_CB.Get'Access);

   Text_IO.Put_Line
     ("PORT: " & Positive'Image (AWS.Server.Status.Port (WS_CB.WS)));

   WS_CB.WebSock_Start;

   Text_IO.Put_Line ("Server started, wait connection");
   WS_CB.Wait.Ready;

   delay 0.5;

   for J in 1 .. 5 loop
      Text_IO.Put_Line ("Send " & J'Img);
      WS_CB.Server_Push_Send (J);
   end loop;

   WS_CB.Wait.Close;

   AWS.Server.Shutdown (WS_CB.WS);

exception
   when E : others =>
      Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));

      WS_CB.Wait.Close;
      AWS.Server.Shutdown (WS_CB.WS);
end WS;
