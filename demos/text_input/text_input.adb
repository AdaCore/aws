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

--  Example of text input through an HTTP server.

with Ada.Text_IO;

with AWS.Server;
with AWS.Log;
with AWS.Default;

with Text_Input_CB;

procedure Text_Input is

   WS : AWS.Server.HTTP;

begin
   Ada.Text_IO.Put_Line
     ("I'm on the port"
        & Positive'Image (AWS.Default.Server_Port) & ASCII.LF
        & "press Q key if you want me to stop.");

   AWS.Log.Start
     (Text_Input_CB.Text_Log,
      Split           => AWS.Log.Daily,
      Filename_Prefix => "text_input");

   AWS.Server.Start
     (WS, "Text input",
      Max_Connection => 4,
      Callback       => Text_Input_CB.Text_CB'Access);

   AWS.Server.Wait (AWS.Server.Q_Key_Pressed);

   AWS.Log.Stop (Text_Input_CB.Text_Log);
   AWS.Server.Shutdown (WS);
end Text_Input;
