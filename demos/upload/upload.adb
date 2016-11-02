------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2011-2016, AdaCore                     --
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

--  A simple file upload

with Ada.Text_IO;

with AWS.Default;
with AWS.Server;

with Upload_CB;

procedure Upload is

   use Ada;
   use AWS;

   WS : AWS.Server.HTTP;

begin
   Text_IO.Put_Line
     ("Call me on port"
      & Positive'Image (AWS.Default.Server_Port)
      & ", press Q to stop the server...");

   Server.Start
     (WS, "Upload Demo",
      Max_Connection   => 4,
      Callback         => Upload_CB.HW_CB'Access,
      Upload_Directory => ".");

   Text_IO.Put_Line ("Press Q to quit.");

   Server.Wait (Server.Q_Key_Pressed);

   Server.Shutdown (WS);
end Upload;
