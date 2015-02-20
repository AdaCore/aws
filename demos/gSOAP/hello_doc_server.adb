------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                       Copyright (C) 2015, AdaCore                        --
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

--  The famous Hello Word demo, using AWS/SOAP framework.
--
--  SOAP Server

with AWS.Server;

with Hello_DOC.Server;
with Hello_DOC_Server_CB;

procedure Hello_DOC_Server is

   use AWS;

   H_Server : Server.HTTP;

begin
   Server.Start
     (H_Server, "Hello DOC",
      Hello_DOC_Server_CB.CB'Access,
      Port => Hello_DOC.Server.Port);

   Server.Wait (Server.Q_Key_Pressed);
end Hello_DOC_Server;
