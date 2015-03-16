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

with Ada.Text_IO;

with AWS.Client;
with AWS.Server.Status;

with DOC.Client;
with DOC.Server;
with DOC_Server_CB;

procedure DOC_Binding is

   use Ada;
   use AWS;

   H_Server : Server.HTTP;

begin
   Server.Start
     (H_Server, "Doc WSDL", DOC_Server_CB.CB'Access,
      Port => 0);

   Text_IO.Put_Line
     (DOC.Client.SayHello
        (firstName => "AWS", Endpoint => Server.Status.Local_URL (H_Server)));

   AWS.Server.Shutdown (H_Server);
end DOC_Binding;
