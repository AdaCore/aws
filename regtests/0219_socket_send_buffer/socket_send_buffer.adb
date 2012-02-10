------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2011-2012, AdaCore                     --
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
with AWS.Config.Set;
with AWS.Net;
with AWS.Server.Status;
with AWS.Response;
with AWS.Status;
with AWS.MIME;
with AWS.Utils;

procedure Socket_Send_Buffer is

   use Ada;
   use AWS;

   V : array (1 .. 2) of Natural;
   I : Positive := V'First;

   function CB (Request : Status.Data) return Response.Data is
   begin
      V (I) := Net.Get_Send_Buffer_Size (Status.Socket (Request));
      I := I + 1;
      return Response.Build (MIME.Text_HTML, "Socket_Send_Buffer_Size");
   end CB;

   WS   : Server.HTTP;
   R    : Response.Data;
   Conf : Config.Object;

begin
   Config.Set.Server_Port (Conf, 0);

   AWS.Server.Start (WS, CB'Unrestricted_Access, Conf);

   Text_IO.Put_Line ("started"); Ada.Text_IO.Flush;

   R := Client.Get (Server.Status.Local_URL (WS));

   AWS.Server.Shutdown (WS);

   Config.Set.Send_Buffer_Size (Conf, 128_000);

   AWS.Server.Start (WS, CB'Unrestricted_Access, Conf);

   R := Client.Get (Server.Status.Local_URL (WS));

   Server.Shutdown (WS);

   if V (2) > V (1) then
      Text_IO.Put_Line ("Ok");
   else
      Text_IO.Put_Line ("NOk");
   end if;

   Text_IO.Put_Line ("shutdown");
end Socket_Send_Buffer;
