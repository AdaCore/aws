------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                       Copyright (C) 2011, AdaCore                        --
--                                                                          --
--  This library is free software; you can redistribute it and/or modify    --
--  it under the terms of the GNU General Public License as published by    --
--  the Free Software Foundation; either version 2 of the License, or (at   --
--  your option) any later version.                                         --
--                                                                          --
--  This library is distributed in the hope that it will be useful, but     --
--  WITHOUT ANY WARRANTY; without even the implied warranty of              --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU       --
--  General Public License for more details.                                --
--                                                                          --
--  You should have received a copy of the GNU General Public License       --
--  along with this library; if not, write to the Free Software Foundation, --
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.          --
--                                                                          --
--  As a special exception, if other files instantiate generics from this   --
--  unit, or you link this unit with other files to produce an executable,  --
--  this  unit  does not  by itself cause  the resulting executable to be   --
--  covered by the GNU General Public License. This exception does not      --
--  however invalidate any other reasons why the executable file  might be  --
--  covered by the  GNU Public License.                                     --
------------------------------------------------------------------------------

with Ada.Text_IO;

with AWS.Client;
with AWS.Config.Set;
with AWS.Net;
with AWS.Server;
with AWS.Response;
with AWS.Status;
with AWS.MIME;
with AWS.Utils;

with Get_Free_Port;

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

   WS : Server.HTTP;
   Port : Natural := 2349;
   R    : Response.Data;
   Conf : Config.Object;

begin
   Get_Free_Port (Port);

   Config.Set.Server_Port (Conf, Port);

   AWS.Server.Start (WS, CB'Unrestricted_Access, Conf);

   Text_IO.Put_Line ("started"); Ada.Text_IO.Flush;

   R := Client.Get ("http://localhost:" & Utils.Image (Port));

   AWS.Server.Shutdown (WS);

   Get_Free_Port (Port);

   Config.Set.Send_Buffer_Size (Conf, 128_000);
   Config.Set.Server_Port (Conf, Port);

   AWS.Server.Start (WS, CB'Unrestricted_Access, Conf);

   R := Client.Get ("http://localhost:" & Utils.Image (Port));

   Server.Shutdown (WS);

   if V (2) > V (1) then
      Text_IO.Put_Line ("Ok");
   else
      Text_IO.Put_Line ("NOk");
   end if;

   Text_IO.Put_Line ("shutdown");
end Socket_Send_Buffer;
