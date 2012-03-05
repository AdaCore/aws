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

   type Array_Of_Natural is array (1 .. 3) of Natural;

   C : constant Array_Of_Natural := (32_000, 64_000, 128_000);
   V : Array_Of_Natural;
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
   Div2 : Positive;

begin
   declare
      S1, S2 : Net.Socket_Type'Class := Net.Socket (False);
      BS : constant := 111001;
      BR : Integer;
   begin
      S1.Socket_Pair (S2);

      S1.Set_Send_Buffer_Size (BS);
      BR := S1.Get_Send_Buffer_Size;

      if BS = BR then
         --  Simmetric setsockopt and getsockopt on SO_SNDBUF

         Div2 := 1;

      elsif 2 * BS = BR then
         --  Linux bug, setsockopt and getsockopt on SO_SNDBUF have
         --  difference on multiplier 2

         Div2 := 2;

      else
         Text_IO.Put_Line ("sockopt on SO_SNDBUF" & BS'Img & BR'Img);
      end if;
   end;

   Config.Set.Server_Port (Conf, 0);

   Text_IO.Put_Line ("started"); Ada.Text_IO.Flush;

   for J in C'Range loop
      Config.Set.Send_Buffer_Size (Conf, C (J) / Div2);

      AWS.Server.Start (WS, CB'Unrestricted_Access, Conf);

      R := Client.Get (Server.Status.Local_URL (WS));

      AWS.Server.Shutdown (WS);
   end loop;

   if V = C then
      Text_IO.Put_Line ("Ok");
   else
      Text_IO.Put ("NOk");

      for J in V'Range loop
         Text_IO.Put (V (J)'Img);
      end loop;

      Text_IO.New_Line;
   end if;

   Text_IO.Put_Line ("shutdown");
end Socket_Send_Buffer;
