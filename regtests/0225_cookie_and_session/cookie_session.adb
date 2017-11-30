------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2012-2017, AdaCore                     --
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

with Ada.Strings.Fixed;
with Ada.Text_IO;

with AWS.Client;
with AWS.Cookie;
with AWS.Headers;
with AWS.Messages;
with AWS.Response;
with AWS.Server.Status;
with AWS.Session;
with AWS.Status;

procedure Cookie_Session is

   use Ada;
   use AWS;

   function Callback (Request : Status.Data) return Response.Data is
      Res : Response.Data;
      SID : Session.Id := Status.Session (Request);
   begin
      Res := Response.Build
        ("text/plain", "Session " & Session.Image (SID) & ASCII.LF);
      Cookie.Set (Res, "key1", "val1");
      Cookie.Set (Res, "key2", "val2");
      Session.Set (SID, "skey", "sval");
      return Res;
   end Callback;

   WS  : Server.HTTP;
   Res : Response.Data;
   HL  : Headers.List;

begin
   Server.Start
     (WS, "Cookie Session", Callback'Unrestricted_Access, Port => 0,
      Session => True);

   Res := Client.Get (Server.Status.Local_URL (WS));

   HL := Response.Header (Res);

   for K in 1 .. HL.Count loop
      declare
         L : String := Headers.Get_Line (HL, K);
         I : Natural := Strings.Fixed.Index (L, "AWS=SID-");
         O : Positive := 8;
      begin
         if L (L'First .. L'First + Messages.Set_Cookie_Token'Length - 1)
           = Messages.Set_Cookie_Token
         then
            if I = 0 then
               I := Strings.Fixed.Index (L, "AWS_Private=");
               O := 12;
            end if;

            if I /= 0 then
               I := I + O;
               while L (I) /= ';' loop
                  L (I) := 'x';
                  I := I + 1;
               end loop;
            end if;
            Text_IO.Put_Line (L);
         end if;
      end;
   end loop;

   Server.Shutdown (WS);
end Cookie_Session;
