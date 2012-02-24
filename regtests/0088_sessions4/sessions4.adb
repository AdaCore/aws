------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2004-2012, AdaCore                     --
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

with Ada.Exceptions;
with Ada.Text_IO;

with AWS.Client;
with AWS.MIME;
with AWS.Response;
with AWS.Server.Status;
with AWS.Session;
with AWS.Status;
with AWS.Utils;

procedure Sessions4 is

   use Ada;
   use AWS;

   WS : Server.HTTP;
   C  : Client.HTTP_Connection;
   R  : Response.Data;

   type Context is record
      N : Natural := 0;
      S : String (1 .. 10);
      B : Boolean;
      C : Character;
   end record;

   Null_Context : constant Context := (0, "          ", False, ASCII.Nul);

   package Context_Session is new Session.Generic_Data (Context, Null_Context);

   function Image (C : Context) return String;
   --  Returns Image for a context object

   --------
   -- CB --
   --------

   function CB (Request : Status.Data) return Response.Data is
      SID : constant Session.ID := Status.Session (Request);
      Ctx : Context := (0, "aaaaaaaaaa", False, 'z');
   begin
      if Session.Exist (SID, "context") then
         Ctx := Context_Session.Get (SID, "context");
         Ctx.N := Ctx.N + 1;
         Ctx.B := not Ctx.B;
         Ctx.C := Character'Pred (Ctx.C);

         for K in Ctx.S'Range loop
            Ctx.S (K) := Character'Succ (Ctx.S (K));
         end loop;
      end if;

      Context_Session.Set (SID, "context", Ctx);

      declare
         R : Context := Context_Session.Get (SID, "toto");
      begin
         if R = Null_Context then
            Text_IO.Put_Line ("Ok, Null_Context");
         end if;
      end;

      return Response.Build
        (MIME.Text_HTML, "Ok, this is call " & Image (Ctx));
   end CB;

   -----------
   -- Image --
   -----------

   function Image (C : Context) return String is
   begin
      return Natural'Image (C.N) & ", "
        & C.S & ", "
        & Boolean'Image (C.B) & ", "
        & C.C;
   end Image;

begin
   Server.Start
     (WS, "session", CB'Unrestricted_Access,
      Port           => 0,
      Max_Connection => 5,
      Session        => True);

   Text_IO.Put_Line ("started");

   Client.Create (C, AWS.Server.Status.Local_URL (WS));

   Client.Get (C, R, "/");
   Text_IO.Put_Line ("Response : " & Response.Message_Body (R));

   Client.Get (C, R, "/");
   Text_IO.Put_Line ("Response : " & Response.Message_Body (R));

   Client.Get (C, R, "/");
   Text_IO.Put_Line ("Response : " & Response.Message_Body (R));

   Client.Get (C, R, "/");
   Text_IO.Put_Line ("Response : " & Response.Message_Body (R));

   Client.Get (C, R, "/");
   Text_IO.Put_Line ("Response : " & Response.Message_Body (R));

   Text_IO.Put_Line ("Ready to stop");

   Server.Shutdown (WS);
   Text_IO.Put_Line ("shutdown");

   Client.Close (C);

exception
   when E : others =>
      Text_IO.Put_Line (Exceptions.Exception_Information (E));
      Server.Shutdown (WS);
end Sessions4;
