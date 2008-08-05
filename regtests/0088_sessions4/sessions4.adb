------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2004-2008, AdaCore                     --
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

with Ada.Exceptions;
with Ada.Text_IO;

with AWS.Client;
with AWS.MIME;
with AWS.Response;
with AWS.Server;
with AWS.Session;
with AWS.Status;
with AWS.Utils;

with Get_Free_Port;

procedure Sessions4 is

   use Ada;
   use AWS;

   WS   : Server.HTTP;
   Port : Natural := 1274;

   C  : Client.HTTP_Connection;
   R  : Response.Data;

   type Context is record
      N : Natural := 0;
      S : String (1 .. 10);
      B : Boolean;
      C : Character;
   end record;

   Null_Context : constant Context := (0, "          ", False, ASCII.Nul);

   task Server is
      entry Started;
      entry Stop;
   end Server;

   package Context_Session is new Session.Generic_Data (Context, Null_Context);

   function Image (C : in Context) return String;
   --  Returns Image for a context object

   --------
   -- CB --
   --------

   function CB (Request : in Status.Data) return Response.Data is
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

   function Image (C : in Context) return String is
   begin
      return Natural'Image (C.N) & ", "
        & C.S & ", "
        & Boolean'Image (C.B) & ", "
        & C.C;
   end Image;

   ------------
   -- Server --
   ------------

   task body Server is
   begin
      Get_Free_Port (Port);

      AWS.Server.Start
        (WS, "session",
         CB'Unrestricted_Access,
         Port           => Port,
         Max_Connection => 5,
         Session        => True);

      Ada.Text_IO.Put_Line ("started");

      accept Started;

      accept Stop;

      Ada.Text_IO.Put_Line ("Ready to stop");
   exception
      when E : others =>
         Text_IO.Put_Line (Exceptions.Exception_Information (E));
   end Server;

begin
   Server.Started;

   Client.Create (C, "http://localhost:" & Utils.Image (Port));

   Client.Get (C, R, "/");
   Ada.Text_IO.Put_Line ("Response : " & Response.Message_Body (R));

   Client.Get (C, R, "/");
   Ada.Text_IO.Put_Line ("Response : " & Response.Message_Body (R));

   Client.Get (C, R, "/");
   Ada.Text_IO.Put_Line ("Response : " & Response.Message_Body (R));

   Client.Get (C, R, "/");
   Ada.Text_IO.Put_Line ("Response : " & Response.Message_Body (R));

   Client.Get (C, R, "/");
   Ada.Text_IO.Put_Line ("Response : " & Response.Message_Body (R));

   Server.Stop;

   AWS.Server.Shutdown (WS);
   Ada.Text_IO.Put_Line ("shutdown");

   Client.Close (C);

exception
   when E : others =>
      Text_IO.Put_Line (Exceptions.Exception_Information (E));
end Sessions4;
