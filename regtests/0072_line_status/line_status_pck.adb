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

with Ada.Strings.Unbounded;
with Ada.Text_IO;

with AWS.Client;
with AWS.MIME;
with AWS.Response;
with AWS.Server;
with AWS.Session;
with AWS.Status;
with AWS.Templates;
with AWS.Utils;

package body Line_Status_Pck is

   use Ada;
   use Ada.Strings.Unbounded;
   use AWS;

   type Parse_Context is new Templates.Dynamic.Lazy_Tag with null record;

   procedure Value
     (Context      : not null access Parse_Context;
      Variable     : in String;
      Translations : in out Templates.Translate_Set);

   function CB (Request : in Status.Data) return Response.Data;

   procedure Check (Connection : in out Client.HTTP_Connection);

   WS     : Server.HTTP;
   Port   : Natural;
   Ctx    : aliased Parse_Context;
   C1, C2 : Client.HTTP_Connection;

   -----------
   -- Value --
   -----------

   procedure Value
     (Context      : not null access Parse_Context;
      Variable     : in String;
      Translations : in out Templates.Translate_Set)
   is
      Request : constant Status.Data := Server.Get_Status;
      SID     : constant Session.Id  := Status.Session (Request);
      Index   : Natural := 0;
   begin
      Text_IO.Put_Line ("Variable " & Variable);

      --  Check session

      if Session.Exist (SID, "index") then
         Index := Session.Get (SID, "index");
      end if;

      Index := Index + 1;
      Session.Set (SID, "index", Index);

      Templates.Insert
        (Translations,
         Templates.Assoc ("UNKNOWN_VAR", "Index " & Positive'Image (Index)));
   end Value;

   --------
   -- CB --
   --------

   function CB (Request : in Status.Data) return Response.Data is
      SID : constant Session.Id  := Status.Session (Request);
   begin
      return Response.Build
        (MIME.Text_HTML,
         String'(Templates.Parse
                   ("line_status.tmplt", Lazy_Tag => Ctx'Access)));
   end CB;

   -----------
   -- Check --
   -----------

   procedure Check (Connection : in out Client.HTTP_Connection) is
      R : Response.Data;
   begin
      Client.Get (Connection, R, "/toto");
      Text_IO.Put_Line ("> " & Response.Message_Body (R));
   end Check;

   ---------
   -- Run --
   ---------

   procedure Run (Port : in Natural) is
      R : Response.Data;
   begin
      Line_Status_Pck.Port := Port;

      Server.Start
        (WS, "Line Status", CB'Access, Session => True, Port => Port);

      Client.Create (C1, "http://localhost:" & Utils.Image (Port));
      Client.Create (C2, "http://localhost:" & Utils.Image (Port));

      Check (C1);
      Check (C1);
      Check (C2);
      Check (C1);
      Check (C2);

      Server.Shutdown (WS);
   end Run;

end Line_Status_Pck;
