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

with Ada.Strings.Unbounded;
with Ada.Text_IO;

with AWS.Client;
with AWS.MIME;
with AWS.Response;
with AWS.Server.Status;
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
      Variable     : String;
      Translations : in out Templates.Translate_Set);

   function CB (Request : Status.Data) return Response.Data;

   procedure Check (Connection : in out Client.HTTP_Connection);

   WS     : Server.HTTP;
   Ctx    : aliased Parse_Context;
   C1, C2 : Client.HTTP_Connection;

   -----------
   -- Value --
   -----------

   procedure Value
     (Context      : not null access Parse_Context;
      Variable     : String;
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

   function CB (Request : Status.Data) return Response.Data is
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

   procedure Run is
      R : Response.Data;
   begin
      Server.Start
        (WS, "Line Status", CB'Access, Session => True, Port => 0);

      declare
         URL : constant String := Server.Status.Local_URL (WS);
      begin
         Client.Create (C1, URL);
         Client.Create (C2, URL);
      end;

      Check (C1);
      Check (C1);
      Check (C2);
      Check (C1);
      Check (C2);

      Server.Shutdown (WS);
   end Run;

end Line_Status_Pck;
