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
with AWS.Config.Set;
with AWS.MIME;
with AWS.Response.Set;
with AWS.Server.Status;
with AWS.Status;
with AWS.Utils;

package body S_Append_Pack is

   use Ada;
   use AWS;

   WS  : Server.HTTP;
   CNF : Config.Object;

   function CB (Request : Status.Data) return Response.Data;

   --------
   -- CB --
   --------

   function CB (Request : Status.Data) return Response.Data is
      Answer : Response.Data;

      procedure Append (Item : String);

      ------------
      -- Append --
      ------------

      procedure Append (Item : String) is
      begin
         if Item'Length > 0 then
            Response.Set.Append_Body (Answer, Item & ASCII.LF);

            Append (Item (Item'First .. Item'Last - 1));

            declare
               Inverse : String (Item'Range);
            begin
               for J in Item'Range loop
                  Inverse (Item'Last - J + 1) := Item (J);
               end loop;

               Response.Set.Append_Body (Answer, Inverse & ASCII.LF);
            end;
         end if;
      end Append;

   begin
      Append
        ("1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
         & "!""#$%&'()*+,-./:;<=>?@[\]^_`{|}~");

      return Answer;
   end CB;

   procedure Run (Protocol : String) is
      R : Response.Data;
   begin
      Config.Set.Server_Name    (CNF, "append message " & Protocol);
      Config.Set.Server_Host    (CNF, "localhost");
      Config.Set.Server_Port    (CNF, 0);
      Config.Set.Security       (CNF, Protocol = "https");
      Config.Set.Max_Connection (CNF, 5);

      Server.Start (WS, CB'Access, CNF);
      Ada.Text_IO.Put_Line ("started");

      R := Client.Get (Server.Status.Local_URL (WS));

      Ada.Text_IO.Put (Response.Message_Body (R));

      Server.Shutdown (WS);

      Ada.Text_IO.Put_Line ("shutdown");
   end Run;

end S_Append_Pack;
