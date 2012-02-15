------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2003-2012, AdaCore                     --
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
with AWS.Messages;
with AWS.Response.Set;
with AWS.Server.Status;
with AWS.Status;
with AWS.Utils;

procedure ZApp is

   use Ada;
   use AWS;

   WS : Server.HTTP;

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
      Response.Set.Data_Encoding (Answer, Messages.Deflate);

      Append
        ("1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
           & "!""#$%&'()*+,-./:;<=>?@[\]^_`{|}~");

      return Answer;
   end CB;

   R : Response.Data;

begin
   Server.Start
     (WS, "append message",
      CB'Unrestricted_Access,
      Port           => 0,
      Max_Connection => 5);

   Ada.Text_IO.Put_Line ("started");

   R := Client.Get (Server.Status.Local_URL (WS));

   Ada.Text_IO.Put (Response.Message_Body (R));

   Server.Shutdown (WS);

   Ada.Text_IO.Put_Line ("shutdown");
end ZApp;
