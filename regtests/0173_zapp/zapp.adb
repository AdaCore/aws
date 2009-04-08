------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2003-2009, AdaCore                     --
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
with AWS.Messages;
with AWS.Response.Set;
with AWS.Server;
with AWS.Status;
with AWS.Utils;

with Get_Free_Port;

procedure ZApp is

   use Ada;
   use AWS;

   WS : Server.HTTP;

   Free_Port : Positive := 8787;

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
   Get_Free_Port (Free_Port);

   Server.Start
     (WS, "append message",
      CB'Unrestricted_Access,
      Port           => Free_Port,
      Max_Connection => 5);

   Ada.Text_IO.Put_Line ("started");

   R := Client.Get ("http://localhost:" & AWS.Utils.Image (Free_Port));

   Ada.Text_IO.Put (Response.Message_Body (R));

   Server.Shutdown (WS);

   Ada.Text_IO.Put_Line ("shutdown");
end ZApp;
