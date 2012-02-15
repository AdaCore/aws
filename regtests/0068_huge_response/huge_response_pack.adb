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

with Ada.Strings.Unbounded;
with Ada.Text_IO;

with AWS.Client;
with AWS.MIME;
with AWS.Response.Set;
with AWS.Server.Status;
with AWS.Status;
with AWS.Utils;

package body Huge_Response_Pack is

   use Ada;
   use Ada.Strings.Unbounded;
   use AWS;

   WS : Server.HTTP;

   Len_Message : constant := 20_000_000;
   Sample      : constant String  := "0123456789";
   Message     : Unbounded_String := (Len_Message / Sample'Length) * Sample;

   --------
   -- CB --
   --------

   function CB (Request : Status.Data) return Response.Data is
   begin
      return Response.Build (MIME.Text_HTML, Message);
   end CB;

   R : Response.Data;

   ---------
   -- Run --
   ---------

   procedure Run (Security : Boolean) is
   begin
      Server.Start
        (WS, "huge_message",
         CB'Access,
         Port           => 0,
         Security       => Security,
         Max_Connection => 1);

      Ada.Text_IO.Put_Line ("started"); Ada.Text_IO.Flush;

      R := Client.Get (Server.Status.Local_URL (WS));

      if Message = Unbounded_String'(Response.Message_Body (R)) then
         Text_IO.Put_Line ("Ok");
      else
         Text_IO.Put_Line ("Nok: " & To_String (Response.Message_Body (R)));
      end if;

      Server.Shutdown (WS);
      Ada.Text_IO.Put_Line ("shutdown");
   end Run;

end Huge_Response_Pack;
