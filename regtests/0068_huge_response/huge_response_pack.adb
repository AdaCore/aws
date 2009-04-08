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

with Ada.Strings.Unbounded;
with Ada.Text_IO;

with AWS.Client;
with AWS.MIME;
with AWS.Response.Set;
with AWS.Server;
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

   procedure Run (Port : Positive; Security : Boolean) is
   begin
      Server.Start
        (WS, "huge_message",
         CB'Access,
         Port           => Port,
         Security       => Security,
         Max_Connection => 1);

      Ada.Text_IO.Put_Line ("started"); Ada.Text_IO.Flush;

      if Security then
         R := Client.Get ("https://localhost:" & AWS.Utils.Image (Port));
      else
         R := Client.Get ("http://localhost:" & AWS.Utils.Image (Port));
      end if;

      if Message = Unbounded_String'(Response.Message_Body (R)) then
         Text_IO.Put_Line ("Ok");
      else
         Text_IO.Put_Line ("Nok: " & To_String (Response.Message_Body (R)));
      end if;

      Server.Shutdown (WS);
      Ada.Text_IO.Put_Line ("shutdown");
   end Run;

end Huge_Response_Pack;
