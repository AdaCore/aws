------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                       Copyright (C) 2010, AdaCore                        --
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
with AWS.Containers.Tables;
with AWS.Cookie;
with AWS.Default;
with AWS.Headers;
with AWS.Messages;
with AWS.MIME;
with AWS.Response;
with AWS.Server;
with AWS.Status;
with AWS.Utils;

with Get_Free_Port;

procedure Cookies is

   use Ada;
   use Ada.Strings.Unbounded;
   use AWS;

   WS   : Server.HTTP;
   Port : Positive := 8379;

   --------
   -- CB --
   --------

   function CB (Request : Status.Data) return Response.Data is
      R : Response.Data :=
	    Response.Build (MIME.Text_HTML, "response with cookies");
   begin
      Cookie.Set (R, "ckey", "cval");
      Cookie.Set (R, "date", 9, Max_Age => Default.One_Hour);
      Cookie.Set (R, "z", 76.3, Path => "/justhere");
      return R;
   end CB;

   -----------------
   -- Dump_Header --
   -----------------

   procedure Dump_Header (Name, Value : String) is
   begin
      if Name = Messages.Set_Cookie_Token then
	 Text_IO.Put_Line (Name & " " & Value);
      end if;
   end Dump_Header;

   R : Response.Data;
   H : Headers.List;

begin
   Get_Free_Port (Port);

   Server.Start
     (WS, "Client Headers", CB'Unrestricted_Access, Port => Port);

   R := AWS.Client.Get
     (URL => "http://localhost:" & Utils.Image (Port) & "/whatever");
   H := Response.Header (R);

   declare
      Values : constant Containers.Tables.VString_Array :=
	         Headers.Get_Values (H, Messages.Set_Cookie_Token);
   begin
      for K in Values'Range loop
	 Text_IO.Put_Line (To_String (Values (K)));
      end loop;
   end;

   Server.Shutdown (WS);
   Text_IO.Put_Line ("shutdown");
end Cookies;
