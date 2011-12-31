------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2010-2012, AdaCore                     --
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
