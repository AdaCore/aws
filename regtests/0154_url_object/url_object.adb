------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2007-2012, AdaCore                     --
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

with Ada.Exceptions;
with Ada.Text_IO;
with Ada.Strings.Fixed;

with AWS.Client;
with AWS.Parameters;
with AWS.Server.Status;
with AWS.Response;
with AWS.Status;
with AWS.MIME;
with AWS.URL;
with AWS.Utils;

procedure URL_Object is

   use Ada;
   use AWS;

   WS : Server.HTTP;

   function CB (Request : Status.Data) return Response.Data is
      U : constant URL.Object := Status.URI (Request);
      P : constant Parameters.List := Status.Parameters (Request);

      URL_Image  : String := URL.URL (U);
      Port       : constant Positive := Server.Status.Port (WS);
      Port_Image : constant String  := Utils.Image (Port);
      Port_Idx   : constant Natural :=
        Strings.Fixed.Index (URL_Image, ':' & Port_Image & '/');

   begin
      Strings.Fixed.Replace_Slice
        (URL_Image, Port_Idx + 1, Port_Idx + Port_Image'Length, "port");

      Text_IO.Put_Line ("p1=" & Parameters.Get (P, "p1"));
      Text_IO.Put_Line ("p2=" & Parameters.Get (P, "p2"));
      Text_IO.Put_Line ("----------------------");
      Text_IO.Put_Line ("p1=" & Status.Parameter (Request, "p1"));
      Text_IO.Put_Line ("p2=" & Status.Parameter (Request, "p2"));
      Text_IO.Put_Line ("----------------------");
      Text_IO.Put_Line ("URI         = " & Status.URI (Request));
      Text_IO.Put_Line ("URL         = " & URL_Image);
      Text_IO.Put_Line ("Query       = " & URL.Query (U));
      Text_IO.Put_Line ("Path        = " & URL.Path (U));
      Text_IO.Put_Line ("Pathname    = " & URL.Pathname (U));
      Text_IO.Put_Line ("File        = " & URL.File (U));
      Text_IO.Put_Line ("Parameters  = " & URL.Parameters (U));
      Text_IO.Put_Line ("Server_Name = " & URL.Server_Name (U));

      if URL.Port (U) /= Port or else URL.Port (U) /= Port_Image then
         Text_IO.Put_Line ("URL.Port error");
      end if;

      return Response.Build (MIME.Text_HTML, "not used");

   exception
      when E : others =>
         Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));

         return Response.Build (MIME.Text_HTML, "error");
   end CB;

begin
   Server.Start (WS, "url_object", CB'Unrestricted_Access, Port => 0);
   Text_IO.Put_Line ("started"); Ada.Text_IO.Flush;

   declare
      URL : constant String := AWS.Server.Status.Local_URL (WS);
      R   : Response.Data;
   begin
      R := Client.Get (URL & "/get_it?p1=1&p2=toto");
      R := Client.Get (URL & "/get_it/disk.html?p1=0956&p2=uuu");
   end;

   Server.Shutdown (WS);
   Text_IO.Put_Line ("shutdown");
end URL_Object;
