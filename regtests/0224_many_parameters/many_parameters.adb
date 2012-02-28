------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                        Copyright (C) 2012, AdaCore                       --
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
with AWS.Messages;
with AWS.MIME;
with AWS.Parameters;
with AWS.Response;
with AWS.Server.Status;
with AWS.Status;
with AWS.Utils;

procedure Many_Parameters is

   use Ada;
   use Ada.Strings.Unbounded;
   use AWS;
   use type AWS.Messages.Status_Code;

   WS  : Server.HTTP;
   CNF : Config.Object;

   CRLF : constant String := ASCII.CR & ASCII.LF;

   Max_Parameters : constant := 2000;
   Value_Prefix : constant String := "value";

   M_Body : Unbounded_String := To_Unbounded_String ("name=" & Value_Prefix);

   --------
   -- CB --
   --------

   function CB (Request : Status.Data) return Response.Data is
      Params : constant Parameters.List := Status.Parameters (Request);
   begin
      if Params.Get ("name") /= Value_Prefix then
         Text_IO.Put_Line
           ("Error name = " & Params.Get ("name") & " /= " & Value_Prefix);
      end if;

      for J in 1 .. Max_Parameters - 1 loop
         if Params.Get ("name" & Utils.Image (J))
            /= Value_Prefix & Utils.Image (J)
         then
            Text_IO.Put_Line
              ("Error name" & Utils.Image (J)
               & " = " & Params.Get ("name" & Utils.Image (J))
               & " /= " & Value_Prefix & Utils.Image (J));
         end if;
      end loop;

      return Response.Build (MIME.Text_HTML, "ok" & Params.Count'Img);
   end CB;

   R : Response.Data;

begin
   Config.Set.Server_Name (CNF, "Max Parameter");
   Config.Set.Server_Host (CNF, "localhost");
   Config.Set.Server_Port (CNF, 0);

   Config.Set.Max_POST_Parameters (CNF, Max_Parameters);

   Server.Start (WS, CB'Unrestricted_Access, CNF);

   Text_IO.Put_Line ("started"); Ada.Text_IO.Flush;

   for J in 1 .. Max_Parameters - 1 loop
      Append
        (M_Body,
         "&name" & Utils.Image (J) & '=' & Value_Prefix & Utils.Image (J));
   end loop;

   R := Client.Post
          (Server.Status.Local_URL (WS) & "/this_uri?P1=12&P2=azerty&P3=aws",
           To_String (M_Body));

   Text_IO.Put_Line (Response.Message_Body (R) & Length (M_Body)'Img);

   Append (M_Body, "&odd=value");

   R := Client.Post
          (Server.Status.Local_URL (WS) & "/those_uri",
           To_String (M_Body));

   Text_IO.Put_Line (Response.Message_Body (R));

   Server.Shutdown (WS);
   Text_IO.Put_Line ("shutdown");
end Many_Parameters;
