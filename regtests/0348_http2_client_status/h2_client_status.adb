------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                       Copyright (C) 2021, AdaCore                        --
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

with Ada.Streams;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with AWS.Attachments;
with AWS.Client;
with AWS.Config.Set;
with AWS.Headers;
with AWS.Messages;
with AWS.MIME;
with AWS.Net.Log;
with AWS.Parameters;
with AWS.Response;
with AWS.Server.Log;
with AWS.Server.Status;
with AWS.Status;
with AWS.Translator;
with AWS.Utils;

procedure H2_Client_Status is

   use Ada;
   use Ada.Streams;
   use Ada.Strings.Unbounded;
   use AWS;

   --------
   -- CB --
   --------

   function CB (Request : Status.Data) return Response.Data is
      URI : constant String := AWS.Status.URI (Request);
      Mth : constant String := AWS.Status.Method (Request);
      Msg : constant String := URI & " - " & Mth;
   begin
      if URI = "/c1" then
        return Response.Build (MIME.Text_HTML, Msg & " - OK");
      elsif URI = "/c2" then
        return Response.Build (MIME.Text_HTML, Msg & " - OK", Messages.S300);
      else
        return Response.Acknowledge (Messages.S404, Msg & " - not there!");
      end if;
   end CB;

   ------------
   -- Output --
   ------------

   procedure Output (Msg : String; R : Response.Data) is
   begin
     Text_IO.Put_Line (Msg & " : " & Response.Message_Body (R));
     Text_IO.Put_Line ("     status: " & AWS.Response.Status_Code (R)'Img);
     Text_IO.New_Line;
   end Output;

   CNF : Config.Object;
   WS  : Server.HTTP;
   WC  : Client.HTTP_Connection;
   R   : Response.Data;

begin
   Config.Set.Server_Name (CNF, "H2 Client Data");
   Config.Set.Server_Port (CNF, 0);
   Config.Set.Max_Connection (CNF, 5);
   Config.Set.Upload_Directory (CNF, ".");
   Config.Set.HTTP2_Activated (CNF, True);

   Server.Start (WS, CB'Unrestricted_Access, CNF);

   Server.Log.Start_Error (WS);

   Text_IO.Put_Line ("started");
   Text_IO.Flush;
   Text_IO.New_Line;

   Client.Create (WC, Server.Status.Local_URL (WS), HTTP_Version => HTTPv2);

   AWS.Client.Post (WC, R, URI => "/c1", Data => "don't care");
   Output ("R1", R);

   AWS.Client.Get (WC, R, URI => "/c1");
   Output ("R2", R);

   AWS.Client.Post (WC, R, URI => "/c2", Data => "whatever");
   Output ("R3", R);

   AWS.Client.Post (WC, R, URI => "/x", Data => AWS.Client.No_Data);
   Output ("R4", R);

   AWS.Client.Get (WC, R, URI => "/x");
   Output ("R5", R);

   Client.Close (WC);
   Server.Shutdown (WS);
   Text_IO.Put_Line ("shutdown");
end H2_Client_Status;
