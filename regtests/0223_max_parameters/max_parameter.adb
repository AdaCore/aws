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

procedure Max_Parameter (Max_Parameters : Positive) is

   use Ada;
   use Ada.Strings.Unbounded;
   use AWS;
   use type AWS.Messages.Status_Code;

   WS  : Server.HTTP;
   CNF : Config.Object;

   CRLF : constant String := ASCII.CR & ASCII.LF;

   M_Body : constant String :=
              "BODY_P1=56&BODY_P2=inthebody&BODY_P3=secure_aws" & CRLF;

   --------
   -- CB --
   --------

   function CB (Request : Status.Data) return Response.Data is
      Params : constant Parameters.List := Status.Parameters (Request);
   begin
      Text_IO.Put_Line ("P1      >" & Parameters.Get (Params, "P1"));
      Text_IO.Put_Line ("P2      >" & Parameters.Get (Params, "P2"));
      Text_IO.Put_Line ("P3      >" & Parameters.Get (Params, "P2"));
      Text_IO.Put_Line ("BODY_P1 >" & Parameters.Get (Params, "BODY_P1"));
      Text_IO.Put_Line ("BODY_P2 >" & Parameters.Get (Params, "BODY_P2"));
      Text_IO.Put_Line ("BODY_P3 >" & Parameters.Get (Params, "BODY_P3"));
      return Response.Build (MIME.Text_HTML, "ok");
   end CB;

   R : Response.Data;

begin
   Config.Set.Server_Name (CNF, "Max Parameter");
   Config.Set.Server_Host (CNF, "localhost");
   Config.Set.Server_Port (CNF, 0);

   Config.Set.Max_POST_Parameters (CNF, Max_Parameters);

   Server.Start (WS, CB'Unrestricted_Access, CNF);

   Text_IO.Put_Line ("started"); Ada.Text_IO.Flush;

   R := Client.Post
          (Server.Status.Local_URL (WS) & "/this_uri?P1=12&P2=azerty&P3=aws",
           M_Body);

   if Response.Status_Code (R) = Messages.S200 then
      Text_IO.Put_Line (Response.Message_Body (R));

   elsif Response.Status_Code (R) = Messages.S403
     and then Index (Response.Message_Body (R), "Maximum number of para") /= 0
   then
      Text_IO.Put_Line ("Got exception TOO_MANY_PARAMETERS");

   else
      Text_IO.Put_Line
        ("Got : " & Messages.Status_Code'Image (Response.Status_Code (R)));
      Text_IO.Put_Line (Response.Message_Body (R));
   end if;

   Server.Shutdown (WS);
   Text_IO.Put_Line ("shutdown");
end Max_Parameter;
