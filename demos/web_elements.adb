------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                            Copyright (C) 2004                            --
--                                ACT-Europe                                --
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

with AWS.Config.Set;
with AWS.Default;
with AWS.Dispatchers.Callback;
with AWS.MIME;
with AWS.OS_Lib;
with AWS.Server;
with AWS.Services.Dispatchers.URI;
with AWS.Status;
with AWS.Response;
with AWS.Templates;
with AWS.Utils;

procedure Web_Elements is

   use Ada;
   use AWS;

   WWW_Root : constant String := "../web_elements";

   --------
   -- CB --
   --------

   function CB (Request : in Status.Data) return Response.Data is
      URI      : constant String := Status.URI (Request);
      Filename : constant String := URI (URI'First + 1 .. URI'Last);
   begin
      if URI = "/" then
         return Response.Build
           ("text/html",
            Message_Body => Templates.Parse ("we_library.html"));

      elsif OS_Lib.Is_Regular_File (WWW_Root & URI) then
         return AWS.Response.File
           (MIME.Content_Type (Filename),
            Filename => WWW_Root & URI);

      else
         return AWS.Response.Build
           ("text/html", Message_Body => Templates.Parse (Filename));
      end if;
   end CB;

   --------------
   -- CB_Icons --
   --------------

   function CB_Icons (Request : in Status.Data) return Response.Data is
      URI      : constant String := Status.URI (Request);
      Filename : constant String := URI (URI'First + 1 .. URI'Last);
   begin
      return AWS.Response.File
        (MIME.Content_Type (Filename),
         Filename => WWW_Root & "/icons/" & URI (10 .. URI'Last));
   end CB_Icons;

   Port   : constant Integer := 2400;

   WS   : Server.HTTP;
   Conf : Config.Object := Config.Get_Current;
   Disp : Services.Dispatchers.URI.Handler;

begin
   Text_IO.Put_Line
     ("Connect to http://localhost:" & Utils.Image (Port) & "/");

   Config.Set.Server_Port (Conf, Port);
   Config.Set.Max_Connection (Conf, 1);
   Config.Set.WWW_Root (Conf, WWW_Root);

   Services.Dispatchers.URI.Register
     (Disp, "/we_icons/", CB_Icons'Unrestricted_Access, Prefix => True);
   Services.Dispatchers.URI.Register_Default_Callback
     (Disp, Dispatchers.Callback.Create (CB'Unrestricted_Access));

   Server.Start
     (WS,
      Config     => Conf,
      Dispatcher => Disp);

   Server.Wait (AWS.Server.Q_Key_Pressed);

   Server.Shutdown (WS);
end Web_Elements;
