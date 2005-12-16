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
with AWS.Parameters;
with AWS.Response;
with AWS.Server;
with AWS.Services.Callbacks;
with AWS.Services.Dispatchers.URI;
with AWS.Status;
with AWS.Templates;
with AWS.Utils;

with Web_Elements_Containers;

procedure Web_Elements is

   use Ada;
   use AWS;
   use Web_Elements_Containers;

   WWW_Root : constant String := "../web_elements";

   C        : Natural := 0;

   --------
   -- CB --
   --------

   function CB (Request : in Status.Data) return Response.Data is
      URI      : constant String := Status.URI (Request);
      Filename : constant String := URI (URI'First + 1 .. URI'Last);
      P_List   : constant Parameters.List := Status.Parameters (Request);
   begin
      if URI = "/" then
         return Response.Build
           (MIME.Text_HTML,
            Message_Body => Templates.Parse ("we_library.html"));

      elsif URI = "/clickme" then
         C := C + 1;
         return Response.Build
           (MIME.Text_HTML,
            Message_Body => Natural'Image (C) &
               ". response for the click me button!");

      elsif URI = "/select_color" then
         declare
            Color : constant String := Parameters.Get (P_List, "select_color");
         begin
            return Response.Build
              (MIME.Text_HTML,
               "<p>You have chosen the " &
               "<span class=""" & Color & """><b>" & Color & "</b></span> " &
               "color</p>");
         end;

      elsif URI = "/data" then
         C := C + 1;
         return Response.Build
           (MIME.Text_HTML,
            Message_Body => Natural'Image (C) &
              ". some data from the server, and the field value '" &
              Parameters.Get (P_List, "field") & ''');

      elsif URI = "/small_form" then
         declare
            Color : constant String := Parameters.Get (P_List, "form_color");
            Size  : constant String := Parameters.Get (P_List, "size");
         begin
            return Response.Build
              (MIME.Text_HTML,
               Message_Body =>
                 "You have ordered a " &
               "<span class=""" & Size & """><b>" & Size & "</b></span> " &
               "<span class=""" & Color & """><b>" & Color & "</b></span> " &
               "pant.");
         end;

      elsif URI = "/db_sel1" then
         return Response.Build
           (MIME.Text_HTML,
            String'(Templates.Parse ("we_ajax_group.html")));

      elsif URI = "/db_sel2" then
         return Response.Build
           (MIME.Text_HTML,
            String'(Templates.Parse
              ("we_ajax_user.html",
                 (1 => Templates.Assoc ("GROUP_V", Get_Groups)))));

      elsif URI = "/g_action" then
         Add_Group (Parameters.Get (P_List, "g_name"));
         return Response.Build
           (MIME.Text_HTML,
            String'(Templates.Parse ("we_ajax_group.html")));

      elsif URI = "/u_action" then
         Add_User (Parameters.Get (P_List, "u_name"));
         return Response.Build
           (MIME.Text_HTML,
            String'(Templates.Parse
              ("we_ajax_user.html",
                 (1 => Templates.Assoc ("GROUP_V", Get_Groups)))));

      elsif URI = "/u_list" then
         return Response.Build
           (MIME.Text_HTML,
            String'(Templates.Parse
              ("we_ajax_user.html",
                 (1 => Templates.Assoc ("LIST", True),
                  2 => Templates.Assoc ("USER_V", Get_Users)))));

      elsif URI = "/g_list" then
         return Response.Build
           (MIME.Text_HTML,
            String'(Templates.Parse
              ("we_ajax_group.html",
                 (1 => Templates.Assoc ("LIST", True),
                  2 => Templates.Assoc ("GROUP_V", Get_Groups)))));

      elsif URI'Length > 4
        and then URI (URI'First .. URI'First + 3) = "/xml"
      then
         return AWS.Response.File
           (MIME.Text_XML,
            Filename => URI (URI'First + 1 .. URI'Last) & ".xml");

      elsif OS_Lib.Is_Regular_File (WWW_Root & URI) then
         return AWS.Response.File
           (MIME.Content_Type (Filename),
            Filename => WWW_Root & URI);

      else
         return AWS.Response.Build
           (MIME.Text_HTML, Message_Body => Templates.Parse (Filename));
      end if;
   end CB;

   --------------
   -- CB_Icons --
   --------------

   function CB_Icons is
     new Services.Callbacks.File ("/we_icons/", WWW_Root & "/icons/");

   -----------
   -- CB_JS --
   -----------

   function CB_JS is
     new Services.Callbacks.File ("/we_js/", WWW_Root & "/javascripts/");

   Port : constant Integer := 2400;

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
   Services.Dispatchers.URI.Register
     (Disp, "/we_js/", CB_JS'Unrestricted_Access, Prefix => True);

   Services.Dispatchers.URI.Register_Default_Callback
     (Disp, Dispatchers.Callback.Create (CB'Unrestricted_Access));

   Server.Start
     (WS,
      Config     => Conf,
      Dispatcher => Disp);

   Server.Wait (AWS.Server.Q_Key_Pressed);

   Server.Shutdown (WS);
end Web_Elements;
