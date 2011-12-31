------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2004-2012, AdaCore                     --
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

   WWW_Root : constant String := "../../web_elements";

   C        : Natural := 0;

   --------
   -- CB --
   --------

   function CB (Request : Status.Data) return Response.Data is
      URI      : constant String := Status.URI (Request);
      Filename : constant String := URI (URI'First + 1 .. URI'Last);
      P_List   : constant Parameters.List := Status.Parameters (Request);
   begin
      if URI = "/" then
         return Response.Build
           (MIME.Text_HTML,
            Message_Body => Templates.Parse ("we_library.html"));

      elsif URI = "/onclick$clickme" then
         C := C + 1;
         return Response.Build
           (MIME.Text_HTML,
            Message_Body => Natural'Image (C) &
               ". response for the click me button!");

      elsif URI = "/onchange$select_color" then
         declare
            Color : constant String := Parameters.Get (P_List, "select_color");
         begin
            return Response.Build
              (MIME.Text_HTML,
               "<p>You have chosen the " &
               "<span class=""" & Color & """><b>" & Color & "</b></span> " &
               "color</p>");
         end;

      elsif URI = "/onclick$data" then
         C := C + 1;
         return Response.Build
           (MIME.Text_HTML,
            Message_Body => Natural'Image (C) &
              ". some data from the server, and the field value '" &
              Parameters.Get (P_List, "field") & ''');

      elsif URI = "/onsubmit$small_form" then
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

      elsif URI = "/onclick$db_sel1" then
         return Response.Build
           (MIME.Text_HTML,
            String'(Templates.Parse ("we_ajax_group.html")));

      elsif URI = "/onclick$db_sel2" then
         return Response.Build
           (MIME.Text_HTML,
            String'(Templates.Parse
              ("we_ajax_user.html",
                 (1 => Templates.Assoc ("GROUP_V", Get_Groups)))));

      elsif URI = "/onclick$g_action" then
         Add_Group (Parameters.Get (P_List, "g_name"));
         return Response.Build
           (MIME.Text_HTML,
            String'(Templates.Parse ("we_ajax_group.html")));

      elsif URI = "/onclick$u_action" then
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

      elsif URI = "/xml_action" then
         C := C + 1;
         return Response.Build
           (MIME.Text_XML,
            String'(Templates.Parse
              ("xml_action.txml",
                 (1 => Templates.Assoc ("COUNTER", C)))));

      elsif URI = "/onclick$xml_get_list" then
         C := C + 1;
         declare
            use type Templates.Tag;
            Result : constant Parameters.VString_Array :=
                       Parameters.Get_Values (P_List, "tolist[]");
            List_V : Templates.Tag;
         begin
            for K in Result'Range loop
               List_V := List_V & Result (K);
            end loop;

            return Response.Build
              (MIME.Text_XML,
               String'(Templates.Parse
                 ("xml_get_list.txml",
                    (1 => Templates.Assoc ("COUNTER", C),
                     2 => Templates.Assoc ("LIST_V", List_V)))));
         end;

      elsif URI = "/onclick$showdata" then
         return Response.Build
           (MIME.Text_XML,
            String'(Templates.Parse
                      ("xml_showdata.txml",
                       (1 => Templates.Assoc
                          ("TEXT", Parameters.Get (P_List, "xfield"))))));

      elsif URI'Length > 12
        and then URI (URI'First .. URI'First + 11) = "/onclick$xml"
      then
         return AWS.Response.File
           (MIME.Text_XML,
            Filename => URI (URI'First + 9 .. URI'Last) & ".xml");

      elsif URI'Length > 6
        and then URI (URI'First .. URI'First + 6) = "/we_js/"
      then
         return AWS.Response.Build
           (MIME.Content_Type (Filename),
            Message_Body => Templates.Parse
               (WWW_Root & "/javascripts" & URI (URI'First + 6 .. URI'Last)));

      elsif Utils.Is_Regular_File (WWW_Root & URI) then
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

   Services.Dispatchers.URI.Register_Default_Callback
     (Disp, Dispatchers.Callback.Create (CB'Unrestricted_Access));

   Server.Start
     (WS,
      Config     => Conf,
      Dispatcher => Disp);

   Server.Wait (AWS.Server.Q_Key_Pressed);

   Server.Shutdown (WS);
end Web_Elements;
