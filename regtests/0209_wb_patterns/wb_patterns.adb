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
with AWS.Config.Set;
with AWS.Dispatchers.Callback;
with AWS.Messages;
with AWS.Server.Status;
with AWS.Services.Dispatchers.URI;
with AWS.Services.Web_Block.Context;
with AWS.Services.Web_Block.Registry;
with AWS.Status;
with AWS.Templates;
with AWS.Response;
with AWS.Utils;

procedure WB_Patterns is

   use Ada;
   use AWS;
   use AWS.Services;
   use AWS.Services.Web_Block.Registry;

   Cfg : Config.Object;
   WS  : AWS.Server.HTTP;

   procedure Main_Callback
     (Request      :                 Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set);

   procedure Main_P_Callback
     (Request      :                 Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Parameters   :                 Callback_Parameters;
      Translations : in out          Templates.Translate_Set);

   procedure Null_Callback
     (Request      :                 Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set) is null;

   procedure Main_Callback
     (Request      :                 Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set) is
   begin
      Text_IO.Put_Line ("No params");
   end Main_Callback;

   procedure Main_P_Callback
     (Request      :                 Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Parameters   :                 Callback_Parameters;
      Translations : in out          Templates.Translate_Set) is
   begin
      Text_IO.Put_Line (Strings.Unbounded.To_String (Parameters (2)));
   end Main_P_Callback;

   ----------------------
   -- Default_Callback --
   ----------------------

   function Default_Callback (Request : Status.Data) return Response.Data is
      URI          : constant String := Status.URI (Request);
      C_Request    : aliased Status.Data := Request;
      Translations : Templates.Translate_Set;
      Web_Page     : Response.Data;
   begin
      Web_Page := Services.Web_Block.Registry.Build
        (URI, C_Request, Translations,
         Cache_Control => Messages.Prevent_Cache,
         Context_Error => "/");
      return Web_Page;
   end Default_Callback;

   ----------
   -- Test --
   ----------

   procedure Test (URI : String) is
      R : constant Response.Data :=
                     Client.Get (AWS.Server.Status.Local_URL (WS) & URI);
      Content : constant String := Response.Message_Body (R);
   begin
      if Content /= "" then
         Text_IO.Put_Line (Content);
      end if;
   end Test;

   H  : AWS.Services.Dispatchers.URI.Handler;

begin
   Services.Dispatchers.URI.Register_Default_Callback
     (H, AWS.Dispatchers.Callback.Create
        (Default_Callback'Unrestricted_Access));

   Services.Web_Block.Registry.Register
     (Key      => "/main/",
      Template => "page_main.thtml",
      Data_CB  => Main_Callback'Unrestricted_Access);

   Services.Web_Block.Registry.Register_Pattern_URL
     (Prefix   => "/main/",
      Regexp   => "([0-9])/([a-z])/([0-9]+)",
      Template => "page_main.thtml",
      Data_CB  => Main_P_Callback'Unrestricted_Access);

   Services.Web_Block.Registry.Register
     (Key      => "/main/2",
      Prefix   => True,
      Template => "page_main.thtml",
      Data_CB  => Null_Callback'Unrestricted_Access);

   Cfg := AWS.Config.Get_Current;

   AWS.Config.Set.Server_Host (Cfg, "localhost");
   AWS.Config.Set.Server_Port (Cfg, 0);

   AWS.Server.Start (WS, Dispatcher => H, Config => Cfg);

   Test ("/main/8/a/33"); --  Main_P callback
   Test ("/main/2/b/7");  --  Main_P callback
   Test ("/main/23/c/7"); --  Null_Callback (with prefix /main/)
   Test ("/main/3/d/7");  --  Main_P callback
   Test ("/main/");       --  Main_Callback (no prefix)

   --  Close servers

   AWS.Server.Shutdown (WS);
end WB_Patterns;
