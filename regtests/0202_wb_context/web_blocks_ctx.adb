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

with Ada.Text_IO;

with AWS.Client;
with AWS.Config.Set;
with AWS.Dispatchers.Callback;
with AWS.Messages;
with AWS.Server;
with AWS.Services.Dispatchers.URI;
with AWS.Services.Web_Block.Context;
with AWS.Services.Web_Block.Registry;
with AWS.Status;
with AWS.Templates;
with AWS.Response;
with AWS.Utils;

with Get_Free_Port;

procedure Web_Blocks_Ctx is

   use Ada;
   use AWS;
   use AWS.Services;

   Cfg       : Config.Object;
   Free_Port : Positive;

   --------------------
   -- Block_Callback --
   --------------------

   procedure Block_Callback
     (Request      :                 Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set) is
   begin
      Context.Set_Value ("BLOCK", "SET");
      Context.Set_Value ("NAME", "BLOCK");
      Templates.Insert (Translations, Templates.Assoc ("VAR", "_6_"));
   end Block_Callback;

   ---------------------
   -- Block2_Callback --
   ---------------------

   procedure Block2_Callback
     (Request      :                 Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set) is
   begin
      Context.Set_Value ("BLOCK", "SET2");
      Context.Set_Value ("NAME", "BLOCK2");
   end Block2_Callback;

   -------------------
   -- Main_Callback --
   -------------------

   procedure Main_Callback
     (Request      :                 Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set) is
   begin
      Context.Set_Value ("MAIN", "SET");
   end Main_Callback;

   ---------------------
   -- Sblock_Callback --
   ---------------------

   procedure Sblock_Callback
     (Request      :                 Status.Data;
      Context      : not null access Services.Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set) is
   begin
      Context.Set_Value ("SBLOCK", "SET in " & Context.Get_Value ("NAME"));
   end Sblock_Callback;

   ----------------------
   -- Default_Callback --
   ----------------------

   function Default_Callback (Request : Status.Data) return Response.Data is
      URI          : constant String := Status.URI (Request);
      C_Request    : aliased Status.Data := Request;
      Translations : Templates.Translate_Set;
      Web_Page     : Response.Data;
      Context      : aliased Web_Block.Context.Object;
   begin
      Templates.Insert (Translations, Templates.Assoc ("GLOBAL", "GVALUE"));

      Web_Page := Services.Web_Block.Registry.Build
        (URI, C_Request, Translations,
         Context       => Context'Unchecked_Access,
         Cache_Control => Messages.Prevent_Cache,
         Context_Error => "/");

      Text_IO.Put_Line ("MAIN   : " & Context.Get_Value ("MAIN"));
      Text_IO.Put_Line ("BLOCK  : " & Context.Get_Value ("BLOCK"));
      Text_IO.Put_Line ("SBLOCK : " & Context.Get_Value ("SBLOCK"));
      Text_IO.Put_Line ("Main   : " & Context.Get_Value ("Main"));
      return Web_Page;
   end Default_Callback;

   ----------
   -- Test --
   ----------

   procedure Test (URI : String) is
      R       : constant Response.Data :=
                  Client.Get
                    ("http://localhost:" & AWS.Utils.Image (Free_Port) & URI);
      Content : constant String := Response.Message_Body (R);
   begin
      if Content /= "" then
         Text_IO.Put_Line (Content);
      end if;
   end Test;

   H  : AWS.Services.Dispatchers.URI.Handler;

   WS : AWS.Server.HTTP;

begin
   Services.Dispatchers.URI.Register_Default_Callback
     (H, AWS.Dispatchers.Callback.Create
        (Default_Callback'Unrestricted_Access));

   Services.Web_Block.Registry.Register
     ("/main",
      "page_main.thtml",
      Main_Callback'Unrestricted_Access);
   Services.Web_Block.Registry.Register
     ("BLOCK",
      "block.thtml",
      Block_Callback'Unrestricted_Access);
   Services.Web_Block.Registry.Register
     ("BLOCK2",
      "block2.thtml",
      Block2_Callback'Unrestricted_Access);
   Services.Web_Block.Registry.Register
     ("SBLOCK",
      "sblock.thtml",
      Sblock_Callback'Unrestricted_Access);

   --  This default callback will handle all Web_Block callbacks
   Cfg := AWS.Config.Get_Current;

   Free_Port := AWS.Config.Server_Port (Cfg);
   Get_Free_Port (Free_Port);
   AWS.Config.Set.Server_Port (Cfg, Free_Port);

   AWS.Server.Start
     (WS,
      Dispatcher => H,
      Config     => Cfg);

   Test ("/main");

   --  Close servers

   AWS.Server.Shutdown (WS);
end Web_Blocks_Ctx;
