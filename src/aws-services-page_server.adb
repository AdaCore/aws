------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2004                          --
--                                ACT-Europe                                --
--                                                                          --
--  Authors: Dmitriy Anisimkov - Pascal Obry                                --
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

--  $Id$

with AWS.Config;
with AWS.Messages;
with AWS.MIME;
with AWS.OS_Lib;
with AWS.Resources;
with AWS.Server;
with AWS.Services.Directory;
with AWS.Templates;

package body AWS.Services.Page_Server is

   Browse_Directory : Boolean := False;

   --------------
   -- Callback --
   --------------

   function Callback (Request : in AWS.Status.Data) return AWS.Response.Data is
      WWW_Root : String renames AWS.Config.WWW_Root (Config.Get_Current);
      URI      : constant String := AWS.Status.URI (Request);
      Filename : constant String := WWW_Root & URI (2 .. URI'Last);

   begin
      if Resources.Is_Regular_File (Filename) then
         return AWS.Response.File
           (Content_Type => AWS.MIME.Content_Type (Filename),
            Filename     => Filename);

      elsif Browse_Directory and then OS_Lib.Is_Directory (Filename) then
         declare
            Directory_Browser_Page : constant String
              := Config.Directory_Browser_Page
                   (Server.Config (Server.Get_Current.all));
         begin
            return AWS.Response.Build
              (Content_Type => MIME.Text_HTML,
               Message_Body =>
                 AWS.Services.Directory.Browse
                   (Filename, Directory_Browser_Page, Request));
         end;

      else
         if Resources.Is_Regular_File (WWW_Root & "404.thtml") then

            declare
               Table : constant AWS.Templates.Translate_Table
                 := (1 => Templates.Assoc ("PAGE", URI));
            begin
               --  Here we return the 404.thtml page if found. Note that on
               --  Microsoft IE this page will be displayed only if the total
               --  page size is bigger than 512 bytes or if it includes at
               --  leat one image.

               return AWS.Response.Acknowledge
                 (Messages.S404,
                  Templates.Parse (WWW_Root & "404.thtml", Table));
            end;

         else
            return AWS.Response.Acknowledge
              (Messages.S404,
               "<p>Page '" & URI & "' Not found.");
         end if;
      end if;
   end Callback;

   ------------------------
   -- Directory_Browsing --
   ------------------------

   procedure Directory_Browsing (Activated : in Boolean) is
   begin
      Browse_Directory := Activated;
   end Directory_Browsing;

end AWS.Services.Page_Server;
