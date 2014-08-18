------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2014, AdaCore                     --
--                                                                          --
--  This library is free software;  you can redistribute it and/or modify   --
--  it under terms of the  GNU General Public License  as published by the  --
--  Free Software  Foundation;  either version 3,  or (at your  option) any --
--  later version. This library is distributed in the hope that it will be  --
--  useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                    --
--                                                                          --
--  As a special exception under Section 7 of GPL version 3, you are        --
--  granted additional permissions described in the GCC Runtime Library     --
--  Exception, version 3.1, as published by the Free Software Foundation.   --
--                                                                          --
--  You should have received a copy of the GNU General Public License and   --
--  a copy of the GCC Runtime Library Exception along with this program;    --
--  see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see   --
--  <http://www.gnu.org/licenses/>.                                         --
--                                                                          --
--  As a special exception, if other files instantiate generics from this   --
--  unit, or you link this unit with other files to produce an executable,  --
--  this  unit  does not  by itself cause  the resulting executable to be   --
--  covered by the GNU General Public License. This exception does not      --
--  however invalidate any other reasons why the executable file  might be  --
--  covered by the  GNU Public License.                                     --
------------------------------------------------------------------------------

with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Strings.Unbounded;

with AWS.Config;
with AWS.MIME;
with AWS.Resources;
with AWS.Server;
with AWS.Services.Directory;
with AWS.Templates;
with AWS.Utils;

package body AWS.Services.Page_Server is

   use Ada.Strings.Unbounded;

   Browse_Directory : Boolean := False;
   Cache_Option     : Unbounded_String;
   --  Store the cache options to be sent

   --------------
   -- Callback --
   --------------

   function Callback (Request : Status.Data) return Response.Data is
      use Ada.Strings;
      use type Templates.Translate_Set;

      WWW_Root : String renames Config.WWW_Root
                   (Server.Config (Server.Get_Current.all));
      URI      : constant String := Status.URI (Request);
      Filename : constant String := WWW_Root & URI (2 .. URI'Last);

   begin
      if Resources.Is_Regular_File (Filename) then
         if Cache_Option /= Null_Unbounded_String then
            return Response.File
              (Content_Type  => MIME.Content_Type (Filename),
               Filename      => Filename,
               Cache_Control => Messages.Cache_Option
                 (To_String (Cache_Option)));
         else
            return Response.File
              (Content_Type => MIME.Content_Type (Filename),
               Filename     => Filename);
         end if;

      elsif Browse_Directory and then Utils.Is_Directory (Filename) then
         declare
            Directory_Browser_Page : constant String :=
                                       Config.Directory_Browser_Page
                                         (Server.Config
                                            (Server.Get_Current.all));
         begin
            if Cache_Option /= Null_Unbounded_String then
               return Response.Build
                 (Content_Type => MIME.Text_HTML,
                  Message_Body =>
                    Services.Directory.Browse
                      (Filename, Directory_Browser_Page, Request),
                 Cache_Control => Messages.Cache_Option
                    (To_String (Cache_Option)));
            else
               return Response.Build
                 (Content_Type => MIME.Text_HTML,
                  Message_Body =>
                    Services.Directory.Browse
                      (Filename, Directory_Browser_Page, Request));
            end if;
         end;

      else
         if Resources.Is_Regular_File (WWW_Root & "404.thtml") then
            --  Here we return the 404.thtml page if found. Note that on
            --  Microsoft IE this page will be displayed only if the total
            --  page size is bigger than 512 bytes or if it includes at
            --  leat one image.

            return Response.Acknowledge
                     (Messages.S404,
                      Templates.Parse
                        (WWW_Root & "404.thtml",
                         +Templates.Assoc ("PAGE", URI)));

         else
            return Response.Acknowledge
              (Messages.S404,
               "<p>Page '"
               --  Replace HTML control characters to the HTML inactive symbols
               --  to avoid correct HTML pages initiated from the client side.
               --  See http://www.securityfocus.com/bid/7596
               & Fixed.Translate (URI, Maps.To_Mapping ("<>&", "{}@"))
               & "' Not found.");
         end if;
      end if;
   end Callback;

   ------------------------
   -- Directory_Browsing --
   ------------------------

   procedure Directory_Browsing (Activated : Boolean) is
   begin
      Browse_Directory := Activated;
   end Directory_Browsing;

   -----------------------
   -- Set_Cache_Control --
   -----------------------

   procedure Set_Cache_Control (Data : Messages.Cache_Data) is
   begin
      Cache_Option :=
        To_Unbounded_String (String (Messages.To_Cache_Option (Data)));
   end Set_Cache_Control;

end AWS.Services.Page_Server;
