------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2001                          --
--                                ACT-Europe                                --
--                                                                          --
--  Authors: Dmitriy Anisimov - Pascal Obry                                 --
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

with Ada.Strings.Fixed;

with AWS.Messages;

package body AWS.URL is

   use Ada;

   -----------
   -- Parse --
   -----------

   function Parse (URL : in String) return Object is

      HTTP_Token  : constant String := "http://";
      HTTPS_Token : constant String := "https://";

      O : Object;

      procedure Parse (URL : in String);
      --  parse URL, the URL must not contain the HTTP_Token prefix.

      -----------
      -- Parse --
      -----------

      procedure Parse (URL : in String) is

         function US (S : in String)
                     return Unbounded_String
           renames To_Unbounded_String;

         I1, I2 : Natural;

      begin
         I1 := Strings.Fixed.Index (URL, ":");
         I2 := Strings.Fixed.Index (URL, "/");

         if I1 = 0 then
            O.Port := Default_Port;

            if I2 = 0 then
               O.Server_Name := US (URL);
               O.URI         := US ("/");
            else
               O.Server_Name := US (URL (URL'First .. I2 - 1));
               O.URI         := US (URL (I2 .. URL'Last));
            end if;

         else

            O.Server_Name := US (URL (URL'First .. I1 - 1));

            if I2 = 0 then
               O.Port := Positive'Value (URL (I1 + 1 .. URL'Last));
               O.URI  := US ("/");
            else
               O.Port := Positive'Value (URL (I1 + 1 .. I2 - 1));
               O.URI  := US (URL (I2 .. URL'Last));
            end if;
         end if;
      end Parse;

   begin
      if Messages.Is_Match (URL, HTTP_Token) then
         Parse (URL (URL'First + HTTP_Token'Length .. URL'Last));
         O.Security := False;
      elsif Messages.Is_Match (URL, HTTPS_Token) then
         Parse (URL (URL'First + HTTPS_Token'Length .. URL'Last));
         O.Security := True;
      else
         Parse (URL);
         O.Security := False;
      end if;
      return O;
   exception
      when others =>
         raise URL_Error;
   end Parse;

   -----------------
   -- Server_Name --
   -----------------

   function Server_Name (URL : in Object) return String is
   begin
      return To_String (URL.Server_Name);
   end Server_Name;

   ----------
   -- Port --
   ----------

   function Port (URL : in Object) return Positive is
   begin
      return URL.Port;
   end Port;

   function Port (URL : in Object) return String is
      P_Image : constant String := Positive'Image (URL.Port);
   begin
      return P_Image (2 .. P_Image'Last);
   end Port;

   --------------
   -- Security --
   --------------

   function Security (URL : in Object) return Boolean is
   begin
      return URL.Security;
   end Security;

   ---------
   -- URI --
   ---------

   function URI (URL : in Object) return String is
   begin
      return To_String (URL.URI);
   end URI;

end AWS.URL;
