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

   ------------
   -- Encode --
   ------------

   function Encode (URL : in String) return String is
      Res : String (1 .. URL'Length * 3);
      K   : Natural := 0;
   begin
      for I in URL'Range loop

         case URL (I) is
            when ' ' =>
               K := K + 1;
               Res (K .. K + 2) := "%20";
               K := K + 2;

            when others =>
               K := K + 1;
               Res (K) := URL (I);
         end case;
      end loop;

      return Res (1 .. K);
   end Encode;

   ---------------
   -- Normalize --
   ---------------

   procedure Normalize (URL : in out Object) is
      K : Natural;
      P : Natural;
   begin
      --  checks for parent directory

      loop
         K:= Index (URL.URI, "/../");

         exit when K = 0;

         --  look for previous directory, which should be removed.

         P := Strings.Fixed.Index
           (Slice (URL.URI, 1, K - 1), "/", Strings.Backward);

         exit when P = 0;

         Delete (URL.URI, P, K + 2);
      end loop;

      --  checks for current directory

      loop
         K:= Index (URL.URI, "/./");

         exit when K = 0;

         --  look for previous directory, which should be removed.

         P := Strings.Fixed.Index
           (Slice (URL.URI, 1, K - 1), "/", Strings.Backward);

         exit when P = 0;

         Delete (URL.URI, P, K + 1);
      end loop;
   end Normalize;

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

      elsif URL /= "" then
         --  No server and port, just an URL.

         if URL (URL'First) = '/' then
            --  This is a rooted URL, no problem to parse as-is
            Parse (URL);

         else
            --  This is not rooted. Parse with a '/' slash added, then remove
            --  it after parsing.
            Parse ('/' & URL);
            O.URI := To_Unbounded_String (Slice (O.URI, 2, Length (O.URI)));
         end if;

            O.Security := False;
      end if;

      return O;

   exception
      when others =>
         raise URL_Error;
   end Parse;

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

   -----------------
   -- Server_Name --
   -----------------

   function Server_Name (URL : in Object) return String is
   begin
      return To_String (URL.Server_Name);
   end Server_Name;

   ---------
   -- URI --
   ---------

   function URI
     (URL    : in Object;
      Encode : in Boolean := False)
     return String is
   begin
      if Encode then
         return AWS.URL.Encode (To_String (URL.URI));
      else
         return To_String (URL.URI);
      end if;
   end URI;

   ---------
   -- URL --
   ---------

   function URL (URL : in Object) return String is

      function HTTP return String;
      pragma Inline (HTTP);
      --  Returns the HTTP protocol to be used.

      function Port return String;
      pragma Inline (Port);
      --  Returns the port number if not the standard HTTP Port and the empty
      --  string otherwise.

      ----------
      -- HTTP --
      ----------

      function HTTP return String is
      begin
         if URL.Security then
            return "https://";
         else
            return "http://";
         end if;
      end HTTP;

      function Port return String is
      begin
         if URL.Port /= 80 then
            return ':' & Port (URL);
         else
            return "";
         end if;
      end Port;

   begin
      if Server_Name (URL) = "" then
         return URI (URL);
      else
         return HTTP & Server_Name (URL) & Port & URI (URL);
      end if;
   end URL;

end AWS.URL;
