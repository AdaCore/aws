------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2001                          --
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

with Ada.Characters.Handling;
with Ada.Exceptions;
with Ada.Strings.Fixed;

with AWS.Messages;
with AWS.Utils;

package body AWS.URL is

   use Ada;

   subtype Escape_Code is String (1 .. 2);

   Not_Escaped : constant Escape_Code := "  ";

   function Code (C : in Character) return Escape_Code;
   pragma Inline (Code);
   --  Returns hexadecimal code for character C.

   ----------
   -- Code --
   ----------

   function Code (C : in Character) return Escape_Code is
   begin
      return Utils.Hex (Character'Pos (C));
   end Code;

   Hex_Escape : constant array (Character) of Escape_Code
     := (';' => Code (';'), '/' => Code ('/'), '?' => Code ('?'),
         ':' => Code (':'), '@' => Code ('@'), '&' => Code ('&'),
         '=' => Code ('='), '+' => Code ('+'), '$' => Code ('$'),
         ',' => Code (','), '<' => Code ('<'), '>' => Code ('>'),
         '#' => Code ('#'), '%' => Code ('%'), '"' => Code ('"'),
         '{' => Code ('{'), '}' => Code ('}'), '|' => Code ('|'),
         '\' => Code ('\'), '^' => Code ('^'), '[' => Code ('['),
         ']' => Code (']'), '`' => Code ('`'), others => Not_Escaped);

   ------------
   -- Decode --
   ------------

   function Decode (Str : in String) return String is
      Res : String (1 .. Str'Length);
      K   : Natural := 0;
      I   : Positive := Str'First;
   begin
      if Str = "" then
         return "";
      end if;

      loop
         K := K + 1;

         if Str (I) = '%'
           and then I + 2 <= Str'Last
           and then Characters.Handling.Is_Hexadecimal_Digit (Str (I + 1))
           and then Characters.Handling.Is_Hexadecimal_Digit (Str (I + 2))
         then
            Res (K) := Character'Val
              (Natural'Value ("16#" & Str (I + 1 .. I + 2) & '#'));
            I := I + 2;

         elsif Str (I) = '+' then
            Res (K) := ' ';

         else
            Res (K) := Str (I);
         end if;

         I := I + 1;
         exit when I > Str'Last;
      end loop;

      return Res (1 .. K);
   end Decode;

   ------------
   -- Encode --
   ------------

   function Encode (Str : in String) return String is
      Res : String (1 .. Str'Length * 3);
      K   : Natural := 0;
   begin
      for I in Str'Range loop
         if Str (I) = ' ' then
            --  special case for the space that can be encoded as %20 or
            --  '+'. The later being more readable we use this encoding here.
            K := K + 1;
            Res (K) := '+';

         elsif Hex_Escape (Str (I)) = Not_Escaped then
            K := K + 1;
            Res (K) := Str (I);

         else
            K := K + 1;
            Res (K) := '%';
            K := K + 1;
            Res (K .. K + 1) := Hex_Escape (Str (I));
            K := K + 1;
         end if;
      end loop;

      return Res (1 .. K);
   end Encode;

   ----------
   -- File --
   ----------

   function File
     (URL    : in Object;
      Encode : in Boolean := False)
      return String is
   begin
      if Encode then
         return AWS.URL.Encode (To_String (URL.File));
      else
         return To_String (URL.File);
      end if;
   end File;

   ----------
   -- Host --
   ----------

   function Host (URL : in Object) return String is
   begin
      return To_String (URL.Host);
   end Host;

   ---------------
   -- Normalize --
   ---------------

   procedure Normalize (URL : in out Object) is
      K : Natural;
      P : Natural;
   begin
      --  Checks for parent directory

      loop
         K := Index (URL.Path, "/../");

         exit when K = 0;

         --  Look for previous directory, which should be removed.

         P := Strings.Fixed.Index
           (Slice (URL.Path, 1, K - 1), "/", Strings.Backward);

         exit when P = 0;

         Delete (URL.Path, P, K + 2);
      end loop;

      --  Checks for current directory and removes all occurences

      loop
         K := Index (URL.Path, "/./");

         exit when K = 0;

         Delete (URL.Path, K, K + 1);
      end loop;
   end Normalize;

   ----------------
   -- Parameters --
   ----------------

   function Parameters
     (URL    : in Object;
      Encode : in Boolean := False)
      return String is
   begin
      if Encode then
         return AWS.URL.Encode (To_String (URL.Params));
      else
         return To_String (URL.Params);
      end if;
   end Parameters;

   -----------
   -- Parse --
   -----------

   function Parse (URL : in String) return Object is

      HTTP_Token  : constant String := "http://";
      HTTPS_Token : constant String := "https://";

      P : Natural;

      O : Object;

      procedure Parse (URL : in String);
      --  Parse URL, the URL must not contain the HTTP_Token prefix.

      -----------
      -- Parse --
      -----------

      procedure Parse (URL : in String) is

         function US (S : in String)
           return Unbounded_String
           renames To_Unbounded_String;

         procedure Parse_Path_File;
         --  Parse Path and File URL information

         I1, I2 : Natural;

         ---------------------
         -- Parse_Path_File --
         ---------------------

         procedure Parse_Path_File is
            PF : constant String := URL (I2 + 1 .. URL'Last);
            I3 : constant Natural
              := Strings.Fixed.Index (PF, "/", Strings.Backward);
         begin
            if I3 = 0 then
               O.Path := US ("/");
               O.File := US (URL (I2 + 1 .. URL'Last));
            else
               O.Path := US (URL (I2 .. I3));
               O.File := US (URL (I3 + 1 .. URL'Last));
            end if;
         end Parse_Path_File;

      begin
         I1 := Strings.Fixed.Index (URL, ":");
         I2 := Strings.Fixed.Index (URL, "/");

         if I1 = 0 then
            if I2 = 0 then
               O.Host := US (URL);
               O.Path        := US ("/");
            else
               O.Host := US (URL (URL'First .. I2 - 1));
               Parse_Path_File;
            end if;

         else
            O.Host := US (URL (URL'First .. I1 - 1));

            if I2 = 0 then
               O.Port := Positive'Value (URL (I1 + 1 .. URL'Last));
               O.Path := US ("/");
            else
               O.Port := Positive'Value (URL (I1 + 1 .. I2 - 1));
               Parse_Path_File;
            end if;
         end if;
      end Parse;

   begin
      O.Security := False;

      --  Checks for parameters

      P := Strings.Fixed.Index (URL, "?");

      if P = 0 then
         P := URL'Last;
      else
         O.Params := To_Unbounded_String (URL (P .. URL'Last));
         P := P - 1;
      end if;

      --  Checks for prefix

      if Messages.Match (URL, HTTP_Token) then
         O.Port := Default_HTTP_Port;
         Parse (URL (URL'First + HTTP_Token'Length .. P));

      elsif Messages.Match (URL, HTTPS_Token) then
         O.Port := Default_HTTPS_Port;
         Parse (URL (URL'First + HTTPS_Token'Length .. P));
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
            O.Path := To_Unbounded_String (Slice (O.Path, 2, Length (O.Path)));
         end if;

            O.Security := False;
      end if;

      if O.Host /= Null_Unbounded_String
        and then Length (O.Path) > 3
        and then Slice (O.Path, 1, 4) = "/../"
      then
         Exceptions.Raise_Exception
           (URL_Error'Identity, "URI can't start with /..");
      end if;

      return O;

   exception

      when URL_Error =>
         raise;

      when others =>
         raise URL_Error;
   end Parse;

   ----------
   -- Path --
   ----------

   function Path
     (URL    : in Object;
      Encode : in Boolean := False)
      return String is
   begin
      if Encode then
         return AWS.URL.Encode (To_String (URL.Path));
      else
         return To_String (URL.Path);
      end if;
   end Path;

   --------------
   -- Pathname --
   --------------

   function Pathname
     (URL    : in Object;
      Encode : in Boolean := False)
      return String
   is
      Result : constant String
        := To_String (URL.Path & URL.File & URL.Params);
   begin
      if Encode then
         return AWS.URL.Encode (Result);
      else
         return Result;
      end if;
   end Pathname;

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

   -------------------
   -- Protocol_Name --
   -------------------

   function Protocol_Name (URL : in Object) return String is
   begin
      if URL.Security then
         return "https";
      else
         return "http";
      end if;
   end Protocol_Name;

   --------------
   -- Security --
   --------------

   function Security (URL : in Object) return Boolean is
   begin
      return URL.Security;
   end Security;

   ---------
   -- URL --
   ---------

   function URL (URL : in Object) return String is

      function Port return String;
      pragma Inline (Port);
      --  Returns the port number if not the standard HTTP or HTTPS Port and
      --  the empty string otherwise.

      ----------
      -- Port --
      ----------

      function Port return String is
      begin
         if URL.Security then
            if URL.Port /= Default_HTTPS_Port then
               return ':' & Port (URL);
            else
               return "";
            end if;

         else
            if URL.Port /= Default_HTTP_Port then
               return ':' & Port (URL);
            else
               return "";
            end if;
         end if;
      end Port;

   begin
      if Host (URL) = "" then
         return Pathname (URL);
      else
         return Protocol_Name (URL) & "://"
           & Host (URL) & Port & Pathname (URL);
      end if;
   end URL;

end AWS.URL;
