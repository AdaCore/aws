------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2002                          --
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
with Ada.Strings.Fixed;
with Ada.Strings.Maps;

with AWS.Messages;
with AWS.Utils;
with AWS.URL.Raise_URL_Error;

package body AWS.URL is

   use Ada;

   subtype Escape_Code is String (1 .. 2);

   Not_Escaped : constant Escape_Code := "  ";

   function Code (C : in Character) return Escape_Code;
   pragma Inline (Code);
   --  Returns hexadecimal code for character C.

   function Normalize (Path : in Unbounded_String) return Unbounded_String;
   --  Returns Path with all possible occurences of parent and current
   --  directories removed. Does not raise exception.

   --------------
   -- Abs_Path --
   --------------

   function Abs_Path
     (URL    : in Object;
      Encode : in Boolean := False)
      return String
   is
      Result : constant String
        := To_String (URL.Path & URL.File);
   begin
      if Encode then
         return AWS.URL.Encode (Result);
      else
         return Result;
      end if;
   end Abs_Path;

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
            Res (K) := Character'Val (Utils.Hex_Value (Str (I + 1 .. I + 2)));
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

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid (URL : in Object) return Boolean is
   begin
      return URL.Status = Valid;
   end Is_Valid;

   ---------------
   -- Normalize --
   ---------------

   function Normalize (Path : in Unbounded_String) return Unbounded_String is
      URL_Path : Unbounded_String := Path;

      K : Natural;
      P : Natural;
   begin
      --  Checks for current directory and removes all occurences

      --  Look for starting ./

      if Length (URL_Path) >= 2 and then Slice (URL_Path, 1, 2) = "./" then
         Delete (URL_Path, 1, 1);
      end if;

      --  Look for all /./ references

      loop
         K := Index (URL_Path, "/./");

         exit when K = 0;

         Delete (URL_Path, K, K + 1);
      end loop;

      --  Checks for parent directory

      loop
         K := Index (URL_Path, "/../");

         exit when K = 0;

         --  Look for previous directory, which should be removed.

         P := Strings.Fixed.Index
           (Slice (URL_Path, 1, K - 1), "/", Strings.Backward);

         exit when P = 0;

         Delete (URL_Path, P, K + 2);
      end loop;

      return URL_Path;
   end Normalize;

   procedure Normalize (URL : in out Object) is
   begin
      URL.Path := URL.N_Path;

      if URL.Status = Wrong then
         Raise_URL_Error
           (To_String (URL.Path),
            "Reference Web root parent directory");
      end if;
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

   function Parse
      (URL            : in String;
       Check_Validity : in Boolean := True;
       Normalize      : in Boolean := False)
       return Object
   is
      HTTP_Token  : constant String := "http://";
      HTTPS_Token : constant String := "https://";

      L_URL : constant String
        := Strings.Fixed.Translate (URL, Strings.Maps.To_Mapping ("\", "/"));

      P : Natural;

      O : Object;

      procedure Parse (URL : in String; Protocol_Specified : in Boolean);
      --  Parse URL, the URL must not contain the HTTP_Token prefix.
      --  Protocol_Specified is set to True when the protocol (http:// or
      --  https:// prefix) was specified. This is used to raise ambiguity
      --  while parsing the URL. See comment below.

      -----------
      -- Parse --
      -----------

      procedure Parse (URL : in String;  Protocol_Specified : in Boolean) is

         function "+"
           (S : in String)
            return Unbounded_String
            renames To_Unbounded_String;

         procedure Parse_Path_File (Start : in Positive);
         --  Parse Path and File URL information starting at position Start in
         --  URL.

         I1, I2, I3 : Natural;
         F          : Positive;

         ---------------------
         -- Parse_Path_File --
         ---------------------

         procedure Parse_Path_File (Start : in Positive) is
            PF : constant String := URL (Start .. URL'Last);
            I3 : constant Natural
              := Strings.Fixed.Index (PF, "/", Strings.Backward);
         begin
            if I3 = 0 then
               --  No '/' so this is certainly a single file. As a special
               --  exception we check for current and parent directories
               --  which must be part of the path.

               declare
                  File : constant String := URL (Start .. URL'Last);
               begin
                  if File = ".." or else File = "." then
                     O.Path := +File;
                     O.File := +"";
                  else
                     O.Path := +"";
                     O.File := +File;
                  end if;
               end;

            else
               --  Check that after the last '/' we have not a current or
               --  parent directories which must be part of the path.

               declare
                  File : constant String := URL (I3 + 1 .. URL'Last);
               begin
                  if File = ".." or else File = "." then
                     O.Path := +URL (Start .. URL'Last);
                     O.File := +"";
                  else
                     O.Path := +URL (Start .. I3);
                     O.File := +File;
                  end if;
               end;
            end if;
         end Parse_Path_File;

         User_Password : Boolean := False;

      begin
         I1 := Strings.Fixed.Index (URL, ":");
         I2 := Strings.Fixed.Index (URL, "/");
         I3 := Strings.Fixed.Index (URL, "@");

         --  Check for [user:pawwsord@]

         if I1 /= 0 and then I3 /= 0 and then I1 < I3 then
            --  We have [user:password@]
            O.User     := +URL (URL'First .. I1 - 1);
            O.Password := +URL (I1 + 1 .. I3 - 1);

            F  := I3 + 1;

            --  Check if there is another ':' specified
            I1 := Strings.Fixed.Index (URL (F .. URL'Last), ":");

            User_Password := True;

         else
            F := URL'First;
         end if;

         if I1 = 0
           and then not User_Password
           and then not Protocol_Specified
         then
            --  No ':', there is no port specified and no host since we did
            --  not have a [user:password@] parsed and there was no protocol
            --  specified. Let's just parse the data as a path information.
            --
            --  There is ambiguity here, the data could be either:
            --
            --     some_host_name/some_path
            --   or
            --     relative_path/some_more_path
            --
            --  As per explanations above we take the second choice.

            O.Host := +"";
            Parse_Path_File (URL'First);

         elsif I1 = 0 then
            --  In this case we have not port specified but a [user:password@]
            --  was found, we expect the first string to be the hostname.

            if I2 = 0 then
               --  No path information, case [user:password@host]
               O.Host := +URL (F .. URL'Last);
               O.Path := +"/";

            else
               --  A path, case [user:password@host/path]
               O.Host := +URL (F .. I2 - 1);
               Parse_Path_File (I2);
            end if;

         else
            --  Here we have a port specified [host:port]
            O.Host := +URL (F .. I1 - 1);

            if I2 = 0 then
               --  No path, we have [host:port]
               if Utils.Is_Number (URL (I1 + 1 .. URL'Last)) then
                  O.Port := Positive'Value (URL (I1 + 1 .. URL'Last));
               else
                  Raise_URL_Error (AWS.URL.Parse.URL, "Port is not valid");
               end if;

               O.Path := +"/";
            else
               --  Here we have a complete URL [host:port/path]
               if Utils.Is_Number (URL (I1 + 1 .. I2 - 1)) then
                  O.Port := Positive'Value (URL (I1 + 1 .. I2 - 1));
               else
                  Raise_URL_Error (AWS.URL.Parse.URL, "Port is not valid");
               end if;

               Parse_Path_File (I2);
            end if;
         end if;
      end Parse;

   begin
      O.Security := False;

      --  Checks for parameters

      P := Strings.Fixed.Index (L_URL, "?");

      if P = 0 then
         P := L_URL'Last;
      else
         O.Params := To_Unbounded_String (L_URL (P .. L_URL'Last));
         P := P - 1;
      end if;

      --  Checks for prefix

      if Messages.Match (L_URL, HTTP_Token) then
         O.Port := Default_HTTP_Port;
         Parse (L_URL (L_URL'First + HTTP_Token'Length .. P), True);

      elsif Messages.Match (L_URL, HTTPS_Token) then
         O.Port := Default_HTTPS_Port;
         Parse (L_URL (L_URL'First + HTTPS_Token'Length .. P), True);
         O.Security := True;

      elsif L_URL /= "" then
         --  Prefix is not recognied, this is either because there is no
         --  protocol specified or the protocol is not supported by AWS. For
         --  example a javascript reference start with "javascript:". This
         --  will be catched on the next parsing level.
         --
         --  At least we know that it is not a Secure HTTP protocol URL.

         O.Security := False;

         Parse (L_URL (L_URL'First .. P), False);
      end if;

      --  Normalize the URL path

      O.N_Path := AWS.URL.Normalize (O.Path);

      --  Set status

      declare
         Path_Len : constant Natural := Length (O.N_Path);
      begin
         if (Path_Len >= 4
               and then Slice (O.N_Path, 1, 4) = "/../")
           or else
           (Path_Len = 3
              and then Slice (O.N_Path, 1, 3) = "/..")
         then
            O.Status := Wrong;
         else
            O.Status := Valid;
         end if;
      end;

      --  If Normalize is activated, the active URL Path is the normalized one

      if Normalize then
         O.Path := O.N_Path;
      end if;

      --  Raise URL_Error is the URL is suspicious

      if Check_Validity and then O.Status = Wrong then
         Raise_URL_Error
           (To_String (O.N_Path),
            "Reference Web root parent directory");
      end if;

      return O;
   end Parse;

   --------------
   -- Password --
   --------------

   function Password (URL : in Object) return String is
   begin
      return To_String (URL.Password);
   end Password;

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

   -----------------------------
   -- Pathname_And_Parameters --
   -----------------------------

   function Pathname_And_Parameters
     (URL    : in Object;
      Encode : in Boolean := False)
      return String is
   begin
      return Pathname (URL, Encode) & Parameters (URL, Encode);
   end Pathname_And_Parameters;

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

   -----------
   -- Query --
   -----------

   function Query
     (URL    : in Object;
      Encode : in Boolean := False)
      return String
   is
      P : constant String := Parameters (URL, Encode);
   begin
      return P (P'First + 1 .. P'Last);
   end Query;

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

      function User_Password return String;
      pragma Inline (User_Password);
      --  Returns the user:password@ if present and the empty string otherwise

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

      -------------------
      -- User_Password --
      -------------------

      function User_Password return String is
         User     : constant String := To_String (URL.User);
         Password : constant String := To_String (URL.Password);
      begin
         if User = "" then
            if Password = "" then
               return "";
            else
               return ':' & Password & '@';
            end if;

         else
            if Password = "" then
               return User & ":@";
            else
               return User & ':' & Password & '@';
            end if;
         end if;
      end User_Password;

   begin
      if Host (URL) = "" then
         return Pathname_And_Parameters (URL);
      else
         return Protocol_Name (URL) & "://"
           & User_Password
           & Host (URL) & Port & Pathname (URL) & Parameters (URL);
      end if;
   end URL;

   ----------
   -- User --
   ----------

   function User (URL : in Object) return String is
   begin
      return To_String (URL.User);
   end User;

end AWS.URL;
