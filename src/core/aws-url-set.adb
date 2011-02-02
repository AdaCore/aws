------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2007-2011, AdaCore                     --
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

with Ada.Strings.Fixed;

with AWS.Messages;
with AWS.Parameters.Set;
with AWS.URL.Raise_URL_Error;
with AWS.Utils;

package body AWS.URL.Set is

   function Normalize (Path : Unbounded_String) return Unbounded_String;
   --  Returns Path with all possible occurences of parent and current
   --  directories removed. Does not raise exception.

   ---------------------
   -- Connection_Data --
   ---------------------

   procedure Connection_Data
     (URL      : in out Object;
      Host     : String;
      Port     : Positive;
      Security : Boolean) is
   begin
      if Host = "" then
         URL.Host := To_Unbounded_String ("localhost");
      else
         URL.Host := To_Unbounded_String (Host);
      end if;

      URL.Port := Port;

      if Security then
         URL.Protocol := HTTPS;
      else
         URL.Protocol := HTTP;
      end if;
   end Connection_Data;

   ---------------
   -- Normalize --
   ---------------

   function Normalize (Path : Unbounded_String) return Unbounded_String is
      URL_Path : Unbounded_String := Path;
      K        : Natural;
      P        : Natural;
   begin
      --  Checks for current directory and removes all occurences

      --  Look for starting ./

      if Length (URL_Path) >= 2 and then Slice (URL_Path, 1, 2) = "./" then
         Delete (URL_Path, 1, 1);
      end if;

      --  Look for all // references

      K := 1;

      loop
         K := Index (URL_Path, "//", From => K);

         exit when K = 0;

         if K > 1 and then Slice (URL_Path, K - 1, K - 1) = ":" then
            K := K + 1;
         else
            Delete (URL_Path, K, K);
         end if;
      end loop;

      --  Look for all /./ references

      K := 1;

      loop
         K := Index (URL_Path, "/./", From => K);

         exit when K = 0;

         Delete (URL_Path, K, K + 1);
      end loop;

      --  Checks for parent directory

      P := 1;

      loop
         K := Index (URL_Path, "/../", From => P);

         exit when K = 0;

         --  Look for previous directory, which should be removed

         P := Strings.Fixed.Index
                (Slice (URL_Path, 1, K - 1), "/", Strings.Backward);

         exit when P = 0;

         Delete (URL_Path, P, K + 2);
      end loop;

      return URL_Path;
   end Normalize;

   ----------------
   -- Parameters --
   ----------------

   procedure Parameters (URL : in out Object; Set : AWS.Parameters.List) is
   begin
      URL.Parameters := Set;
   end Parameters;

   function Parameters
     (URL : not null access Object) return access AWS.Parameters.List is
   begin
      return URL.Parameters'Unchecked_Access;
   end Parameters;

   -----------
   -- Parse --
   -----------

   procedure Parse
     (Item           : in out Object;
      URL            : String;
      Check_Validity : Boolean := True;
      Normalize      : Boolean := False)
   is
      FTP_Token   : constant String := "ftp://";
      HTTP_Token  : constant String := "http://";
      HTTPS_Token : constant String := "https://";

      L_URL       : constant String :=
                      Strings.Fixed.Translate
                        (URL, Strings.Maps.To_Mapping ("\", "/"));
      P           : Natural;

      procedure Parse (URL : String; Protocol_Specified : Boolean);
      --  Parse URL, the URL must not contain the HTTP_Token prefix.
      --  Protocol_Specified is set to True when the protocol (http:// or
      --  https:// prefix) was specified. This is used to raise ambiguity
      --  while parsing the URL. See comment below.

      -----------
      -- Parse --
      -----------

      procedure Parse (URL : String; Protocol_Specified : Boolean) is

         function "+" (S : String) return Unbounded_String
            renames To_Unbounded_String;

         procedure Parse_Path_File (Start : Positive);
         --  Parse Path and File URL information starting at position Start in
         --  URL.

         I1, I2, I3 : Natural;
         F          : Positive;

         ---------------------
         -- Parse_Path_File --
         ---------------------

         procedure Parse_Path_File (Start : Positive) is
            PF : constant String := URL (Start .. URL'Last);
            I3 : constant Natural :=
                   Strings.Fixed.Index (PF, "/", Strings.Backward);
         begin
            if I3 = 0 then
               --  No '/' so this is certainly a single file. As a special
               --  exception we check for current and parent directories
               --  which must be part of the path.

               declare
                  File : constant String := URL (Start .. URL'Last);
               begin
                  if File = ".." or else File = "." then
                     Item.Path := +File;
                     Item.File := +"";
                  else
                     Item.Path := +"";
                     Item.File := +File;
                  end if;
               end;

            else
               --  Check that after the last '/' we have not a current or
               --  parent directories which must be part of the path.

               declare
                  File : constant String := URL (I3 + 1 .. URL'Last);
               begin
                  if File = ".." or else File = "." then
                     Item.Path := +URL (Start .. URL'Last);
                     Item.File := +"";
                  else
                     Item.Path := +URL (Start .. I3);
                     Item.File := +File;
                  end if;
               end;
            end if;
         end Parse_Path_File;

         User_Password : Boolean := False;

      begin
         I1 := Strings.Fixed.Index (URL, ":");
         I2 := Strings.Fixed.Index (URL, "/");
         I3 := Strings.Fixed.Index (URL, "@");

         --  Check for [user:password@]

         if I1 /= 0 and then I3 /= 0 and then I1 < I3 then
            --  We have [user:password@]
            Item.User     := +URL (URL'First .. I1 - 1);
            Item.Password := +URL (I1 + 1 .. I3 - 1);

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

            Item.Host := +"";
            Parse_Path_File (URL'First);

         elsif I1 = 0 then
            --  In this case we have not port specified but a [user:password@]
            --  was found, we expect the first string to be the hostname.

            if I2 = 0 then
               --  No path information, case [user:password@host]
               Item.Host := +URL (F .. URL'Last);
               Item.Path := +"/";

            else
               --  A path, case [user:password@host/path]
               Item.Host := +URL (F .. I2 - 1);
               Parse_Path_File (I2);
            end if;

         else
            if I2 = 0 then
               --  No path, we have [host:port]

               Item.Host := +URL (F .. I1 - 1);

               if Utils.Is_Number (URL (I1 + 1 .. URL'Last)) then
                  Item.Port := Positive'Value (URL (I1 + 1 .. URL'Last));
               else
                  Raise_URL_Error (Set.Parse.URL, "Port is not valid");
               end if;

               Item.Path := +"/";

            elsif I1 < I2 then
               --  Here we have a complete URL [host:port/path]

               Item.Host := +URL (F .. I1 - 1);

               if Utils.Is_Number (URL (I1 + 1 .. I2 - 1)) then
                  Item.Port := Positive'Value (URL (I1 + 1 .. I2 - 1));
               else
                  Raise_URL_Error (Set.Parse.URL, "Port is not valid");
               end if;

               Parse_Path_File (I2);

            else
               --  Here we have a complete URL, with no port specified
               --  The semicolon is part of the URL [host/path]

               Item.Host := +URL (F .. I2 - 1);

               Parse_Path_File (I2);
            end if;
         end if;
      end Parse;

   begin
      Item.Protocol := HTTP;

      --  Checks for parameters

      P := Strings.Fixed.Index (L_URL, "?");

      if P = 0 then
         P := L_URL'Last;

      else
         AWS.Parameters.Set.Add (Item.Parameters, L_URL (P .. L_URL'Last));
         P := P - 1;
      end if;

      --  Checks for prefix

      if Messages.Match (L_URL, HTTP_Token) then
         Item.Port := Default_HTTP_Port;
         Parse (L_URL (L_URL'First + HTTP_Token'Length .. P), True);

      elsif Messages.Match (L_URL, HTTPS_Token) then
         Item.Port := Default_HTTPS_Port;
         Parse (L_URL (L_URL'First + HTTPS_Token'Length .. P), True);
         Item.Protocol := HTTPS;

      elsif Messages.Match (L_URL, FTP_Token) then
         Item.Port := Default_FTP_Port;
         Parse (L_URL (L_URL'First + FTP_Token'Length .. P), True);
         Item.Protocol := FTP;

      elsif L_URL /= "" then
         --  Prefix is not recognized, this is either because there is no
         --  protocol specified or the protocol is not supported by AWS. For
         --  example a javascript reference start with "javascript:". This
         --  will be caught on the next parsing level.
         --
         --  At least we know that it is not a Secure HTTP protocol URL.

         Parse (L_URL (L_URL'First .. P), False);
      end if;

      --  Normalize the URL path

      Item.N_Path := Set.Normalize (Item.Path);

      --  Set status

      declare
         Path_Len : constant Natural := Length (Item.N_Path);
      begin
         if (Path_Len >= 4 and then Slice (Item.N_Path, 1, 4) = "/../")
           or else
           (Path_Len = 3 and then Slice (Item.N_Path, 1, 3) = "/..")
         then
            Item.Status := Wrong;
         else
            Item.Status := Valid;
         end if;
      end;

      --  If Normalize is activated, the active URL Path is the normalized one

      if Normalize then
         Item.Path := Item.N_Path;
      end if;

      --  Raise URL_Error if the URL is suspicious

      if Check_Validity and then Item.Status = Wrong then
         Raise_URL_Error
           (To_String (Item.N_Path),
            "Reference Web root parent directory");
      end if;
   end Parse;

end AWS.URL.Set;
