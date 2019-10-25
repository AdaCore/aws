------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2007-2019, AdaCore                     --
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

with Ada.Characters.Handling;
with Ada.Strings.Fixed;

with AWS.Parameters;
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
      elsif Host (Host'First) = '[' and then Host (Host'Last) = ']' then
         URL.Host :=
           To_Unbounded_String (Host (Host'First + 1 .. Host'Last - 1));
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
      FTP_Token   : constant String := "ftp:";
      HTTP_Token  : constant String := "http:";
      HTTPS_Token : constant String := "https:";
      WS_Token    : constant String := "ws:";
      WSS_Token   : constant String := "wss:";

      L_URL       : constant String :=
                      Strings.Fixed.Translate
                        (URL, Strings.Maps.To_Mapping ("\", "/"));
      P, F        : Natural;
      Scheme      : Unbounded_String;

      function "+" (S : String) return Unbounded_String
         renames To_Unbounded_String;

      procedure Parse (URL : String);
      --  Parse URL, the URL must not contain the HTTP_Token prefix.
      --  If a hostname is specified, the URL should start with "//"

      function Parse_Scheme (URL : String) return String;
      --  Parse the protocol part of the URL and return it. Return an empty
      --  string if not found.

      -----------
      -- Parse --
      -----------

      procedure Parse (URL : String) is

         procedure Set_Host (First, Last : Positive);

         procedure Parse_Path_File (Start : Positive);
         --  Parse Path and File URL information starting at position Start in
         --  URL.

         I1, I2, I3 : Natural;
         LB, RB     : Natural;
         F          : Positive;

         Authority_Specified : Boolean;

         ---------------------
         -- Parse_Path_File --
         ---------------------

         procedure Parse_Path_File (Start : Positive) is
            PF : constant String := Decode (URL (Start .. URL'Last));
            I3 : constant Natural :=
                   Strings.Fixed.Index (PF, "/", Strings.Backward);
         begin
            if I3 = 0 then
               --  No '/' so this is certainly a single file. As a special
               --  exception we check for current and parent directories
               --  which must be part of the path.

               declare
                  File : constant String := PF;
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
                  File : constant String := PF (I3 + 1 .. PF'Last);
               begin
                  if File = ".." or else File = "." then
                     Item.Path := +PF;
                     Item.File := +"";
                  else
                     Item.Path := +PF (PF'First .. I3);
                     Item.File := +File;
                  end if;
               end;
            end if;
         end Parse_Path_File;

         --------------
         -- Set_Host --
         --------------

         procedure Set_Host (First, Last : Positive) is
         begin
            if First < Last
              and then URL (First) = '['
              and then URL (Last) = ']'
            then
               Item.Host := +URL (First + 1 .. Last - 1);
            else
               Item.Host := +URL (First .. Last);
            end if;
         end Set_Host;

      begin
         Authority_Specified := URL'Length > 2
           and then URL (URL'First .. URL'First + 1) = "//";

         if not Authority_Specified then
            --  No "//", there is no authority (hostname or IP) specified.
            --  Let's just parse the data as a path information.
            --
            --  There no is ambiguity here, the first part cannot be a hostname
            --  (the URL would start with "//").

            Item.Host := +"";
            Parse_Path_File (URL'First);

         else
            --  The URL starts with "//", so we look up various characters for
            --  the authority part. At maximum length it could have:
            --
            --  //user:pass@host_or_ip:port/
            --    |   |    |               |
            --    F   I1   I3              I2

            F := URL'First + 2;

            I1 := Strings.Fixed.Index (URL, ":", F);
            I2 := Strings.Fixed.Index (URL, "/", F);
            I3 := Strings.Fixed.Index (URL, "@", F);
            LB := Strings.Fixed.Index (URL, "[", F);
            RB := Strings.Fixed.Index (URL, "]", F);

            --  Check for [user:password@]

            if I1 /= 0 and then I3 /= 0 and then I1 < I3 then
               --  We have [user:password@]
               Item.User     := +URL (F .. I1 - 1);
               Item.Password := +URL (I1 + 1 .. I3 - 1);

               F  := I3 + 1;

               --  Check if there is another ':' specified
               I1 := Strings.Fixed.Index (URL (F .. URL'Last), ":");
            end if;

            --  On the previous example, we now have:
            --
            --  //user:pass@host_or_ip:port/
            --              |         |    |
            --              F         I1   I2
            --  (I3 no longer used)
            --
            --  If IPv6 address in brackets, the I1 should point to the ':'
            --  character right after ']' (port number) or should be 0 (no port
            --  number specified). For example we want:
            --
            --  //optional_user:pass@[IP:6::addr]:port/
            --                       |          ||    |
            --                       F, LB    RB  I1  I2

            if LB < RB and then LB > 0 and then (LB < I2 or else I2 = 0) then
               if RB < URL'Last and then URL (RB + 1) = ':' then
                  I1 := RB + 1;
               else
                  I1 := 0;
               end if;
            end if;

            if I1 = 0 then
               --  In this case we have not port specified
               --  We expect the first string to be the hostname.

               if I2 = 0 then
                  --  No path information, case [user:password@host]
                  Set_Host (F, URL'Last);
                  Item.Path := +"/";

               else
                  --  A path, case [user:password@host/path]
                  Set_Host (F, I2 - 1);
                  Parse_Path_File (I2);
               end if;

            elsif I2 = 0 then
               --  No path, we have [host:port]

               Set_Host (F, I1 - 1);

               if Utils.Is_Number (URL (I1 + 1 .. URL'Last)) then
                  Item.Port := Positive'Value (URL (I1 + 1 .. URL'Last));
               else
                  Raise_URL_Error (Set.Parse.URL, "Port is not valid");
               end if;

               Item.Path := +"/";

            elsif I1 < I2 then
               --  Here we have a complete URL [host:port/path]

               Set_Host (F, I1 - 1);

               if Utils.Is_Number (URL (I1 + 1 .. I2 - 1)) then
                  Item.Port := Positive'Value (URL (I1 + 1 .. I2 - 1));
               else
                  Raise_URL_Error (Set.Parse.URL, "Port is not valid");
               end if;

               Parse_Path_File (I2);

            else
               --  Here we have a complete URL, with no port specified
               --  The semicolon is part of the URL [host/path]

               Set_Host (F, I2 - 1);

               Parse_Path_File (I2);
            end if;
         end if;
      end Parse;

      ------------------
      -- Parse_Scheme --
      ------------------

      function Parse_Scheme (URL : String) return String is
         Res : String := URL;
      begin
         for I in Res'Range loop
            case Res (I) is
               when 'a' .. 'z' =>
                  null;

               when 'A' .. 'Z' =>
                  Res (I) := Characters.Handling.To_Lower (Res (I));

               when ':' =>
                  if I > Res'First then
                     return Res (Res'First .. I - 1);
                  else
                     return "";
                  end if;

               when others =>
                  return "";
            end case;
         end loop;

         return "";
      end Parse_Scheme;

   begin
      Item.Protocol := HTTP;

      --  Checks for fragment

      F := Strings.Fixed.Index (L_URL, "#");

      if F = 0 then
         F := L_URL'Last;
         Item.Fragment := Null_Unbounded_String;

      else
         Item.Fragment := +Decode (L_URL (F .. L_URL'Last));
         F := F - 1;
      end if;

      --  Checks for parameters

      P := Strings.Fixed.Index (L_URL (L_URL'First .. F), "?");

      if P = 0 then
         P := F;

      else
         Item.Parameters.Add (L_URL (P .. F));
         P := P - 1;
      end if;

      --  Checks for prefix

      if Utils.Match (L_URL (L_URL'First .. P), HTTP_Token) then
         Item.Port := Default_HTTP_Port;
         Parse (L_URL (L_URL'First + HTTP_Token'Length .. P));

      elsif Utils.Match (L_URL (L_URL'First .. P), HTTPS_Token) then
         Item.Port := Default_HTTPS_Port;
         Parse (L_URL (L_URL'First + HTTPS_Token'Length .. P));
         Item.Protocol := HTTPS;

      elsif Utils.Match (L_URL (L_URL'First .. P), WS_Token) then
         --  Initial handshake is via http
         Item.Port := Default_HTTP_Port;
         Parse (L_URL (L_URL'First + WS_Token'Length .. P));
         Item.Protocol := HTTP;

      elsif Utils.Match (L_URL (L_URL'First .. P), WSS_Token) then
         --  Initial handshake is via https
         Item.Port := Default_HTTPS_Port;
         Parse (L_URL (L_URL'First + WSS_Token'Length .. P));
         Item.Protocol := HTTPS;

      elsif Utils.Match (L_URL (L_URL'First .. P), FTP_Token) then
         Item.Port := Default_FTP_Port;
         Parse (L_URL (L_URL'First + FTP_Token'Length .. P));
         Item.Protocol := FTP;

      elsif P >= L_URL'First then
         --  No known scheme detected. Look for a scheme anyway and parse the
         --  rest of the URL.
         Item.Port := 0;
         Scheme := To_Unbounded_String
                     (Parse_Scheme (L_URL (L_URL'First .. P)));

         if Scheme /= Null_Unbounded_String then
            Item.Protocol := Scheme;
            Parse (L_URL (L_URL'First + Length (Scheme) + 1 .. P));

         else
            Item.Protocol := Null_Unbounded_String;
            Parse (L_URL (L_URL'First .. P));
         end if;

      else
         --  L_URL = ""
         Item.Protocol := Null_Unbounded_String;
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

   --------------
   -- Security --
   --------------

   procedure Security (URL : in out Object; Flag : Boolean) is
      Port_Default : constant Boolean :=
        (URL.Protocol = HTTPS and then URL.Port = Default_HTTPS_Port)
        or else (URL.Protocol = HTTP and then URL.Port = Default_HTTP_Port)
        or else (URL.Protocol = FTP and then URL.Port = Default_FTP_Port);
   begin
      if Flag then
         URL.Protocol := HTTPS;

         if Port_Default then
            URL.Port := Default_HTTPS_Port;
         end if;

      else
         URL.Protocol := HTTP;

         if Port_Default then
            URL.Port := Default_HTTP_Port;
         end if;
      end if;
   end Security;

end AWS.URL.Set;
