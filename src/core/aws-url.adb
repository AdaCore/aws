------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2017, AdaCore                     --
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

pragma Ada_2012;

with Ada.Characters.Handling;
with Ada.Strings.Fixed;

with AWS.URL.Raise_URL_Error;
with AWS.URL.Set;
with AWS.Utils;

package body AWS.URL is

   subtype Escape_Code is String (1 .. 2);

   Not_Escaped : constant Escape_Code := "  ";

   function Code (C : Character) return Escape_Code with Inline;
   --  Returns hexadecimal code for character C

   subtype ASCII_7 is Character range Character'First .. Character'Val (127);
   type ASCII_7_Set is array (ASCII_7) of Escape_Code;

   function Build_Hex_Escape return ASCII_7_Set;
   --  Returns the table with pre-computed encoding for 7bits characters

   --------------
   -- Abs_Path --
   --------------

   function Abs_Path
     (URL    : Object;
      Encode : Boolean := False) return String
   is
      Result : constant String := To_String (URL.Path & URL.File);
   begin
      if Encode then
         return AWS.URL.Encode (Result);
      else
         return Result;
      end if;
   end Abs_Path;

   ----------------------
   -- Build_Hex_Escape --
   ----------------------

   function Build_Hex_Escape return ASCII_7_Set is
      Result : ASCII_7_Set;
   begin
      for C in Character'Val (0) .. Character'Val (127) loop
         if Strings.Maps.Is_In (C, Default_Encoding_Set) then
            Result (C) := Code (C);
         else
            Result (C) := Not_Escaped;
         end if;
      end loop;
      return Result;
   end Build_Hex_Escape;

   ----------
   -- Code --
   ----------

   function Code (C : Character) return Escape_Code is
   begin
      return Utils.Hex (Character'Pos (C), 2);
   end Code;

   Hex_Escape : constant ASCII_7_Set :=  Build_Hex_Escape;
   --  Limit Hex_Escape to 7bits ASCII characters only. Other ISO-8859-1 are
   --  handled separately in Encode function. Space character is not processed
   --  specifically, contrary to what is done in AWS.URL.

   ------------
   -- Decode --
   ------------

   function Decode (Str : String) return String is
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
            --  A plus is used for spaces in forms value for example
            Res (K) := ' ';

         else
            Res (K) := Str (I);
         end if;

         I := I + 1;
         exit when I > Str'Last;
      end loop;

      return Res (1 .. K);
   end Decode;

   function Decode (Str : Unbounded_String) return Unbounded_String is
      use Characters.Handling;
      Res : Unbounded_String;
      I   : Positive := 1;
   begin
      if Str = Null_Unbounded_String then
         return Null_Unbounded_String;
      end if;

      loop
         if Element (Str, I) = '%'
           and then I + 2 <= Length (Str)
           and then Is_Hexadecimal_Digit (Element (Str, I + 1))
           and then Is_Hexadecimal_Digit (Element (Str, I + 2))
         then
            Append
              (Res,
               Character'Val (Utils.Hex_Value (Slice (Str, I + 1, I + 2))));
            I := I + 2;

         elsif Element (Str, I) = '+' then
            --  A plus is used for spaces in forms value for example
            Append (Res, ' ');

         else
            Append (Res, Element (Str, I));
         end if;

         I := I + 1;
         exit when I > Length (Str);
      end loop;

      return Res;
   end Decode;

   ------------
   -- Encode --
   ------------

   function Encode
     (Str          : String;
      Encoding_Set : Strings.Maps.Character_Set := Default_Encoding_Set)
      return String
   is
      C_128 : constant Character := Character'Val (128);
      Res   : String (1 .. Str'Length * 3);
      K     : Natural := 0;
   begin
      for I in Str'Range loop
         if Strings.Maps.Is_In (Str (I), Encoding_Set) then
            --  This character must be encoded

            K := K + 1;
            Res (K) := '%';
            K := K + 1;

            if Str (I) < C_128 then
               --  We keep a table for characters lower than 128 for efficiency
               Res (K .. K + 1) := Hex_Escape (Str (I));
            else
               Res (K .. K + 1) := Code (Str (I));
            end if;

            K := K + 1;

         else
            K := K + 1;
            Res (K) := Str (I);
         end if;
      end loop;

      return Res (1 .. K);
   end Encode;

   ----------
   -- File --
   ----------

   function File
     (URL    : Object;
      Encode : Boolean := False) return String is
   begin
      if Encode then
         return AWS.URL.Encode (To_String (URL.File));
      else
         return To_String (URL.File);
      end if;
   end File;

   --------------
   -- Fragment --
   --------------

   function Fragment (URL : Object) return String is
   begin
      return To_String (URL.Fragment);
   end Fragment;

   ----------
   -- Host --
   ----------

   function Host
     (URL : Object; IPv6_Brackets : Boolean := False) return String is
   begin
      if IPv6_Brackets
        and then Ada.Strings.Unbounded.Index (URL.Host, ":") > 0
      then
         return '[' & To_String (URL.Host) & ']';
      else
         return To_String (URL.Host);
      end if;
   end Host;

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid (URL : Object) return Boolean is
   begin
      return URL.Status = Valid;
   end Is_Valid;

   ---------------
   -- Normalize --
   ---------------

   procedure Normalize (URL : in out Object) is
   begin
      URL.Path := URL.N_Path;

      if URL.Status = Wrong then
         Raise_URL_Error
           (To_String (URL.Path),
            "Reference Web root parent directory");
      end if;
   end Normalize;

   ---------------
   -- Parameter --
   ---------------

   function Parameter
     (URL : Object; Name : String; N : Positive := 1) return String is
   begin
      return AWS.Parameters.Get (URL.Parameters, Name, N);
   end Parameter;

   ----------------
   -- Parameters --
   ----------------

   function Parameters (URL : Object) return AWS.Parameters.List is
   begin
      return URL.Parameters;
   end Parameters;

   function Parameters
     (URL    : Object;
      Encode : Boolean := False) return String
   is
      P : constant String := AWS.Parameters.URI_Format (URL.Parameters);
   begin
      if P'Length = 0 then
         --  No parameter, we do not want the leading '?'
         return "";

      else
         if Encode then
            return '?' & AWS.URL.Encode (P (P'First + 1 .. P'Last));
         else
            return '?' & P (P'First + 1 .. P'Last);
         end if;
      end if;
   end Parameters;

   -----------
   -- Parse --
   -----------

   function Parse
      (URL            : String;
       Check_Validity : Boolean := True;
       Normalize      : Boolean := False) return Object
   is
      O : Object;
   begin
      Set.Parse (O, URL, Check_Validity, Normalize);

      return O;
   end Parse;

   --------------
   -- Password --
   --------------

   function Password (URL : Object) return String is
   begin
      return To_String (URL.Password);
   end Password;

   ----------
   -- Path --
   ----------

   function Path
     (URL    : Object;
      Encode : Boolean := False) return String is
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
     (URL    : Object;
      Encode : Boolean := False) return String is
   begin
      return Pathname (URL, Encode) & Parameters (URL, Encode);
   end Pathname_And_Parameters;

   ----------
   -- Port --
   ----------

   function Port (URL : Object) return Positive is
   begin
      return URL.Port;
   end Port;

   function Port (URL : Object) return String is
   begin
      return AWS.Utils.Image (URL.Port);
   end Port;

   ----------------------
   -- Port_Not_Default --
   ----------------------

   function Port_Not_Default (URL : Object) return String is
   begin
      if (URL.Port = Default_HTTP_Port and then URL.Protocol = HTTP)
        or else (URL.Port = Default_HTTP_Port and then URL.Protocol = WS)
        or else (URL.Port = Default_HTTPS_Port and then URL.Protocol = HTTPS)
        or else (URL.Port = Default_HTTPS_Port and then URL.Protocol = WSS)
        or else (URL.Port = Default_FTP_Port and then URL.Protocol = FTP)
      then
         return "";
      else
         return ':' & AWS.Utils.Image (URL.Port);
      end if;
   end Port_Not_Default;

   -------------------
   -- Protocol_Name --
   -------------------

   function Protocol_Name (URL : Object) return String is
   begin
      return To_String (URL.Protocol);
   end Protocol_Name;

   -----------
   -- Query --
   -----------

   function Query
     (URL    : Object;
      Encode : Boolean := False) return String
   is
      P : constant String := Parameters (URL, Encode);
   begin
      return P (P'First + 1 .. P'Last);
   end Query;

   -------------
   -- Resolve --
   -------------

   function Resolve (URL : Object; Base_URL : Object) return Object is

      Res : Object;

      function Merge (Path1, Path2 : String) return String;
      --  Merge two paths

      procedure Remove_Dot_Segments (URL : in out Object);
      --  Remove dot segments as per RFC 3986 section 5.2.4

      procedure Set_Protocol (Into :  in out Object; From : Object);
      --  Change protocol of Into

      -----------
      -- Merge --
      -----------

      function Merge (Path1, Path2 : String) return String is
         I : Integer;
      begin
         if Path2'Length > 0 and then Path2 (Path2'First) = '/' then
            return Path2;

         else
            I := Strings.Fixed.Index (Path1, "/", Strings.Backward);

            if I not in Path1'Range then
               return "/" & Path2;
            else
               return Path1 (Path1'First .. I) & Path2;
            end if;
         end if;
      end Merge;

      -------------------------
      -- Remove_Dot_Segments --
      -------------------------

      procedure Remove_Dot_Segments (URL : in out Object) is

         function Starts_With
           (S : String; Pat : String; From : Integer) return Boolean;
         --  Return True if S starts with Pat at index From

         function Remaining (S : String; From : Integer) return String;
         --  Return String from the index From

         procedure Go_Up;
         --  Process ".." in path

         Path : String := To_String (URL.Path);
         I, N : Integer := Path'First;

         -----------
         -- Go_Up --
         -----------

         procedure Go_Up is
            N : Integer;
         begin
            N := Index (URL.N_Path, "/", Ada.Strings.Backward);
            if N = 0 then
               URL.N_Path := Null_Unbounded_String;
            else
               Delete (URL.N_Path, N, Length (URL.N_Path));
            end if;
         end Go_Up;

         ---------------
         -- Remaining --
         ---------------

         function Remaining (S : String; From : Integer) return String is
         begin
            if From in S'Range then
               return S (From .. S'Last);
            else
               return "";
            end if;
         end Remaining;

         -----------------
         -- Starts_With --
         -----------------

         function Starts_With
           (S : String; Pat : String; From : Integer) return Boolean
         is
            To : constant Integer := From + Pat'Length - 1;
         begin
            if From in S'Range and then To in S'Range then
               return S (From .. To) = Pat;
            else
               return False;
            end if;
         end Starts_With;

      begin
         URL.N_Path := Null_Unbounded_String;

         while I in Path'Range loop

            --  A.  If the input buffer begins with a prefix of "../" or "./",
            --      then remove that prefix from the input buffer; otherwise,

            if Starts_With (Path, "../", I) then
               I := I + 3;
            elsif Starts_With (Path, "./", I) then
               I := I + 2;

            --  B.  if the input buffer begins with a prefix of "/./" or "/.",
            --      where "." is a complete path segment, then replace that
            --      prefix with "/" in the input buffer; otherwise,

            elsif Starts_With (Path, "/./", I) then
               I := I + 2;
            elsif Remaining (Path, I) = "/." then
               I := I + 1;
               Path (I) := '/';

            --  C.  if the input buffer begins with a prefix of "/../" or
            --      "/..", where ".." is a complete path segment, then replace
            --      that prefix with "/" in the input buffer and remove the
            --      last segment and its preceding "/" (if any) from the output
            --      buffer; otherwise,

            elsif Starts_With (Path, "/../", I) then
               I := I + 3;
               Go_Up;
            elsif Remaining (Path, I) = "/.." then
               I := I + 2;
               Path (I) := '/';
               Go_Up;

            --  D.  if the input buffer consists only of "." or "..", then
            --      remove that from the input buffer; otherwise,

            elsif Remaining (Path, I) = ".." then
               I := I + 2;
            elsif Remaining (Path, I) = "." then
               I := I + 1;

            else
               --  E.  move the first path segment in the input buffer to the
               --      end of the output buffer, including the initial "/"
               --      character(if any) and any subsequent characters up to,
               --      but not including, the next "/" character or the end of
               --      the input buffer.

               N := Strings.Fixed.Index (Path, "/", I + 1);
               if N in I + 1 .. Path'Last then
                  Append (URL.N_Path, Path (I .. N - 1));
                  I := N;
               else

                  Append (URL.N_Path, Path (I .. Path'Last));
                  I := Path'Last + 1;
               end if;
            end if;
         end loop;
      end Remove_Dot_Segments;

      ------------------
      -- Set_Protocol --
      ------------------

      procedure Set_Protocol (Into :  in out Object; From : Object) is
      begin
         --  If both have default port, update it

         if Into.Port = 0
           or else (Port_Not_Default (Into) = ""
                    and then Port_Not_Default (From) = "")
         then
            Into.Port := From.Port;
         end if;

         Into.Protocol := From.Protocol;
      end Set_Protocol;

   begin
      if Protocol_Name (URL) /= "" then
         Res := URL;

      else
         if Host (URL) /= "" then
            Res := URL;
            Set_Protocol  (Res, Base_URL);

         else
            if Abs_Path (URL) = "" then
               Res := Base_URL;
               if Query (URL) /= "" then
                  Res.Parameters := URL.Parameters;
               end if;

            else
               Res := URL;
               if URL.Path = Null_Unbounded_String
                 or else Slice (URL.Path, 1, 1) /= "/"
               then
                  --  Merge Base_URL path with URL path
                  Res.Path := To_Unbounded_String
                    (Merge (Abs_Path (Base_URL), To_String (URL.Path)));
               end if;

               Res.Parameters := URL.Parameters;
               Res.User     := Base_URL.User;
               Res.Password := Base_URL.Password;
               Res.Host     := Base_URL.Host;
               Res.Port     := Base_URL.Port;
            end if;
         end if;

         Res.Protocol := Base_URL.Protocol;
      end if;

      Res.Fragment := URL.Fragment;
      Remove_Dot_Segments (Res);
      Res.Path := Res.N_Path;

      return Res;
   end Resolve;

   -------------
   -- Resolve --
   -------------

   function Resolve (URL : String; Base_URL : String) return String is
   begin
      return AWS.URL.URL (Resolve (Parse (URL), Parse (Base_URL)));
   end Resolve;

   --------------
   -- Security --
   --------------

   function Security (URL : Object) return Boolean is
   begin
      return URL.Protocol = HTTPS or else URL.Protocol = WSS;
   end Security;

   ---------
   -- URL --
   ---------

   function URL (URL : Object) return String is

      function User_Password return String with Inline;
      --  Returns the user:password@ if present and the empty string otherwise

      -------------------
      -- User_Password --
      -------------------

      function User_Password return String is
         User     : constant String := To_String (URL.User);
         Password : constant String := To_String (URL.Password);
      begin
         if User = "" and then Password = "" then
            return "";
         else
            return User & ':' & Password & '@';
         end if;
      end User_Password;

   begin
      if Host (URL) = "" then
         if Protocol_Name (URL) /= "" then
            return Protocol_Name (URL) & ":" & Pathname_And_Parameters (URL);
         else
            return Pathname_And_Parameters (URL);
         end if;
      else
         return Protocol_Name (URL) & "://"
           & User_Password & Host (URL, IPv6_Brackets => True)
           & Port_Not_Default (URL) & Pathname (URL) & Parameters (URL)
           & Fragment (URL);
      end if;
   end URL;

   ----------
   -- User --
   ----------

   function User (URL : Object) return String is
   begin
      return To_String (URL.User);
   end User;

end AWS.URL;
