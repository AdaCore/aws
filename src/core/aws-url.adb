------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2009, AdaCore                     --
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

with Ada.Characters.Handling;

with AWS.Utils;
with AWS.URL.Raise_URL_Error;
with AWS.URL.Set;

package body AWS.URL is

   subtype Escape_Code is String (1 .. 2);

   Not_Escaped : constant Escape_Code := "  ";

   function Code (C : Character) return Escape_Code;
   pragma Inline (Code);
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
      return Utils.Hex (Character'Pos (C));
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

   ----------
   -- Host --
   ----------

   function Host (URL : Object) return String is
   begin
      return To_String (URL.Host);
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
        or else (URL.Port = Default_HTTPS_Port and then URL.Protocol = HTTPS)
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
      case URL.Protocol is
         when HTTPS =>
            return "https";
         when HTTP =>
            return "http";
         when FTP =>
            return "ftp";
      end case;
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

   --------------
   -- Security --
   --------------

   function Security (URL : Object) return Boolean is
   begin
      return URL.Protocol = HTTPS;
   end Security;

   ---------
   -- URL --
   ---------

   function URL (URL : Object) return String is

      function User_Password return String;
      pragma Inline (User_Password);
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
         return Pathname_And_Parameters (URL);
      else
         return Protocol_Name (URL) & "://"
           & User_Password
           & Host (URL) & Port_Not_Default (URL) & Pathname (URL)
           & Parameters (URL);
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
