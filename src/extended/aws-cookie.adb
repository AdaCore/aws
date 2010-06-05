------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                        Copyright (C) 2010, AdaCore                       --
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

with Ada.Strings.Fixed;     use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with AWS.Headers;
with AWS.Response.Set;
with AWS.URL;

package body AWS.Cookie is

   --------------
   --  Exists  --
   --------------

   function Exists
     (Request : Status.Data;
      Key     : String) return Boolean
   is
      Headers : constant AWS.Headers.List := Status.Header (Request);
      Cookie  : constant String :=
                  AWS.Headers.Get_Values (Headers, Cookie_Token);
   begin
      return Index (Cookie, Key & "=") > 0;
   end Exists;

   --------------
   --  Expire  --
   --------------

   procedure Expire
     (Content : in out Response.Data;
      Key     : String;
      Path    : String := "/") is
   begin
      Set (Content => Content,
           Key     => Key,
           Value   => "",
           Max_Age => 0,
           Path    => Path);
   end Expire;

   -----------
   --  Get  --
   -----------

   function Get
     (Request : Status.Data;
      Key     : String) return String
   is
      Headers       : constant AWS.Headers.List :=
                        Status.Header (Request);
      Cookie        : constant String :=
                        AWS.Headers.Get_Values (Headers, Cookie_Token);
      Content_Start : constant Natural :=
                        Index (Cookie, Key & "=") + Key'Length + 1;
      Content_End   : Natural;
   begin
      if Content_Start = 0 then
         return "";
      end if;

      Content_End := Index (Cookie, ";", From => Content_Start);

      if Content_End = 0 then
         Content_End := Cookie'Last;
      else
         Content_End := Content_End - 1;
      end if;

      return URL.Decode (Cookie (Content_Start .. Content_End));
   end Get;

   -----------
   --  Get  --
   -----------

   function Get
     (Request : Status.Data;
      Key     : String) return Integer
   is
      Value : constant String := Get (Request, Key);
   begin
      return Integer'Value (Value);
   exception
      when Constraint_Error =>
         return 0;
   end Get;

   function Get
     (Request : Status.Data;
      Key     : String) return Float
   is
      Value : constant String := Get (Request, Key);
   begin
      return Float'Value (Value);
   exception
      when Constraint_Error =>
         return 0.0;
   end Get;

   function Get
     (Request : Status.Data;
      Key     : String) return Boolean is
   begin
      return Get (Request, Key) = "True";
   end Get;

   -----------
   --  Set  --
   -----------

   procedure Set
     (Content : in out Response.Data;
      Key     : String;
      Value   : String;
      Comment : String := "";
      Domain  : String := "";
      Max_Age : Natural := Ten_Years;
      Path    : String := "/";
      Secure  : Boolean := False)
   is
      use type Response.Data_Mode;

      Value_Part     : constant String :=
                         Key & "=" & URL.Encode (Value) & "; ";
      Max_Age_Part   : constant String := Max_Age_Token & Max_Age'Img & "; ";
      Path_Part      : constant String := Path_Token & Path & "; ";
      Cookie_UString : Unbounded_String :=
                         To_Unbounded_String
                           (Value_Part & Max_Age_Part & Path_Part);
   begin
      if Response.Mode (Content) = Response.No_Data then
         raise Response_Data_Not_Initialized;
      end if;

      if Comment /= "" then
         Append (Cookie_UString, Comment_Token & Comment & "; ");
      end if;

      if Domain /= "" then
         Append (Cookie_UString, Domain_Token & Domain & "; ");
      end if;

      if Secure then
         Append (Cookie_UString, Secure_Token & "; ");
      end if;

      Append (Cookie_UString, Version_Token & "; ");

      Response.Set.Add_Header
        (Content,
         Name  => Set_Cookie_Token,
         Value => To_String (Cookie_UString));
   end Set;

   procedure Set
     (Content : in out Response.Data;
      Key     : String;
      Value   : Integer;
      Comment : String := "";
      Domain  : String := "";
      Max_Age : Natural := Ten_Years;
      Path    : String := "/";
      Secure  : Boolean := False)
   is
      String_Value : constant String :=
                       Trim (Integer'Image (Value), Ada.Strings.Left);
   begin
      Set (Content => Content,
           Key     => Key,
           Value   => String_Value,
           Comment => Comment,
           Domain  => Domain,
           Max_Age => Max_Age,
           Path    => Path,
           Secure  => Secure);
   end Set;

   procedure Set
     (Content : in out Response.Data;
      Key     : String;
      Value   : Float;
      Comment : String := "";
      Domain  : String := "";
      Max_Age : Natural := Ten_Years;
      Path    : String := "/";
      Secure  : Boolean := False)
   is
      String_Value : constant String :=
                       Trim (Float'Image (Value), Ada.Strings.Left);
   begin
      Set (Content => Content,
           Key     => Key,
           Value   => String_Value,
           Comment => Comment,
           Domain  => Domain,
           Max_Age => Max_Age,
           Path    => Path,
           Secure  => Secure);
   end Set;

   procedure Set
     (Content : in out Response.Data;
      Key     : String;
      Value   : Boolean;
      Comment : String := "";
      Domain  : String := "";
      Max_Age : Natural := Ten_Years;
      Path    : String := "/";
      Secure  : Boolean := False) is
   begin
      if Value then
         Set (Content => Content,
              Key     => Key,
              Value   => "True",
              Comment => Comment,
              Domain  => Domain,
              Max_Age => Max_Age,
              Path    => Path,
              Secure  => Secure);
      else
         Set (Content => Content,
              Key     => Key,
              Value   => "False",
              Comment => Comment,
              Domain  => Domain,
              Max_Age => Max_Age,
              Path    => Path,
              Secure  => Secure);
      end if;
   end Set;

end AWS.Cookie;
