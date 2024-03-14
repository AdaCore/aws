------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2010-2018, AdaCore                     --
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

with Ada.Strings.Fixed;     use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with AWS.Headers.Values;
with AWS.Messages;
with AWS.Response.Set;
with AWS.URL;
with AWS.Utils;

package body AWS.Cookie is

   Version_Token : constant String := "Version=1";

   --------------
   --  Exists  --
   --------------

   function Exists
     (Request        : Status.Data;
      Key            : String;
      Case_Sensitive : Boolean := True) return Boolean
   is
      Headers     : constant AWS.Headers.List := Status.Header (Request);
      Cookies     : constant String :=
                      AWS.Headers.Get_Values (Headers, Messages.Cookie_Token);
      Headers_Set : constant AWS.Headers.Values.Set :=
                      AWS.Headers.Values.Split (Cookies);
      Key_Index   : constant Natural :=
                      AWS.Headers.Values.Index
                        (Headers_Set, Key, Case_Sensitive);
   begin
      return Key_Index > 0;
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
           Max_Age => 0.0,
           Path    => Path);
   end Expire;

   -----------
   --  Get  --
   -----------

   function Get
     (Request        : Status.Data;
      Key            : String;
      Case_Sensitive : Boolean := True) return String
   is
      Headers : constant AWS.Headers.List := Status.Header (Request);
      Cookies : constant String :=
                  AWS.Headers.Get_Values (Headers, Messages.Cookie_Token);
      Value   : constant String :=
                  AWS.Headers.Values.Search (Cookies, Key, Case_Sensitive);
   begin
      return URL.Decode (Value);
   end Get;

   function Get
     (Request        : Status.Data;
      Key            : String;
      Case_Sensitive : Boolean := True) return Integer
   is
      Value : constant String := Get (Request, Key, Case_Sensitive);
   begin
      return Integer'Value (Value);
   exception
      when Constraint_Error =>
         return 0;
   end Get;

   function Get
     (Request        : Status.Data;
      Key            : String;
      Case_Sensitive : Boolean := True) return Float
   is
      Value : constant String := Get (Request, Key, Case_Sensitive);
   begin
      return Float'Value (Value);
   exception
      when Constraint_Error =>
         return 0.0;
   end Get;

   function Get
     (Request        : Status.Data;
      Key            : String;
      Case_Sensitive : Boolean := True) return Boolean is
   begin
      return Get (Request, Key, Case_Sensitive) = "True";
   end Get;

   -----------
   --  Set  --
   -----------

   procedure Set
     (Content   : in out Response.Data;
      Key       : String;
      Value     : String;
      Comment   : String := "";
      Domain    : String := "";
      Max_Age   : Duration := Default.Ten_Years;
      Path      : String := "/";
      Secure    : Boolean := False;
      HTTP_Only : Boolean := False)
   is

      procedure Add (Str : String);
      --  Add value with separator if needed into the cookie value

      Cookie_Content : Unbounded_String;

      ---------
      -- Add --
      ---------

      procedure Add (Str : String) is
      begin
         if Cookie_Content /= Null_Unbounded_String then
            Append (Cookie_Content, "; ");
         end if;

         Append (Cookie_Content, Str);
      end Add;

      Value_Part : constant String := Key & "=" & URL.Encode (Value);
      Path_Part  : constant String := Messages.Path_Token & "=" & Path;

   begin
      if Response.Mode (Content) = Response.No_Data then
         raise Response_Data_Not_Initialized;
      end if;

      Add (Value_Part);
      Add (Path_Part);

      if Max_Age /= No_Max_Age then
         Add (Messages.Max_Age_Token & "=" & Utils.Image (Natural (Max_Age)));
      end if;

      if Comment /= "" then
         Add (Messages.Comment_Token & "=" & Comment);
      end if;

      if Domain /= "" then
         Add (Messages.Domain_Token & "=" & Domain);
      end if;

      if Secure then
         Add (Messages.Secure_Token);
      end if;

      if HTTP_Only then
         Add (Messages.HTTP_Only_Token);
      end if;

      Add (Version_Token);

      Response.Set.Add_Header
        (Content,
         Name  => Messages.Set_Cookie_Token,
         Value => To_String (Cookie_Content));
   end Set;

   procedure Set
     (Content   : in out Response.Data;
      Key       : String;
      Value     : Integer;
      Comment   : String := "";
      Domain    : String := "";
      Max_Age   : Duration := Default.Ten_Years;
      Path      : String := "/";
      Secure    : Boolean := False;
      HTTP_Only : Boolean := False)
   is
      String_Value : constant String := Utils.Image (Value);
   begin
      Set (Content   => Content,
           Key       => Key,
           Value     => String_Value,
           Comment   => Comment,
           Domain    => Domain,
           Max_Age   => Max_Age,
           Path      => Path,
           Secure    => Secure,
           HTTP_Only => HTTP_Only);
   end Set;

   procedure Set
     (Content   : in out Response.Data;
      Key       : String;
      Value     : Float;
      Comment   : String := "";
      Domain    : String := "";
      Max_Age   : Duration := Default.Ten_Years;
      Path      : String := "/";
      Secure    : Boolean := False;
      HTTP_Only : Boolean := False)
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
           Secure  => Secure,
           HTTP_Only => HTTP_Only);
   end Set;

   procedure Set
     (Content   : in out Response.Data;
      Key       : String;
      Value     : Boolean;
      Comment   : String := "";
      Domain    : String := "";
      Max_Age   : Duration := Default.Ten_Years;
      Path      : String := "/";
      Secure    : Boolean := False;
      HTTP_Only : Boolean := False) is
   begin
      if Value then
         Set (Content   => Content,
              Key       => Key,
              Value     => "True",
              Comment   => Comment,
              Domain    => Domain,
              Max_Age   => Max_Age,
              Path      => Path,
              Secure    => Secure,
              HTTP_Only => HTTP_Only);
      else
         Set (Content   => Content,
              Key       => Key,
              Value     => "False",
              Comment   => Comment,
              Domain    => Domain,
              Max_Age   => Max_Age,
              Path      => Path,
              Secure    => Secure,
              HTTP_Only => HTTP_Only);
      end if;
   end Set;

end AWS.Cookie;
