------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                            Copyright (C) 2004                            --
--                                ACT-Europe                                --
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
with Ada.Command_Line;
with Ada.Text_IO;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;

with GNAT.AWK;
with GNAT.Directory_Operations;

procedure Gen_GPS_Ref is

   use Ada;
   use Ada.Characters.Handling;
   use Ada.Text_IO;
   use Ada.Strings.Unbounded;
   use GNAT;
   use GNAT.Directory_Operations;

   procedure Gen_Header;

   procedure Gen_Ref (File : in String);

   procedure Gen_Footer;

   ----------------
   -- Gen_Footer --
   ----------------

   procedure Gen_Footer is
   begin
      Put_Line ("</doc>");
   end Gen_Footer;

   ----------------
   -- Gen_Header --
   ----------------

   procedure Gen_Header is
   begin
      Put_Line ("<?xml version=""1.0"" ?>");
      Put_Line ("<!-- This is a GPS Help support file with AWS's API");
      Put_Line ("  -- Just copy this file into ~/.gps/customize -->");
      Put_Line ("<doc>");
      Put_Line ("   <submenu before=""About"" action = """">");
      Put_Line ("      <title>/Help/AWS API</title>");
      Put_Line ("   </submenu>");
   end Gen_Header;

   -------------
   -- Gen_Ref --
   -------------

   procedure Gen_Ref (File : in String) is

      Filename : constant String := File_Name (File);
      Prefix   : Unbounded_String;

      function Get_API_Name return String;
      --  Returns the API name for File, this is done by parsing the file to
      --  get the proper casing.

      function Double_Underscore (Str : in String) return String;
      --  Returns Str with all underscore doubled

      -----------------------
      -- Double_Underscore --
      -----------------------

      function Double_Underscore (Str : in String) return String is
         R : String (1 .. Str'Length * 2);
         K : Natural := 0;
      begin
         for I in Str'Range loop
            K := K + 1;
            if Str (I) = '_' then
               R (K .. K + 1) := "__";
               K := K + 1;
            else
               R (K) := Str (I);
            end if;
         end loop;

         return R (1 .. K);
      end Double_Underscore;

      ------------------
      -- Get_API_Name --
      ------------------

      function Get_API_Name return String is

         Name    : Unbounded_String;
         Session : AWK.Session_Type;

         procedure Action (Quit : in out Boolean);
         --  AWK callback

         ------------
         -- Action --
         ------------

         procedure Action (Quit : in out Boolean) is
            use type AWK.Count;
         begin
            if AWK.Number_Of_Fields (Session) >= 2
              and then (AWK.Field (1, Session) = "package"
                        or else AWK.Field (1, Session) = "procedure"
                        or else AWK.Field (1, Session) = "function")
            then
               Name := To_Unbounded_String (String'(AWK.Field (2, Session)));
               Quit := True;
            end if;

            if AWK.Number_Of_Fields (Session) >= 4
              and then AWK.Field (1, Session) = "private"
              and then (AWK.Field (2, Session) = "package"
                        or else AWK.Field (2, Session) = "function"
                        or else AWK.Field (2, Session) = "procedure")
            then
               Name := To_Unbounded_String (String'(AWK.Field (3, Session)));
               Quit := True;
            end if;
         end Action;

         ---------------------------
         -- Look_For_Package_Name --
         ---------------------------

         procedure Look_For_Package_Name is new AWK.For_Every_Line (Action);

         K : Natural;

      begin
         Look_For_Package_Name (Filename => File, Session => Session);
         AWK.Close (Session);

         K := Strings.Fixed.Index (Filename, "-");

         if K = 0 then
            --  This is not a child package
            K := Strings.Fixed.Index (Filename, ".");
         end if;

         K := K - 1;

         declare
            Base : constant String := Filename (Filename'First .. K);
         begin
            if Base = "templates_parser" then
               Prefix := To_Unbounded_String ("Templates_Parser/");
            else
               Prefix := To_Unbounded_String (To_Upper (Base) & '/');
            end if;
         end;

         return To_String (Name);
      end Get_API_Name;

      API_Name : constant String := Get_API_Name;

   begin
      Put_Line ("   <documentation_file>");
      Put_Line
        ("      <shell>Editor.edit """ & Filename & """</shell>");
      Put_Line ("      <descr>" & API_Name & "</descr>");
      Put_Line ("      <menu>/Help/AWS API/" & To_String (Prefix)
                & Double_Underscore (API_Name) & "</menu>");
      Put_Line ("      <category>AWS API</category>");
      Put_Line ("   </documentation_file>");
   end Gen_Ref;

   File : File_Type;

begin
   Create (File, Out_File, "aws_api.xml");
   Set_Output (File);

   Gen_Header;

   for K in 1 .. Command_Line.Argument_Count loop
      Gen_Ref (Command_Line.Argument (K));
   end loop;

   Gen_Footer;

   Close (File);
end Gen_GPS_Ref;
