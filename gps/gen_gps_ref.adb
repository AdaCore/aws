------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2004-2012, AdaCore                     --
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

with Ada.Command_Line;
with Ada.Text_IO;
with Ada.Strings;
with Ada.Strings.Unbounded;

with GNAT.AWK;
with GNAT.Directory_Operations;

procedure Gen_GPS_Ref is

   use Ada;
   use Ada.Text_IO;
   use Ada.Strings.Unbounded;
   use GNAT;
   use GNAT.Directory_Operations;

   type Item is record
      Filename : Unbounded_String;
      API_Name : Unbounded_String;
      Root     : Boolean;
   end record;

   API : array (1 .. 1_024) of Item;
   A   : Natural := 0;

   function "-" (Str : Unbounded_String) return String
     renames To_String;

   function "+" (Str : String) return Unbounded_String
     renames To_Unbounded_String;

   procedure Gen_Header;

   procedure Get_Ref (File : String);

   procedure Gen_Refs;

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
      Put_Line ("<!-- This is a GPS Help support file with AWS's API -->");
      Put_Line ("<doc>");
      Put_Line ("   <submenu before=""About"" action = """">");
      Put_Line ("      <title>/Help/AWS</title>");
      Put_Line ("   </submenu>");
   end Gen_Header;

   --------------
   -- Gen_Refs --
   --------------

   procedure Gen_Refs is

      function Get_Menu (A : Positive) return String;
      --  Returns the menu entry for API (K), this routine double the
      --  underscores and handle the root menu. A root menu is generated for
      --  every package having a child.

      --------------
      -- Get_Menu --
      --------------

      function Get_Menu (A : Positive) return String is
         Str : constant String := -API (A).API_Name;
         R   : String (1 .. Str'Length * 2);
         K   : Natural := 0;
         L   : Natural := 0; -- last dot
      begin
         for I in Str'Range loop
            K := K + 1;
            if Str (I) = '_' then
               R (K .. K + 1) := "__";
               K := K + 1;

            elsif Str (I) = '.' then
               R (K) := '/';
               L := K;

            else
               R (K) := Str (I);
            end if;
         end loop;

         if API (A).Root then
            return R (1 .. K) & "/&lt;" & R (L + 1 .. K) & "&gt;";
         else
            return R (1 .. K);
         end if;
      end Get_Menu;

   begin
      --  Sort units

      for K in 1 .. A loop
         for L in K + 1 .. A loop
            declare
               A1 : constant String := -API (K).API_Name;
               A2 : constant String := -API (L).API_Name;
            begin
               if A1 > A2
                 or else (A1'Length > A2'Length
                          and then A1 (A1'First .. A1'First + A2'Length - 1)
                          = A2)
               then
                  declare
                     T : constant Item := API (K);
                  begin
                     API (K) := API (L);
                     API (L) := T;
                  end;
               end if;
            end;
         end loop;
      end loop;

      --  Check for root menu

      for K in 1 .. A - 1 loop
         declare
            C : constant String := -API (K).API_Name;
            N : constant String := -API (K + 1).API_Name;
         begin
            if C'Length < N'Length
              and then C = N (N'First .. N'First + C'Length - 1)
            then
               API (K).Root := True;
            else
               API (K).Root := False;
            end if;
         end;
      end loop;

      --  Generate the references

      for K in 1 .. A loop
         Put_Line ("   <documentation_file>");
         Put_Line
           ("      <shell lang=""python"">aws.open_file ("""
            & (-API (K).Filename) & """)</shell>");
         Put_Line ("      <descr>" & (-API (K).API_Name) & "</descr>");
         Put_Line ("      <menu>/Help/AWS/AWS API/"
                     & Get_Menu (K) & "</menu>");
         Put_Line ("      <category>AWS API</category>");
         Put_Line ("   </documentation_file>");
      end loop;
   end Gen_Refs;

   -------------
   -- Get_Ref --
   -------------

   procedure Get_Ref (File : String) is

      Filename : constant String := File_Name (File);

      function Get_API_Name return String;
      --  Returns the API name for File, this is done by parsing the file to
      --  get the proper casing.

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

      begin
         Look_For_Package_Name (Filename => File, Session => Session);
         AWK.Close (Session);
         return To_String (Name);
      end Get_API_Name;

   begin
      A := A + 1;
      API (A) := (+Filename, +Get_API_Name, False);
   end Get_Ref;

   File : File_Type;

begin
   Create (File, Out_File, "aws_api.xml");
   Set_Output (File);

   Gen_Header;

   for K in 1 .. Command_Line.Argument_Count loop
      Get_Ref (Command_Line.Argument (K));
   end loop;

   Gen_Refs;

   Gen_Footer;

   Close (File);

   Command_Line.Set_Exit_Status (Command_Line.Success);
end Gen_GPS_Ref;
