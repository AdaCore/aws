------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2014, AdaCore                     --
--                                                                          --
--  This is free software;  you can redistribute it  and/or modify it       --
--  under terms of the  GNU General Public License as published  by the     --
--  Free Software  Foundation;  either version 3,  or (at your option) any  --
--  later version.  This software is distributed in the hope  that it will  --
--  be useful, but WITHOUT ANY WARRANTY;  without even the implied warranty --
--  of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU     --
--  General Public License for  more details.                               --
--                                                                          --
--  You should have  received  a copy of the GNU General  Public  License   --
--  distributed  with  this  software;   see  file COPYING3.  If not, go    --
--  to http://www.gnu.org/licenses for a complete copy of the license.      --
------------------------------------------------------------------------------

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO;    use Ada.Text_IO;
with AWS.Headers.Values;
with Ada.Strings.Unbounded;

procedure HVal is

   use AWS.Headers.Values;
   use Ada.Strings.Unbounded;

   procedure Test (Data : String);

   Test_1 : constant String := " Token_1 name_1=value_1,";
   Test_2 : constant String := Test_1 & "name_2=""value_2"";" & ASCII.LF;
   Test_3 : constant String := Test_2 & "name_3=""value_3""," & ASCII.HT;
   Test_4 : constant String := Test_3 & "name_4=""value_4""; " & ASCII.CR;
   Test_5 : constant String := Test_4 & "Token_5; ";
   Test_6 : constant String := Test_5 & "Token_6; ";
   Test_7 : constant String := Test_6 & "name_7=value_7;";
   Test_8 : constant String := ",name_8=value_8;";

   ----------
   -- Test --
   ----------

   procedure Test (Data : String) is

      S  : constant Set := Split (Data);

      Dummy : Boolean := False;

      procedure Named_Value (Name, Value : String; Quit : in out Boolean);

      procedure Value (Item : String; Quit : in out Boolean);

      procedure Named_Value (Name, Value : String; Quit : in out Boolean) is
      begin
         Put_Line (Name & "=""" & Value & '"');
      end Named_Value;

      procedure Value (Item : String; Quit : in out Boolean) is
      begin
         Put_Line (Item);
      end Value;

      procedure Test_Parse is new Parse (Value, Named_Value);

   begin
      Put_Line ("===============");

      for I in S'Range loop
         if S (I).Named_Value then
            Named_Value
              (To_String (S (I).Name),
               To_String (S (I).Value),
               Dummy);
         else
            Value (To_String (S (I).Value), Dummy);
         end if;
      end loop;

      Put_Line ("----------");

      --  call the instantiated routine
      Test_Parse (Data);

      New_Line;

   end Test;

begin
   Test (Test_1);
   Test (Test_2);
   Test (Test_3);
   Test (Test_4);
   Test (Test_5);
   Test (Test_6);
   Test (Test_7);
   Test (Test_8);
   Test (Test_1 & Test_2 & Test_3 & Test_4 & Test_5 & Test_6 & Test_7);
   Put_Line (Search (Test_7, "name_1"));
   Put_Line (Search (Test_7, "name_2"));
   Put_Line (Search (Test_7, "name_3"));
   Put_Line (Search (Test_7, "NAME_4", False));
   Put_Line (Search (Test_7, "NAME_4")); -- should be empty line
   Put_Line (Search ("", "nothing")); -- test for empty input string
   Put_Line (Get_Unnamed_Value (Test_7, 1));
   Put_Line (Get_Unnamed_Value (Test_7, 2));
   Put_Line (Get_Unnamed_Value (Test_7, 3));
   Put_Line (Get_Unnamed_Value (Test_7, 4)); -- should be empty line
   Put_Line (Get_Unnamed_Value ("", 2)); -- test for empty input string
   Put_Line (Boolean'Image (Unnamed_Value_Exists (Test_7, "Token_1")));
   Put_Line (Boolean'Image (Unnamed_Value_Exists (Test_7, "Token_2")));
   Put_Line (Boolean'Image (Unnamed_Value_Exists (Test_7, "Token_5")));
   Put_Line (Boolean'Image (Unnamed_Value_Exists (Test_7, "TOKEN_5")));
   Put_Line (Boolean'Image (Unnamed_Value_Exists (Test_7, "TOKEN_5", False)));
   Put_Line (Boolean'Image (Unnamed_Value_Exists (Test_7, "token_6")));
   Put_Line (Boolean'Image (Unnamed_Value_Exists
                              (Test_7 & " Last_Token", "Last_Token")));
   Put_Line (Boolean'Image (Unnamed_Value_Exists
                              (Test_7 & " Last_Token", "Just_Token")));
   Put_Line (Boolean'Image (Unnamed_Value_Exists
                              ("First_Token, " & Test_7,
                               "first_token", False)));
   Put_Line (Boolean'Image (Unnamed_Value_Exists
                              ("First_Token, " & Test_7,
                               "first_token")));
   Put_Line (Boolean'Image (Unnamed_Value_Exists
                       ("", "nothing")));

   --  Test for parse accept tokents
   --  Got from RFC 2616

   Test ("");
   Test ("*");
   Test ("text/plain; q=0.5, text/html,text/x-dvi; q=0.8, text/x-c");
   Test (" text/*, text/html, text/html;level=1, */*");
   Test ("text/*;q=0.3, text/html;q=0.7, text/html;level=1,"
         & "text/html;level=2;q=0.4, */*;q=0.5 ");
   Test ("iso-8859-5, unicode-1-1;q=0.8");
   Test ("compress, gzip");
   Test ("compress;q=0.5, gzip;q=1.0");
   Test ("gzip;q=1.0, identity; q=0.5, *;q=0");
   Test ("da, en-gb;q=0.8, en;q=0.7");

   Put_Line ("Done.");
end HVal;
