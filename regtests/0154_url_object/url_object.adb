------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2007-2012, AdaCore                     --
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

with Ada.Exceptions;
with Ada.Environment_Variables;
with Ada.Text_IO;
with Ada.Strings.Fixed;

with AWS.Client;
with AWS.Parameters;
with AWS.Server.Status;
with AWS.Response;
with AWS.Status;
with AWS.MIME;
with AWS.URL;
with AWS.Utils;

procedure URL_Object is

   use Ada;
   use AWS;

   WS : Server.HTTP;

   function CB (Request : Status.Data) return Response.Data is
      U : constant URL.Object := Status.URI (Request);
      P : constant Parameters.List := Status.Parameters (Request);

      URL_Image  : String := URL.URL (U);
      Port       : constant Positive := Server.Status.Port (WS);
      Port_Image : constant String  := Utils.Image (Port);
      Port_Idx   : constant Natural :=
        Strings.Fixed.Index (URL_Image, ':' & Port_Image & '/');

   begin
      Strings.Fixed.Replace_Slice
        (URL_Image, Port_Idx + 1, Port_Idx + Port_Image'Length, "port");

      Text_IO.Put_Line ("p1=" & Parameters.Get (P, "p1"));
      Text_IO.Put_Line ("p2=" & Parameters.Get (P, "p2"));
      Text_IO.Put_Line ("----------------------");
      Text_IO.Put_Line ("p1=" & Status.Parameter (Request, "p1"));
      Text_IO.Put_Line ("p2=" & Status.Parameter (Request, "p2"));
      Text_IO.Put_Line ("----------------------");
      Text_IO.Put_Line ("URI         = " & Status.URI (Request));
      Text_IO.Put_Line ("URL         = " & URL_Image);
      Text_IO.Put_Line ("Query       = " & URL.Query (U));
      Text_IO.Put_Line ("Path        = " & URL.Path (U));
      Text_IO.Put_Line ("Pathname    = " & URL.Pathname (U));
      Text_IO.Put_Line ("File        = " & URL.File (U));
      Text_IO.Put_Line ("Parameters  = " & URL.Parameters (U));
      Text_IO.Put_Line ("Server_Name = " & URL.Server_Name (U));

      if URL.Port (U) /= Port or else URL.Port (U) /= Port_Image then
         Text_IO.Put_Line ("URL.Port error");
      end if;

      return Response.Build (MIME.Text_HTML, "not used");

   exception
      when E : others =>
         Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));

         return Response.Build (MIME.Text_HTML, "error");
   end CB;

   procedure Test_Relative_Resolution
     (Base : String; URI : String; Expected : String)
   is
      use Ada.Strings.Fixed;
      use AWS.URL;
      Len    : Integer;
      Pad    : Integer;
      URI_O  : AWS.URL.Object;
      Base_O : AWS.URL.Object;
      Result : AWS.URL.Object;

   begin
      URI_O  := Parse (URI, Check_Validity => False);
      Base_O := Parse (Base, Check_Validity => False);
      Result := Resolve (URI_O, Base_O);

      if URI'Length > 20 then
         Len := URI'Length;
         Pad := 0;
      else
         Len := 20;
         Pad := 20 - URI'Length;
      end if;

      Text_IO.Put_Line (URI & (Pad * ' ') & " -> " & AWS.URL.URL (Result));

      if AWS.URL.URL (Result) /= Expected then
         Text_IO.Put_Line
           ("FAILURE" & ((Len - 13) * ' ') & "Expected: " & Expected);
         Text_IO.Put_Line ("* " & AWS.URL.URL (Base_O));
         Text_IO.Put_Line ("  Protocol = " & Protocol_Name (Base_O));
         Text_IO.Put_Line ("  Host     = " & Host (Base_O));
         Text_IO.Put_Line ("  Abs_Path = " & Abs_Path (Base_O));
         Text_IO.Put_Line ("  Path     = " & Path (Base_O));
         Text_IO.Put_Line ("  File     = " & File (Base_O));
         Text_IO.Put_Line ("* " & AWS.URL.URL (URI_O));
         Text_IO.Put_Line ("  Protocol = " & Protocol_Name (URI_O));
         Text_IO.Put_Line ("  Host     = " & Host (URI_O));
         Text_IO.Put_Line ("  Abs_Path = " & Abs_Path (URI_O));
         Text_IO.Put_Line ("  Path     = " & Path (URI_O));
         Text_IO.Put_Line ("  File     = " & File (URI_O));
         Text_IO.New_Line;
      end if;

   exception
      when E : others =>
         if Ada.Environment_Variables.Exists ("debug_aws_test") then
            Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
         else
            --  Do not show stack locations if not requested:
            Text_IO.Put_Line ("Exception name: "
                             & Ada.Exceptions.Exception_Name (E));
            Text_IO.Put_Line ("Message: "
                             & Ada.Exceptions.Exception_Message (E));
            Text_IO.New_Line;
         end if;
   end Test_Relative_Resolution;

begin
   Server.Start (WS, "url_object", CB'Unrestricted_Access, Port => 0);
   Text_IO.Put_Line ("started"); Ada.Text_IO.Flush;

   declare
      URL : constant String := AWS.Server.Status.Local_URL (WS);
      R   : Response.Data;
   begin
      R := Client.Get (URL & "/get_it?p1=1&p2=toto");
      R := Client.Get (URL & "/get_it/disk.html?p1=0956&p2=uuu");
   end;

   Server.Shutdown (WS);
   Text_IO.Put_Line ("shutdown");

   Text_IO.New_Line;
   Text_IO.Put_Line ("------------------------------------------------------");
   Text_IO.Put_Line ("Testing Relative URL Resolution - RFC 2557 Section 5.2");

   declare
      Base : constant String := "http://a/b/c/d;p?q=q";
   begin
      --  '=' were added to query strings because AWS.Parameters cannot handle
      --  arbitrary query strings.
      Text_IO.New_Line;
      Text_IO.Put_Line ("Base: " & Base);
      Text_IO.Put_Line ("----------------------");
      Text_IO.New_Line;
      Text_IO.Put_Line ("Normal Examples:");
      Test_Relative_Resolution (Base, "g:h",        "g:h");
      Test_Relative_Resolution (Base, "g",          "http://a/b/c/g");
      Test_Relative_Resolution (Base, "./g",        "http://a/b/c/g");
      Test_Relative_Resolution (Base, "g/",         "http://a/b/c/g/");
      Test_Relative_Resolution (Base, "/g",         "http://a/g");
      --  Test_Relative_Resolution (Base, "//g",        "http://g");
      Test_Relative_Resolution (Base, "//g/",       "http://g/");
      Test_Relative_Resolution (Base, "?y=y",       "http://a/b/c/d;p?y=y");
      Test_Relative_Resolution (Base, "g?y=y",      "http://a/b/c/g?y=y");
      Test_Relative_Resolution (Base, "#s",         "http://a/b/c/d;p?q=q#s");
      Test_Relative_Resolution (Base, "g#s",        "http://a/b/c/g#s");
      Test_Relative_Resolution (Base, "g?y=y#s",    "http://a/b/c/g?y=y#s");
      Test_Relative_Resolution (Base, ";x",         "http://a/b/c/;x");
      Test_Relative_Resolution (Base, "g;x",        "http://a/b/c/g;x");
      Test_Relative_Resolution (Base, "g;x?y=y#s",  "http://a/b/c/g;x?y=y#s");
      Test_Relative_Resolution (Base, "",           "http://a/b/c/d;p?q=q");
      Test_Relative_Resolution (Base, ".",          "http://a/b/c/");
      Test_Relative_Resolution (Base, "./",         "http://a/b/c/");
      Test_Relative_Resolution (Base, "..",         "http://a/b/");
      Test_Relative_Resolution (Base, "../",        "http://a/b/");
      Test_Relative_Resolution (Base, "../g",       "http://a/b/g");
      Test_Relative_Resolution (Base, "../..",      "http://a/");
      Test_Relative_Resolution (Base, "../../",     "http://a/");
      Test_Relative_Resolution (Base, "../../g",    "http://a/g");
      Text_IO.New_Line;
      Text_IO.Put_Line ("Too much '..':");
      Test_Relative_Resolution (Base, "../../../g",    "http://a/g");
      Test_Relative_Resolution (Base, "../../../../g", "http://a/g");
      Text_IO.New_Line;
      Text_IO.Put_Line ("Do not remove '.' and '..' in path components:");
      Test_Relative_Resolution (Base, "/./g",       "http://a/g");
      Test_Relative_Resolution (Base, "/../g",      "http://a/g");
      Test_Relative_Resolution (Base, "g.",         "http://a/b/c/g.");
      Test_Relative_Resolution (Base, ".g",         "http://a/b/c/.g");
      Test_Relative_Resolution (Base, "g..",        "http://a/b/c/g..");
      Test_Relative_Resolution (Base, "..g",        "http://a/b/c/..g");
      Text_IO.New_Line;
      Text_IO.Put_Line ("Unnecessary '.' and '..':");
      Test_Relative_Resolution (Base, "./../g",     "http://a/b/g");
      Test_Relative_Resolution (Base, "./g/.",      "http://a/b/c/g/");
      Test_Relative_Resolution (Base, "g/./h",      "http://a/b/c/g/h");
      Test_Relative_Resolution (Base, "g/../h",     "http://a/b/c/h");
      Test_Relative_Resolution (Base, "g;x=1/./y",  "http://a/b/c/g;x=1/y");
      Test_Relative_Resolution (Base, "g;x=1/../y", "http://a/b/c/y");
      Text_IO.New_Line;
      Text_IO.Put_Line ("Separate query and fragment:");
      Test_Relative_Resolution (Base, "g?y=/./x",   "http://a/b/c/g?y=/./x");
      Test_Relative_Resolution (Base, "g?y=/../x",  "http://a/b/c/g?y=/../x");
      Test_Relative_Resolution (Base, "g#s/./x",    "http://a/b/c/g#s/./x");
      Test_Relative_Resolution (Base, "g#s/../x",   "http://a/b/c/g#s/../x");
      Text_IO.New_Line;
      Text_IO.Put_Line ("No scheme in relative URI (strict):");
      Test_Relative_Resolution (Base, "http:g",     "http:g");
   end;
end URL_Object;
