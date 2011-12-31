------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2012, AdaCore                     --
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

with Ada.Strings.Unbounded;

with AWS.Parameters;

package body Hotplug_CB is

   use Ada.Strings.Unbounded;

   -------------
   -- Hotplug --
   -------------

   function Hotplug (Request : AWS.Status.Data)
     return AWS.Response.Data
   is
      URI : constant String := AWS.Status.URI (Request);
      P   : constant AWS.Parameters.List := AWS.Status.Parameters (Request);

      Parameters : Unbounded_String;
   begin
      for K in 1 .. AWS.Parameters.Count (P) loop
         Append (Parameters, "<p>" & Positive'Image (K) & ") Name = "
                 & AWS.Parameters.Get_Name (P, K));
         Append (Parameters, " Value = " & AWS.Parameters.Get_Value (P, K));
      end loop;

      return AWS.Response.Build
        (Content_Type => "text/html",
         Message_Body =>
           "<p>This request is using method "
           & AWS.Status.Request_Method'Image (AWS.Status.Method (Request))
           & "<p>Ok, I'm the Hotplug server, you have asked for " & URI
           & "<p>Here are the parameters"
           & To_String (Parameters));
   end Hotplug;

   ----------
   -- Main --
   ----------

   function Main (Request : AWS.Status.Data)
     return AWS.Response.Data
   is
      URI : constant String := AWS.Status.URI (Request);
      P   : constant AWS.Parameters.List := AWS.Status.Parameters (Request);

      Parameters : Unbounded_String;
   begin
      for K in 1 .. AWS.Parameters.Count (P) loop
         Append (Parameters, "<p>" & Positive'Image (K) & ") Name = "
                 & AWS.Parameters.Get_Name (P, K));
         Append (Parameters, " Value = " & AWS.Parameters.Get_Value (P, K));
      end loop;

      return AWS.Response.Build
        (Content_Type => "text/html",
         Message_Body =>
           "<p>Ok, I'm the main server, you have asked for " & URI
           & "<p>Here are the parameters"
           & To_String (Parameters)
           & "<form action=""/AWS"" method=POST>"
           & "<input type=submit value=""Go POST request"">"
           & "<input type=hidden name=myvar1 value=myvalue1>"
           & "<input type=hidden name=myvar2 value=myvalue2></form>");
   end Main;

end Hotplug_CB;
