------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2001                          --
--                                ACT-Europe                                --
--                                                                          --
--  Authors: Dmitriy Anisimov - Pascal Obry                                 --
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

with Ada.Strings.Unbounded;

with AWS.Parameters;

package body Hotplug_CB is

   use Ada.Strings.Unbounded;

   ----------
   -- Main --
   ----------

   function Main (Request : in AWS.Status.Data)
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

   -------------
   -- Hotplug --
   -------------

   function Hotplug (Request : in AWS.Status.Data)
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

end Hotplug_CB;
