------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                            Copyright (C) 2000                            --
--                               Pascal Obry                                --
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

package body Runme_CB is

   function Get (Request : in AWS.Status.Data) return AWS.Response.Data is
      URI : constant String := AWS.Status.URI (Request);
   begin
      --  I've been asked for the ressource Name via a GET message, let's for
      --  now only output a standard message

      if URI = "/first_img" then
         return AWS.Response.File (Content_Type => "image/gif",
                                   Filename     => "adains.gif");

      elsif URI = "/last" then
         return AWS.Response.Build
           (Content_Type => "text/html",
            Message_Body =>
              "<p>Ok, that's the end of it for now!"
            & "<p>Your name is " & AWS.Status.Parameter (Request, "name"));

      elsif URI = "/get-form" then
         return AWS.Response.Build
           (Content_Type => "text/html",
            Message_Body => "<p>Hello again "
            & AWS.Status.Parameter (Request, "name") & " !"
            & "<p>I'll now check for the POST form"
            & "<p>Enter your name <form method=post action=/last>"
            & "<input type=text name=name value=""<default>"" size=15>"
            & "<input type=submit name=go value=""This is a POST Form"">");

      else
         return AWS.Response.Build
           (Content_Type => "text/html",
            Message_Body => "<p>Hello, I'am glad you ask for " & URI & '.'
            & "<p>I'am the runme demo. Note that this"
            & " message could have been"
            & " fetched on my file system..."
            & "<p><img src=""/first_img"">"
            & "<p>Enter your name <form method=get action=/get-form>"
            & "<input type=text name=name value=""<default>"" size=15>"
            & "<input type=submit name=go value=""This is a GET Form"">");
      end if;
   end Get;

end Runme_CB;
