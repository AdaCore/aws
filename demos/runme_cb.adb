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

with AWS.Messages;
with AWS.Session;

package body Runme_CB is

   ---------
   -- Get --
   ---------

   function Get (Request : in AWS.Status.Data) return AWS.Response.Data is
      URI : constant String := AWS.Status.URI (Request);

      Session_ID  : constant AWS.Session.ID := AWS.Status.Session (Request);
      C : Natural := 0;

   begin
      --  let's play with the Session now.

      if AWS.Session.Exist (Session_ID, "counter") then
         C := AWS.Session.Get (Session_ID, "counter");
      end if;

      C := C + 1;
      AWS.Session.Set (Session_ID, "counter", C);

      if URI = "/first_img" then
         return AWS.Response.File (Content_Type => "image/gif",
                                   Filename     => "adains.gif");

      elsif URI = "/last" then
         return AWS.Response.Build
           (Content_Type => "text/html",
            Message_Body =>
              "<p>Ok, that's the end of it for now!"
            & "<p>Your name is " & AWS.Status.Parameter (Request, "name")
            & "<p>parameter name (1) = "
            & AWS.Status.Parameter_Name (Request, 1)
            & "<p>parameter name (2) = "
            & AWS.Status.Parameter_Name (Request, 2)
            & "<p>There was " & Natural'Image (C)
            & " requests to the server.");

      elsif URI = "/get-form" then
         return AWS.Response.Build
           (Content_Type => "text/html",
            Message_Body => "<p>Hello again "
            & AWS.Status.Parameter (Request, "name") & " !"
            & "<p>I'll now check for the POST form, counter="
            & Natural'Image (C)
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
            & "<p>counter = " & Natural'Image (C)
            & "<p><img src=""/first_img"">"
            & "<p>Enter your name <form method=get action=/get-form>"
            & "<input type=text name=name value=""<default>"" size=15>"
            & "<input type=submit name=go value=""This is a GET Form"">");
      end if;
   end Get;

   --------------------
   -- Get_No_Session --
   --------------------

   function Get_No_Session
     (Request : in AWS.Status.Data) return AWS.Response.Data
   is
      URI : constant String := AWS.Status.URI (Request);

   begin
      if URI = "/first_img" then
         return AWS.Response.File (Content_Type => "image/gif",
                                   Filename     => "adains.gif");

      elsif URI = "/last" then
         return AWS.Response.Build
           (Content_Type => "text/html",
            Message_Body =>
              "<p>Ok, that's the end of it for now!"
            & "<p>Your name is " & AWS.Status.Parameter (Request, "name")
            & "<p>parameter name (1) = "
            & AWS.Status.Parameter_Name (Request, 1)
            & "<p>parameter name (2) = "
            & AWS.Status.Parameter_Name (Request, 2));

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
            & "<p>This is a secure connection as shown by your browser's icon"
            & "<p><img src=""/first_img"">"
            & "<p>Enter your name <form method=get action=/get-form>"
            & "<input type=text name=name value=""<default>"" size=15>"
            & "<input type=submit name=go value=""This is a GET Form"">");
      end if;
   end Get_No_Session;

   ---------
   -- Put --
   ---------

   function Put (Request : in AWS.Status.Data) return AWS.Response.Data is
   begin
      return AWS.Response.Acknowledge (Status_Code => AWS.Messages.S200);
   end Put;

   -------------
   -- Service --
   -------------

   function Service (Request : in AWS.Status.Data) return AWS.Response.Data is
      use type AWS.Status.Request_Method;
   begin
      if AWS.Status.Method (Request) = AWS.Status.GET
        or else AWS.Status.Method (Request) = AWS.Status.POST
      then
         return Get (Request);

      elsif AWS.Status.Method (Request) = AWS.Status.PUT then
         return Put (Request);
      end if;
   end Service;

   --------------
   -- Service2 --
   --------------

   function Service2 (Request : in AWS.Status.Data) return AWS.Response.Data is
      use type AWS.Status.Request_Method;
   begin
      if AWS.Status.Method (Request) = AWS.Status.GET
        or else AWS.Status.Method (Request) = AWS.Status.POST
      then
         return Get_No_Session (Request);

      elsif AWS.Status.Method (Request) = AWS.Status.PUT then
         return Put (Request);
      end if;
   end Service2;

end Runme_CB;
