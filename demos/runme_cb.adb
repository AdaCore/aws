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

      if C > 5 then
         AWS.Session.Set (Session_ID, "another", "string value");
      end if;

      if URI = "/first_img" then
         return AWS.Response.File (Content_Type => "image/png",
                                   Filename     => "adains.png");

      elsif URI = "/last" then
         return AWS.Response.Build
           (Content_Type => "text/html",
            Message_Body =>
              "<p>Ok, that's the end of it for now!"
            & "<p>filename = "
            &  AWS.Status.Parameter (Request, "filename")
            & "<p>filename_opt = "
            &  AWS.Status.Parameter (Request, "filename_opt")
            & "<p>color = "
            &  AWS.Status.Parameter (Request, "color")
            & "<p>size = "
            &  AWS.Status.Parameter (Request, "size")
            & "<p>go = "
            &  AWS.Status.Parameter (Request, "go")
            & "<p>length = "
            &  Positive'Image (AWS.Status.Content_Length (Request)));

      elsif URI = "/upload" then
         return AWS.Response.Build
           (Content_Type => "text/html",
            Message_Body =>
              "<p>Let's look at the form data:"
            & "<p>Your name is " & AWS.Status.Parameter (Request, "name")
            & "<p>parameter name (1) = "
            & AWS.Status.Parameter_Name (Request, 1)
            & "<p>parameter value (1) = "
            & AWS.Status.Parameter (Request, 1)
            & "<p>parameter name (2) = "
            & AWS.Status.Parameter_Name (Request, 2)
            & "<p>parameter value (2) = "
            & AWS.Status.Parameter (Request, 2)
            & "<p>There was " & Natural'Image (C)
            & " requests to the server."
            & "<form enctype=""multipart/form-data"" action=/last"
            & " method=post>"
            & "<input type=text name=color value=Color> <br>"
            & "<input type=text name=size value=10> <br>"
            & "<br>File to process: <input name=filename type=file>"
            & "<br><input type=submit name=go value=""Send File"">"
            & "<br>File to process: <input name=filename_opt type=file>"
            & "</form>");

      elsif URI = "/get-form" then
         return AWS.Response.Build
           (Content_Type => "text/html",
            Message_Body => "<p>Hello again "
            & AWS.Status.Parameter (Request, "name") & " !"
            & "<p>You have selected "
            & Natural'Image (AWS.Status.Count (Request, "OS"))
            & " OS."
            & "<p>Your first two OS are "
            & AWS.Status.Parameter (Request, "OS")
            & ", " & AWS.Status.Parameter (Request, "OS", 2)
            & "<p>I'll now check for the POST form, counter="
            & Natural'Image (C)
            & "<p>Enter your name <form method=post action=/upload>"
            & "<input type=text name=name value=""[default]"" size=15>"
            & "<input type=submit name=go value=""This is a POST Form"">"
            & "</form>");

      else
         return AWS.Response.Build
           (Content_Type => "text/html",
            Message_Body => "<p>Hello " & AWS.Status.Peername (Request)
            & ", I'm glad you ask for " & URI & '.'
            & "<p>I'm the runme demo. Note that this"
            & " message could have been"
            & " fetched on my file system... See Templates_Parser :)"
            & "<p>counter = " & Natural'Image (C)
            & "<p><img src=""/first_img"">"
            & "<p>Enter your name <form method=get action=/get-form>"
            & "<input type=text name=name value=""<default>"" size=15>"
            & "<br>"
            & "<select name=OS multiple size=5>"
            & "<option value=BeOS>BeOS"
            & "<option value=Linux>Linux"
            & "<option value=MacOS10>MacOS 10"
            & "<option value=Solaris>Solaris"
            & "<option value=Windows>Windows"
            & "</select>"
            & "<input type=submit name=go value=""This is a GET Form"">"
            & "</form>");
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
         return AWS.Response.File (Content_Type => "image/png",
                                   Filename     => "adains.png");

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
            & "<input type=submit name=go value=""This is a POST Form"">"
            & "</form>");

      else
         return AWS.Response.Build
           (Content_Type => "text/html",
            Message_Body => "<p>Hello, I'm glad you ask for " & URI & '.'
            & "<p>I'm the runme demo. Note that this"
            & " message could have been"
            & " fetched on my file system..."
            & "<p>This is a secure connection as shown by your browser's icon"
            & "<p><img src=""/first_img"">"
            & "<p>Enter your name <form method=get action=/get-form>"
            & "<input type=text name=name value=""<default>"" size=15>"
            & "<input type=submit name=go value=""This is a GET Form"">"
            & "</form>");
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

   function Service (Request : in AWS.Status.Data)
     return AWS.Response.Data is
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

   -----------------
   -- Service_Sec --
   -----------------

   function Service_Sec (Request : in AWS.Status.Data)
     return AWS.Response.Data is
      use type AWS.Status.Request_Method;
   begin
      if AWS.Status.Method (Request) = AWS.Status.GET
        or else AWS.Status.Method (Request) = AWS.Status.POST
      then
         return Get_No_Session (Request);

      elsif AWS.Status.Method (Request) = AWS.Status.PUT then
         return Put (Request);
      end if;
   end Service_Sec;

end Runme_CB;
