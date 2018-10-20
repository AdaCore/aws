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

with AWS.Messages;
with AWS.Parameters;
with AWS.Session;
with Ada.Streams;

package body Runme_CB is

   use AWS;

   function Get (Request : Status.Data) return Response.Data;
   --  Handle the Get requests

   function Get_No_Session
     (Request : Status.Data) return Response.Data;
   --  Handle the Get requests with no session

   function Put (Request : Status.Data) return Response.Data;
   --  Handle the Put requests

   ---------
   -- Get --
   ---------

   function Get (Request : Status.Data) return Response.Data is
      URI        : constant String          := Status.URI (Request);
      P_List     : constant Parameters.List := Status.Parameters (Request);
      Session_Id : constant Session.Id      := Status.Session (Request);
      C : Natural := 0;

   begin
      --  let's play with the Session now

      if Session.Exist (Session_Id, "counter") then
         C := Session.Get (Session_Id, "counter");
      end if;

      C := C + 1;
      Session.Set (Session_Id, "counter", C);

      if C > 5 then
         Session.Set (Session_Id, "another", "string value");
      end if;

      if URI = "/first_img" then
         return Response.File (Content_Type => "image/png",
                               Filename     => "adains.png");

      elsif URI = "/last" then
         return Response.Build
           (Content_Type => "text/html",
            Message_Body =>
              "<p>Ok, that's the end of it for now!"
            & "<p>filename = "
            &  Parameters.Get (P_List, "filename")
            & "<p>filename_opt = "
            &  Parameters.Get (P_List, "filename_opt")
            & "<p>color = "
            &  Parameters.Get (P_List, "color")
            & "<p>size = "
            &  Parameters.Get (P_List, "size")
            & "<p>go = "
            &  Parameters.Get (P_List, "go")
            & "<p>length = "
            &  Ada.Streams.Stream_Element_Offset'Image (Status.Content_Length (Request)));

      elsif URI = "/upload" then
         return Response.Build
           (Content_Type => "text/html",
            Message_Body =>
              "<p>Let's look at the form data:"
            & "<p>Your name is " & Parameters.Get (P_List, "name")
            & "<p>parameter name (1) = "
            & Parameters.Get_Name (P_List, 1)
            & "<p>parameter value (1) = "
            & Parameters.Get_Value (P_List, 1)
            & "<p>parameter name (2) = "
            & Parameters.Get_Name (P_List, 2)
            & "<p>parameter value (2) = "
            & Parameters.Get_Value (P_List, 2)
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
         return Response.Build
           (Content_Type => "text/html",
            Message_Body => "<p>Hello again "
            & Parameters.Get (P_List, "name") & " !"
            & "<p>You have selected "
            & Natural'Image (Parameters.Count (P_List, "OS"))
            & " OS."
            & "<p>Your first two OS are "
            & Parameters.Get (P_List, "OS")
            & ", " & Parameters.Get (P_List, "OS", 2)
            & "<p>I'll now check for the POST form, counter="
            & Natural'Image (C)
            & "<p>Enter your name <form method=post action=/upload>"
            & "<input type=text name=name value=""[default]"" size=15>"
            & "<input type=submit name=go value=""This is a POST Form"">"
            & "</form>");

      else
         return Response.Build
           (Content_Type => "text/html",
            Message_Body => "<p>Hello " & Status.Peername (Request)
            & ", I'm glad you ask for " & URI & '.'
            & "<p>I'm the runme demo. Note that this"
            & " message could have been"
            & " fetched on my file system..."
            & "See <a href=""http://libre.adacore.com/tools/aws/"">"
            & "Templates_Parser</a> :)"
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
     (Request : Status.Data) return Response.Data
   is
      URI    : constant String          := Status.URI (Request);
      P_List : constant Parameters.List := Status.Parameters (Request);

   begin
      if URI = "/first_img" then
         return Response.File (Content_Type => "image/png",
                               Filename     => "adains.png");

      elsif URI = "/last" then
         return Response.Build
           (Content_Type => "text/html",
            Message_Body =>
              "<p>Ok, that's the end of it for now!"
              & "<p>Your name is " & Parameters.Get (P_List, "name")
              & "<p>parameter name (1) = "
              & Parameters.Get_Name (P_List, 1)
              & "<p>parameter name (2) = "
              & Parameters.Get_Name (P_List, 2));

      elsif URI = "/get-form" then
         return Response.Build
           (Content_Type => "text/html",
            Message_Body => "<p>Hello again "
              & Parameters.Get (P_List, "name") & " !"
              & "<p>I'll now check for the POST form"
              & "<p>Enter your name <form method=post action=/last>"
              & "<input type=text name=name value=""<default>"" size=15>"
              & "<input type=submit name=go value=""This is a POST Form"">"
              & "</form>");

      else
         return Response.Build
           (Content_Type => "text/html",
            Message_Body => "<p>Hello, I'm glad you ask for " & URI & '.'
              & "<p>I'm the runme demo. Note that this"
              & " message could have been"
              & " fetched on my file system..."
              & "<p>This is a secure connection as shown by your browser's"
              & " icon"
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

   function Put (Request : Status.Data) return Response.Data is
      pragma Unreferenced (Request);
   begin
      return Response.Acknowledge (Status_Code => Messages.S200);
   end Put;

   -------------
   -- Service --
   -------------

   function Service (Request : Status.Data)
     return Response.Data is
      use type Status.Request_Method;
   begin
      if Status.Method (Request) = Status.GET
        or else Status.Method (Request) = Status.POST
      then
         return Get (Request);

      elsif Status.Method (Request) = Status.PUT then
         return Put (Request);

      else
         return Response.Build
           (Content_Type => "text/html",
            Message_Body => "<p>Unknown Request method");
      end if;
   end Service;

   -----------------
   -- Service_Sec --
   -----------------

   function Service_Sec (Request : Status.Data)
     return Response.Data is
      use type Status.Request_Method;
   begin
      if Status.Method (Request) = Status.GET
        or else Status.Method (Request) = Status.POST
      then
         return Get_No_Session (Request);

      elsif Status.Method (Request) = Status.PUT then
         return Put (Request);

      else
         return Response.Build
           (Content_Type => "text/html",
            Message_Body => "<p>Unknown Request method");
      end if;
   end Service_Sec;

end Runme_CB;
