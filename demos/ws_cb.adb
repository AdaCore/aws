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

with GNAT.OS_Lib;

with AWS.Messages;

package body WS_CB is

   use AWS;
   use GNAT;

   ---------
   -- Get --
   ---------

   function Get (Request : in AWS.Status.Data) return AWS.Response.Data is

      function Is_Gif (Filename : in String) return Boolean;
      --  Returns True if Filename is a GIF file.

      URI      : constant String := AWS.Status.URI (Request);
      Filename : constant String := URI (2 .. URI'Last);

      function Is_Gif (Filename : in String) return Boolean is
      begin
         if Filename'Length > 4
           and then Filename (Filename'Last - 3 .. Filename'Last) = ".gif"
         then
            return True;
         else
            return False;
         end if;
      end Is_Gif;

   begin
      if OS_Lib.Is_Regular_File (Filename) then
         if Is_Gif (Filename) then

            return AWS.Response.File (Content_Type => "image/gif",
                                      Filename     => Filename);
         else
            --  just pretends that if it is not a GIF file it is an HTML file.

            return AWS.Response.File (Content_Type => "text/html",
                                      Filename     => Filename);
         end if;
      else

         return AWS.Response.Acknowledge
           (Messages.S404,
            "<p>Page '" & URI & "' Not found.");
      end if;
   end Get;

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
        or else AWS.Status.Method (Request) = AWS.Status.HEAD
      then
         return Get (Request);

      elsif AWS.Status.Method (Request) = AWS.Status.PUT then
         return Put (Request);

      else
         return AWS.Response.Acknowledge (Status_Code => Messages.S405);
      end if;
   end Service;

end WS_CB;
