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
with AWS.MIME;

package body WS_CB is

   use AWS;
   use GNAT;

   ---------
   -- Get --
   ---------

   function Get (Request : in AWS.Status.Data) return AWS.Response.Data is
      URI      : constant String := AWS.Status.URI (Request);
      Filename : constant String := URI (2 .. URI'Last);
   begin
      if URI = "/ref" then
         return AWS.Response.Moved
           (Location => "http://dieppe:1234/page1.html");

      elsif OS_Lib.Is_Regular_File (Filename) then
         return AWS.Response.File
           (Content_Type => AWS.MIME.Content_Type (Filename),
            Filename     => Filename);
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
