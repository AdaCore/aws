------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2011-2012, AdaCore                     --
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

with Ada.Directories;

with AWS.MIME;
with AWS.Parameters;

package body Upload_CB is

   use Ada;

   -----------
   -- HW_CB --
   -----------

   function HW_CB (Request : Status.Data) return Response.Data is
      URI : constant String          := Status.URI (Request);
      P   : constant Parameters.List := Status.Parameters (Request);
   begin
      if URI = "/" then
         return Response.File (MIME.Text_HTML, "main.html");

      elsif URI = "/upload" then
         --  Rename uploaded file

         Directories.Rename
           (Parameters.Get (P, "filename"),
            Parameters.Get (P, "filename", 2));

         return Response.Build
           (MIME.Text_HTML,
            "<p>The file is now uploaded into the current directory:</p>"
              & "<p>" & Parameters.Get (P, "filename", 2));
      end if;
   end HW_CB;

end Upload_CB;
