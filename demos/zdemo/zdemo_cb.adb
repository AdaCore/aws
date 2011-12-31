------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2003-2012, AdaCore                     --
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
with AWS.MIME;

package body Zdemo_CB is

   use AWS;

   --------
   -- CB --
   --------

   function CB (Request : Status.Data) return Response.Data is
      URI : constant String := Status.URI (Request);
   begin
      if URI = "/" then
         --  Main page, check if browser supports the GZIP encoding
         if Status.Is_Supported (Request, Encoding => Messages.GZip) then
            return Response.Build
              (MIME.Text_HTML,
               "<p>Your browser support GZIP encoding, "
                 & "<a href=""z_content"">click here</a> to continue.");
         else
            return Response.Build
              (MIME.Text_HTML,
               "<p>Your browser does not support GZIP encoding.");
         end if;

      elsif URI = "/z_content" then
         --  Returns a compressed message body (compressed by the server)
         return Response.Build
           (MIME.Text_HTML,
            "<p>This Web Page has been transfered compressed from the server "
              & " to your brower, and just below a compressed image has"
              & " been included"
              & "<p><img src=""/z_file"">"
              & "<p>That's all.",
            Encoding => Messages.GZip);

      elsif URI = "/z_file" then
         --  Returns a compressed file (already compressed on disk)
         return Response.File
           (Content_Type => MIME.Image_Png,
            Filename     => "adains.png.gz",
            Encoding     => Messages.GZip);

      else
         return Response.Build
           (MIME.Text_HTML, "<p>Page not found in this server");
      end if;
   end CB;

end Zdemo_CB;
