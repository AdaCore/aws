------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2003-2009, AdaCore                     --
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
