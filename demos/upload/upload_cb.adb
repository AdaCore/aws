------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2011-2017, AdaCore                     --
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
with Ada.Streams.Stream_IO;

with AWS.Messages;
with AWS.MIME;
with AWS.Parameters;
with AWS.Server;

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

      elsif URI = "/upload2" then
         --  URI to be used with a chunked encoded uploaded file:
         --  $ curl --upload-file file.txt \
         --    --header "Transfer-Encoding: chunked" \
         --    http://localhost:8080/upload2

         declare
            use Ada.Streams;
            File : Stream_IO.File_Type;
         begin
            Stream_IO.Create (File, Stream_IO.Out_File, "upload.txt");

            if not Status.Is_Body_Uploaded (Request) then
               --  For a chunked upload AWS won't have downloaded the body.
               --  This is up to the user to trigger it.
               AWS.Server.Get_Message_Body;
            end if;

            Write (Stream_IO.Stream (File).all, Status.Binary_Data (Request));

            Stream_IO.Close (File);
         end;

         return Response.Build
           (MIME.Text_Plain,
            "The file is now uploaded into the current directory: upload.txt");
      end if;

      return Response.Acknowledge (Status_Code => Messages.S404);
   end HW_CB;

end Upload_CB;
