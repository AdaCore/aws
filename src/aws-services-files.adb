------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                            Copyright (C) 2003                            --
--                                ACT-Europe                                --
--                                                                          --
--  Authors: Dmitriy Anisimkov - Pascal Obry                                --
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
with AWS.MIME;
with AWS.OS_Lib;
with AWS.Resources.Streams.ZLib;
with AWS.Resources.Streams.Disk;

package body AWS.Services.Files is

   use AWS;

   ----------
   -- Open --
   ----------

   function Open
     (Filename : in String;
      Request  : in Status.Data)
      return Response.Data
   is
      Z_Filename     : constant String := Filename & ".gz";
      GZip_Supported : constant Boolean
        := Status.Is_Supported (Request, Messages.GZip);
   begin
      if OS_Lib.Is_Regular_File (Z_Filename) and then GZip_Supported then
         --  Compressed file exists and GZip supported by client
         --  Note, that Content-Type should be of the non compressed file.

         return Response.File
           (MIME.Content_Type (Filename),
            Z_Filename,
            Encoding => Messages.GZip);

      elsif OS_Lib.Is_Regular_File (Filename) then
         --  File exits

         return Response.File (MIME.Content_Type (Filename), Filename);

      elsif OS_Lib.Is_Regular_File (Z_Filename) then
         --  Compressed file exists and GZip not supported by client

         declare
            subtype Stream_Type is Resources.Streams.ZLib.Stream_Type;

            Decoding_Stream : Resources.Streams.Stream_Access;
            In_Stream       : Resources.Streams.Stream_Access;
         begin
            --  Initialize the input stream

            In_Stream := new Resources.Streams.Disk.Stream_Type;

            Resources.Streams.Disk.Open
              (Resources.Streams.Disk.Stream_Type (In_Stream.all), Z_Filename);

            --  Bind the input stream to the decoding stream

            Decoding_Stream := new Stream_Type;

            Resources.Streams.ZLib.Inflate_Initialize
              (Stream_Type (Decoding_Stream.all), In_Stream);

            --  Stream will be decoded on-the-fly, the content type is the one
            --  of the original non-compressed file.

            return Response.Stream
              (MIME.Content_Type (Filename), Decoding_Stream);
         end;

      else
         return Response.Acknowledge (Messages.S404);
      end if;
   end Open;

end AWS.Services.Files;
