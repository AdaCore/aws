------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                            Copyright (C) 2000                            --
--                Dmitriy Anisimkov, Sune Falck, Pascal Obry                --
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

package AWS.MIME is

   Text_HTML         : aliased constant String := "text/html";
   Text_Plain        : aliased constant String := "text/plain";

   Image_Gif         : aliased constant String := "image/gif";
   Image_Jpeg        : aliased constant String := "image/jpeg";
   Image_Png         : aliased constant String := "image/png";

   Appl_Postscript   : aliased constant String := "application/postscript";
   Appl_Pdf          : aliased constant String := "application/pdf";
   Appl_Zip          : aliased constant String := "application/zip";
   Appl_Octet_Stream : aliased constant String := "application/octet-stream";

   function Content_Type (Filename : in String) return String;
   --  Determine the MIME Content Type from the file's type extension.
   --  Returns "application/octet-stream" if the file type is unknown.

end AWS.MIME;
