------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2003                          --
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

package AWS.MIME is

   --  Some content type constants. All of them will be defined into this
   --  package and associated with the right extensions. It is possible to
   --  add new MIME types with the routines below or by placing a file named
   --  aws.mime into the startup directory.
   --
   --  A MIME type is written in two parts: type/format

   ----------
   -- Text --
   ----------

   Text_CSS                    : constant String := "text/css";
   Text_HTML                   : constant String := "text/html";
   Text_Plain                  : constant String := "text/plain";
   Text_XML                    : constant String := "text/xml";
   Text_X_SGML                 : constant String := "text/x-sgml";

   -----------
   -- Image --
   -----------

   Image_Gif                   : constant String := "image/gif";
   Image_Jpeg                  : constant String := "image/jpeg";
   Image_Png                   : constant String := "image/png";
   Image_Tiff                  : constant String := "image/tiff";
   Image_X_Portable_Anymap     : constant String := "image/x-portable-anymap";
   Image_X_Portable_Bitmap     : constant String := "image/x-portable-bitmap";
   Image_X_Portable_Graymap    : constant String := "image/x-portable-graymap";
   Image_X_Portable_Pixmap     : constant String := "image/x-portable-pixmap";
   Image_X_RGB                 : constant String := "image/x-rgb";
   Image_X_Xbitmap             : constant String := "image/x-xbitmap";
   Image_X_Xpixmap             : constant String := "image/x-xpixmap";
   Image_X_Xwindowdump         : constant String := "image/x-xwindowdump";

   -----------------
   -- Application --
   -----------------

   Application_Postscript      : constant String := "application/postscript";
   Application_Pdf             : constant String := "application/pdf";
   Application_Zip             : constant String := "application/zip";
   Application_Octet_Stream    : constant String := "application/octet-stream";
   Application_Form_Data       : constant String
     := "application/x-www-form-urlencoded";
   Application_Mac_Binhex40    : constant String := "application/mac-binhex40";
   Application_Msword          : constant String := "application/msword";
   Application_Powerpoint      : constant String := "application/powerpoint";
   Application_Rtf             : constant String := "application/rtf";
   Application_X_Compress      : constant String := "application/x-compress";
   Application_X_GTar          : constant String := "application/x-gtar";
   Application_X_GZip          : constant String := "application/x-gzip";
   Application_X_Latex         : constant String := "application/x-latex";
   Application_X_Sh            : constant String := "application/x-sh";
   Application_X_Shar          : constant String := "application/x-shar";
   Application_X_Tar           : constant String := "application/x-tar";
   Application_X_Tcl           : constant String := "application/x-tcl";
   Application_X_Tex           : constant String := "application/x-tex";
   Application_X_Texinfo       : constant String := "application/x-texinfo";
   Application_X_Troff         : constant String := "application/x-troff";
   Application_X_Troff_Man     : constant String := "application/x-troff-man";

   -----------
   -- Audio --
   -----------

   Audio_Basic                 : constant String := "audio/basic";
   Audio_Mpeg                  : constant String := "audio/mpeg";
   Audio_X_Wav                 : constant String := "audio/x-wav";
   Audio_X_Pn_Realaudio        : constant String := "audio/x-pn-realaudio";
   Audio_X_Pn_Realaudio_Plugin : constant String
     := "audio/x-pn-realaudio-plugin";
   Audio_X_Realaudio           : constant String := "audio/x-realaudio";

   -----------
   -- Video --
   -----------

   Video_Mpeg                  : constant String := "video/mpeg";
   Video_Quicktime             : constant String := "video/quicktime";
   Video_X_Msvideo             : constant String := "video/x-msvideo";

   ---------------
   -- Multipart --
   ---------------

   Multipart_Form_Data         : constant String := "multipart/form-data";
   Multipart_X_Mixed_Replace   : constant String
     := "multipart/x-mixed-replace";

   -------------
   -- Setting --
   -------------

   procedure Add_Extension (Ext : in String; MIME_Type : in String);
   --  Add extension Ext (file extension without the dot, e.g. "txt") to the
   --  set of MIME type extension handled by this API. Ext will be mapped to
   --  the MIME_Type string.

   procedure Add_Regexp (Filename : in String; MIME_Type : in String);
   --  Add a specific rule to the MIME type table. Filename is a regular
   --  expression and will be mapped to the MIME_Type string.

   ---------------
   -- MIME Type --
   ---------------

   function Content_Type (Filename : in String) return String;
   --  Returns the MIME Content Type based on filename's extension or if not
   --  found the MIME Content type where Filename matches one of the specific
   --  rules set by Add_Regexp (see below).
   --  Returns "application/octet-stream" if the file type is unknown
   --  (i.e. no extension and no regular expression match filename).

   function Is_Text (MIME_Type : in String) return Boolean;
   --  Returns True if the MIME_Type is a text data

   function Is_Audio (MIME_Type : in String) return Boolean;
   --  Returns True if the MIME_Type is an audio data

   function Is_Image (MIME_Type : in String) return Boolean;
   --  Returns True if the MIME_Type is an image data

   function Is_Video (MIME_Type : in String) return Boolean;
   --  Returns True if the MIME_Type is a video data

   function Is_Application (MIME_Type : in String) return Boolean;
   --  Returns True if the MIME_Type is an application data

end AWS.MIME;
