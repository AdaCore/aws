------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2001                          --
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

--  The Callback is an implementation of a simple static Web page server. It
--  will return the Web pages found in the Web server directory. If directory
--  browsing is activated, it will be possible to browse directory content if
--  the requested ressource is a directory. There is two specials files that
--  are recognized:
--
--    404.thtml              The Web page returned if the requested page is
--                           not found. This is a template with a single tag
--                           variable named PAGE. It will be replaced by the
--                           ressource which was not found.
--
--                           Note that on Microsoft IE this page will be
--                           displayed only if the total page size is bigger
--                           than 512 bytes or it includes at least one
--                           image.
--
--    aws_directory.thtml    The template page used for directory browsing.
--                           See AWS.Services.Directory for a full description
--                           of this template usage.

with AWS.Status;
with AWS.Response;

package AWS.Services.Page_Server is

   procedure Directory_Browsing (Activated : in Boolean);
   --  If Activated is set to True the directory browsing faciity will be
   --  activated. By default this feature is not activated.

   function Callback (Request : in AWS.Status.Data) return AWS.Response.Data;
   --  This is the AWS callback for the simple static Web pages server.

end AWS.Services.Page_Server;
