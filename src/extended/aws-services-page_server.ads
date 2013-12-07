------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2013, AdaCore                     --
--                                                                          --
--  This library is free software;  you can redistribute it and/or modify   --
--  it under terms of the  GNU General Public License  as published by the  --
--  Free Software  Foundation;  either version 3,  or (at your  option) any --
--  later version. This library is distributed in the hope that it will be  --
--  useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                    --
--                                                                          --
--  As a special exception under Section 7 of GPL version 3, you are        --
--  granted additional permissions described in the GCC Runtime Library     --
--  Exception, version 3.1, as published by the Free Software Foundation.   --
--                                                                          --
--  You should have received a copy of the GNU General Public License and   --
--  a copy of the GCC Runtime Library Exception along with this program;    --
--  see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see   --
--  <http://www.gnu.org/licenses/>.                                         --
--                                                                          --
--  As a special exception, if other files instantiate generics from this   --
--  unit, or you link this unit with other files to produce an executable,  --
--  this  unit  does not  by itself cause  the resulting executable to be   --
--  covered by the GNU General Public License. This exception does not      --
--  however invalidate any other reasons why the executable file  might be  --
--  covered by the  GNU Public License.                                     --
------------------------------------------------------------------------------

--  The Callback is an implementation of a simple static Web page server. It
--  will return the Web pages found in the Web server directory. If directory
--  browsing is activated, it will be possible to browse directory content if
--  the requested resource is a directory. There is two specials files that
--  are recognized:
--
--    404.thtml              The Web page returned if the requested page is
--                           not found. This is a template with a single tag
--                           variable named PAGE. It will be replaced by the
--                           resource which was not found.
--
--                           Note that on Microsoft IE this page will be
--                           displayed only if the total page size is bigger
--                           than 512 bytes or it includes at least one
--                           image.
--
--    aws_directory.thtml    The template page used for directory browsing.
--                           See AWS.Services.Directory for a full description
--                           of this template usage.

with AWS.Messages;
with AWS.Response;
with AWS.Status;

package AWS.Services.Page_Server is

   procedure Directory_Browsing (Activated : Boolean);
   --  If Activated is set to True the directory browsing facility will be
   --  activated. By default this feature is not activated.

   procedure Set_Cache_Control (Data : Messages.Cache_Data);
   --  Set the Cache-Control header for each response given by the following
   --  callback.

   function Callback (Request : Status.Data) return Response.Data;
   --  This is the AWS callback for the simple static Web pages server

end AWS.Services.Page_Server;
