------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2004-2017, AdaCore                     --
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

--  Services to be used to declare aliases based on URI. This is mostly
--  designed to be used with AWS.services.Dispatchers.URI.

with AWS.Response;
with AWS.Status;

package AWS.Services.Callbacks is

   generic
      Prefix    : String; -- the prefix found in the URI
      Directory : String; -- the directory where the file is
   function File (Request : Status.Data) return Response.Data;
   --  This is a callback function where URL:
   --     http://<host>/<prefix>toto
   --  references the file:
   --     <directory>/toto
   --
   --  If the URL does not start with Prefix it returns a 404 error page.
   --  This is designed to be use with AWS.Services.Dispatchers.URI.

end AWS.Services.Callbacks;
