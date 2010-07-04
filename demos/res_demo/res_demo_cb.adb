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

package body Res_Demo_CB is

   -----------
   -- RD_CB --
   -----------

   function RD_CB (Request : AWS.Status.Data)
      return AWS.Response.Data
   is
      URI : constant String := AWS.Status.URI (Request);
   begin
      if URI = "/img" then
         return AWS.Response.File ("image/png", "adains.png");
      elsif URI = "/page" then
         return AWS.Response.File ("text/html", "page3.html");
      else
         return AWS.Response.Acknowledge (AWS.Messages.S404, "<p>Not found !");
      end if;
   end RD_CB;

end Res_Demo_CB;
