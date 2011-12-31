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
