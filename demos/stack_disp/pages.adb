------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2016, CNRS                         --
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
with AWS.MIME;

package body Pages is

   Call_Number : Natural := 0;

   overriding function Callback (Object : First_Page;
                                 Request : AWS.Status.Data)
                                return AWS.Response.Data is
      pragma Unreferenced (Object);
      URI : constant String := AWS.Status.URI (Request);
   begin
      if URI = "/" then
         return AWS.Response.Build (AWS.MIME.Text_HTML,
                                    "<p>Very basic page - only handle / URI");
      else
         return AWS.Response.Acknowledge
           (AWS.Messages.S404,
            "<p>Request not handled by callback</p>");
      end if;
   end Callback;

   overriding function Callback (Object : Second_Page;
                                 Request : AWS.Status.Data)
                                return AWS.Response.Data is
      URI : constant String := AWS.Status.URI (Request);
      pragma Unreferenced (Object);
   begin
      Call_Number := Call_Number + 1;
      return AWS.Response.Build (AWS.MIME.Text_HTML,
                                 "<p>Very basic page - your URI :<b>" & URI
                                   & "</b> call #" & Call_Number'Img);
   end Callback;

end Pages;
