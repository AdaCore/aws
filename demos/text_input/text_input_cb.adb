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

with AWS.Parameters;

package body Text_Input_CB is

   -------------
   -- Text_CB --
   -------------

   function Text_CB
     (Request : AWS.Status.Data)
      return AWS.Response.Data
   is
      Text : constant String
        := AWS.Parameters.Get (AWS.Status.Parameters (Request), "text");
   begin
      if Text = "" then
         return AWS.Response.Build
           ("text/html", "<html><body>"
              & "<form>"
              & "<textarea rows=""7"" name=""text"" cols=""48""></textarea>"
              & "<br><input type=""Submit"">"
              & "</form></body></html>");
      end if;

      AWS.Log.Write (Text_Log, Request, Text);

      return AWS.Response.Build
        ("text/html", "<html><body>"
           & "<p>Thanks for you comment <br><pre>"
           & Text & "</pre>"
           & "<form>"
           & "<input type=""Submit"" value=""Back"">"
           & "</form></body></html>");
   end Text_CB;

end Text_Input_CB;
