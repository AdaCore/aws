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

package body VH_Demo_CB is

   -----------
   -- H1_CB --
   -----------

   function H1_CB
     (Request : AWS.Status.Data)
     return AWS.Response.Data
   is
      P : AWS.Parameters.List := AWS.Status.Parameters (Request);
   begin
      return AWS.Response.Build
        ("text/html",
         "<p>This is server 1"
         & "<br>" & AWS.Parameters.Get (P, "PARAM"));
   end H1_CB;

   -----------
   -- H2_CB --
   -----------

   function H2_CB
     (Request : AWS.Status.Data)
     return AWS.Response.Data
   is
      P : AWS.Parameters.List := AWS.Status.Parameters (Request);
   begin
      return AWS.Response.Build
        ("text/html",
         "<p>This is server 2"
         & "<br>" & AWS.Parameters.Get (P, "PARAM"));
   end H2_CB;

end VH_Demo_CB;
