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

with AWS.Dispatchers.Stacks;
with AWS.Response;
with AWS.Status;

package Pages is

   type First_Page is new AWS.Dispatchers.Stacks.Dispatch_Item_Interface
     with null record;
   overriding function Callback (Object : in out First_Page;
                                 Request : AWS.Status.Data)
                                return AWS.Response.Data;

   type Second_Page is new AWS.Dispatchers.Stacks.Dispatch_Item_Interface with
      record
         Call_Number : Natural := 0;
   end record;
   overriding function Callback (Object : in out Second_Page;
                                 Request : AWS.Status.Data)
                                return AWS.Response.Data;

end Pages;
