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

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash_Case_Insensitive;

with AWS.Response;
with AWS.Status;

with REST.Dispatch_Item;

package REST_Example is

   --  Data accept size limit is 2048 Bytes
   --  collection are given as a string separated by ;
   --  = is used to separate value from key

   package String_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type => String,
      Element_Type => String,
      Hash => Ada.Strings.Hash_Case_Insensitive,
      Equivalent_Keys => "=",
      "=" => "=");

   type REST_Conf is new REST.Dispatch_Item.REST_Item with record
      Map : String_Maps.Map;
      Post_Call : Natural := 0;
   end record;
   function GET (Object : REST_Conf;
                 Request : AWS.Status.Data)
                return AWS.Response.Data;
   function PUT (Object : in out REST_Conf;
                 Request : AWS.Status.Data)
                return AWS.Response.Data;
   function DELETE (Object : in out REST_Conf;
                    Request : AWS.Status.Data)
                   return AWS.Response.Data;
   function POST (Object : in out REST_Conf;
                  Request : AWS.Status.Data)
                 return AWS.Response.Data;

end REST_Example;
