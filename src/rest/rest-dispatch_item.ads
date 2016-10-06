------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2016, CNRS                         --
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

pragma Ada_2012;

with AWS.Dispatchers.Stacks;
with AWS.Response;
with AWS.Status;

package REST.Dispatch_Item is

   type REST_Item is abstract
     new AWS.Dispatchers.Stacks.Dispatch_Item_Interface with private;
   overriding function Callback (Object : in out REST_Item;
                                 Request : AWS.Status.Data)
                                return AWS.Response.Data;

   function GET (Object : REST_Item;
                 Request : AWS.Status.Data)
                return AWS.Response.Data is abstract;
   --  GET : Get a specific resource using an id.
   --  One can also get a collection of resource if no id is provided.
   function PUT (Object : in out REST_Item;
                 Request : AWS.Status.Data)
                return AWS.Response.Data is abstract;
   --  PUT : Update a specific resource using an id.
   --  Collection of resources can also be updated.
   --  If the resource doesn't exist and the id is known it will be created.
   function DELETE (Object : in out REST_Item;
                    Request : AWS.Status.Data)
                   return AWS.Response.Data is abstract;
   --  DELETE : Delete a specific resource using an identifier.
   --  The collection can be deleted if no id is given.
   function POST (Object : in out REST_Item;
                  Request : AWS.Status.Data)
                 return AWS.Response.Data is abstract;
   --  POST : Create a new resource.
   --  Can be used for all operations that don't fit into the other categories.

private

   type REST_Item is abstract
     new AWS.Dispatchers.Stacks.Dispatch_Item_Interface with null record;

end REST.Dispatch_Item;
