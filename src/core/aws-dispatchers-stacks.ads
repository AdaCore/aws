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

with Ada.Containers.Indefinite_Vectors;

package AWS.Dispatchers.Stacks is

   type Dispatch_Item_Interface is interface;
   function Callback (Object : in out Dispatch_Item_Interface;
                      Request : AWS.Status.Data)
                     return AWS.Response.Data is abstract;
   Not_Handled : exception;

   package Dispatch_Item_Vectors is new Ada.Containers.Indefinite_Vectors
     (Positive, Dispatch_Item_Interface'Class, "=");

   type Handler is new AWS.Dispatchers.Handler with private;
   procedure Append_Distpatch_Item (Dispatcher : in out Handler;
                                    Item : Dispatch_Item_Interface'Class);
   overriding function Dispatch (Dispatcher : in out Handler;
                                 Request    : Status.Data)
                                return Response.Data;

private

   type Handler is new AWS.Dispatchers.Handler with record
      Stack : Dispatch_Item_Vectors.Vector;
   end record;
   overriding function Clone (Object : Handler) return Handler is (Object);

end AWS.Dispatchers.Stacks;