------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2001                          --
--                                ACT-Europe                                --
--                                                                          --
--  Authors: Dmitriy Anisimkov - Pascal Obry                                --
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

--  $Id$

--  Dispatch on a Callback procedure.

with AWS.Dispatchers;
with AWS.Response;
with AWS.Status;

package AWS.Dispatchers.Callback is

   type Handler is new Dispatchers.Handler with private;
   --  This is a simple wrapper around standard callback procedure (access to
   --  function). It will be used to build dispatchers services and for the
   --  main server callback.

   function Create
     (Callback : in Response.Callback)
     return Handler;
   pragma Inline (Create);
   --  Build a dispatcher for the specified callback.

   function Dispatch
     (Dispatcher : in Handler;
      Request    : in Status.Data)
     return Response.Data;

private

   type Handler is new AWS.Dispatchers.Handler with record
      Callback : Response.Callback;
   end record;

end AWS.Dispatchers.Callback;
