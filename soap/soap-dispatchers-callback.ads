------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2003                          --
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

--  Dispatch on a Callback procedures.

package SOAP.Dispatchers.Callback is

   type Handler is new Dispatchers.Handler with private;
   --  This is a simple wrapper around standard callback procedure (access to
   --  function). It will be used to build dispatchers services and for the
   --  main server callback.

   type Callback_Routine is
      access function (Action  : in String;
                       Request : in Message.Payload.Object)
                       return  AWS.Response.Data;
   --  This is the SOAP Server Callback procedure.
   --  ??? We should move this type declaration somewhere upper,
   --  To the SOAP specification, or to the SOAP.Types.

   function Create
     (Base_Callback : in AWS.Response.Callback;
      SOAP_Callback : in Callback_Routine)
      return        Handler;
   pragma Inline (Create);
   --  Build a dispatcher for the specified callback.

private

   function Dispatch_SOAP
     (Dispatcher : in Handler;
      Action     : in String;
      Request    : in Message.Payload.Object)
      return     AWS.Response.Data;
   --  This dispatch function would call SOAP callback.

   function Dispatch_Base
     (Dispatcher : in Handler;
      Request    : in AWS.Status.Data)
      return     AWS.Response.Data;
   --  This dispatch function would call non SOAP callback.

   type Handler is new Dispatchers.Handler with record
      Base_Callback : AWS.Response.Callback;
      SOAP_Callback : Callback_Routine;
   end record;

end SOAP.Dispatchers.Callback;
