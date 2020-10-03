------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                    Copyright (C) 2018-2020, AdaCore                      --
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

with SOAP.Message.Payload;
with SOAP.Message.Response;

package SOAP.Client.Callback is

   --  Callback routine types
   type Pre_Call_CB is not null access
      procedure (Connection : AWS.Client.HTTP_Connection;
                 Request    : SOAP.Message.Payload.Object;
                 Schema     : SOAP.WSDL.Schema.Definition);

   type Post_Call_CB is not null access
      procedure (Connection : AWS.Client.HTTP_Connection;
                 Request    : SOAP.Message.Payload.Object;
                 Response   : SOAP.Message.Response.Object'Class;
                 Schema     : SOAP.WSDL.Schema.Definition);

   --  Default callback routines
   procedure Null_Pre_Call_Callback
      (Connection : AWS.Client.HTTP_Connection;
       Request    : SOAP.Message.Payload.Object;
       Schema     : SOAP.WSDL.Schema.Definition) is null;

   procedure Null_Post_Call_Callback
      (Connection : AWS.Client.HTTP_Connection;
       Request    : SOAP.Message.Payload.Object;
       Response   : SOAP.Message.Response.Object'Class;
       Schema     : SOAP.WSDL.Schema.Definition) is null;

end SOAP.Client.Callback;
