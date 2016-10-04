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

with SOAP.Dispatchers;
with SOAP.WSDL.Schema;

package SOAP.Dispatch_Item is

   type SOAP_Item is
     new AWS.Dispatchers.Stacks.Dispatch_Item_Interface with private;

   function Schema
     (Object     : SOAP_Item;
      SOAPAction : String)
     return WSDL.Schema.Definition;

   function Create (Callback : Dispatchers.SOAP_Callback)
                   return AWS.Dispatchers.Stacks.Dispatch_Item_Interface'Class;

private

   type SOAP_Item is
     new AWS.Dispatchers.Stacks.Dispatch_Item_Interface with record
      Schema : WSDL.Schema.Definition;
      SOAP_Callback : Dispatchers.SOAP_Callback;
   end record;
   overriding function Callback (Object : in out SOAP_Item;
                                 Request : AWS.Status.Data)
                                return AWS.Response.Data;

end SOAP.Dispatch_Item;
