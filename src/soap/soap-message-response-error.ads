------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2015, AdaCore                     --
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

package SOAP.Message.Response.Error is

   type Object is new Message.Response.Object with private;

   type Faultcode is new String;

   overriding function From (P : Message.Payload.Object) return Object;
   --  Build an Error response from a Payload object

   overriding function XML_Image
     (E      : Object;
      Schema : WSDL.Schema.Definition :=
                 WSDL.Schema.Empty) return Unbounded_String;
   --  Returns the Fault env and associated data (faultcode, faultstring...)

   function Build
     (Faultcode   : Error.Faultcode;
      Faultstring : String)
      return Object;
   --  Returns an Error object built using Faultcode and Faultstring

   overriding function Is_Error (E : Object) return Boolean;
   --  Always returns True. This overrides  Response.Object's method

   -----------------
   -- Fault Codes --
   -----------------

   function Version_Mismatch (Subname : String := "") return Faultcode;
   --  Returns the Version_Mismatch faultcode

   function Must_Understand (Subname : String := "") return Faultcode;
   --  Returns the Must_Understand faultcode

   function Client (Subname : String := "") return Faultcode;
   --  Returns the Client faultcode

   function Server (Subname : String := "") return Faultcode;
   --  Returns the Server faultcode

private

   type Object is new Message.Response.Object with null record;

end SOAP.Message.Response.Error;
