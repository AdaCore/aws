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

with SOAP.Message.Payload;
with SOAP.Message.Response;

package SOAP.Message.Response.Error is

   type Object is new Message.Response.Object with private;

   type Faultcode is new String;

   function From (P : in Message.Payload.Object) return Object;
   --  Build an Error response from a Payload object.

   function XML_Image (E : in Object) return Unbounded_String;
   --  Returns the Fault env and associated data (faultcode, faultstring...).

   function Build
     (Faultcode   : in Error.Faultcode;
      Faultstring : in String)
     return Object;
   --  Returns an Error object built using Faultcode and Faultstring.

   function Is_Error (E : in Object) return Boolean;
   --  Always returns True. This overrides  Response.Object's method.

   -----------------
   -- Fault Codes --
   -----------------

   function Version_Mismatch (Subname : in String := "") return Faultcode;
   --  Returns the Version_Mismatch faultcode.

   function Must_Understand (Subname : in String := "") return Faultcode;
   --  Returns the Must_Understand faultcode.

   function Client (Subname : in String := "") return Faultcode;
   --  Returns the Client faultcode.

   function Server (Subname : in String := "") return Faultcode;
   --  Returns the Server faultcode.

private

   type Object is new Message.Response.Object with null record;

end SOAP.Message.Response.Error;
