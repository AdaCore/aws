------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2008, AdaCore                          --
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

function AWS.Status.Translate_Set
  (Status : in Data) return Templates.Translate_Set
is
   use Templates;
   Tr : Templates.Translate_Set;
begin
   Insert (Tr, Assoc ("PEERNAME",     To_String (Status.Peername)));
   Insert (Tr, Assoc ("METHOD",       Request_Method'Image (Status.Method)));
   Insert (Tr, Assoc ("URI",          URL.URL (Status.URI)));
   Insert (Tr, Assoc ("HTTP_VERSION", To_String (Status.HTTP_Version)));
   Insert (Tr, Assoc ("AUTH_MODE",
                     Authorization_Type'Image (Status.Auth_Mode)));
   Insert (Tr, Assoc ("SOAP_ACTION",  Status.SOAP_Action));
   Insert (Tr, Assoc ("PAYLOAD",      AWS.Status.Payload (Status)));

   return Tr;
end AWS.Status.Translate_Set;
