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

with Ada.Strings.Unbounded;

package AWS.URL is

   type Object is private;

   URL_Error : exception;

   Default_HTTP_Port  : constant := 80;
   Default_HTTPS_Port : constant := 443;

   function Parse (URL : in String) return Object;
   --  Parse an URL and return an Object representing this URL. It is then
   --  possible to extract each part of the URL with the services bellow.
   --  An URL has the following form: http://server_name:port/uri. The port
   --  being optional and the protocol could be a secure HTTP (https://
   --  prefix).

   procedure Normalize (URL : in out Object);
   --  Removes all occurences to parent directory ".." and current
   --  directory '.'

   function URL (URL : in Object) return String;
   --  Returns full URL string, this can be different to the URL pased if it
   --  has been normalized.

   function Server_Name (URL : in Object) return String;
   --  Returns the server name.

   function Port (URL : in Object) return Positive;
   --  Returns the port as a positive.

   function Port (URL : in Object) return String;
   --  Returns the port as a string.

   function Security (URL : in Object) return Boolean;
   --  Returns True if it is an secure HTTP (HTTPS) URL.

   function URI (URL : in Object; Encode : in Boolean := False) return String;
   --  Returns the URI. If Encode is True then the URI will be encoded using
   --  the Encode routine.

   function Encode (URL : in String) return String;
   --  Encode URL. Many characters are forbiden into an URL and needs to be
   --  encoded. A character is encoded by %XY where XY is the character's
   --  ASCII hexadecimal code. For example a space is encoded as %20.

private

   use Ada.Strings.Unbounded;

   type Object is record
      Server_Name : Unbounded_String;
      Port        : Positive := Default_HTTP_Port;
      Security    : Boolean := False;
      URI         : Unbounded_String;
   end record;

end AWS.URL;
