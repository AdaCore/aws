------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                            Copyright (C) 2000                            --
--                               Pascal Obry                                --
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

with AWS.Response;

package AWS.Client is

   URL_Error : exception;

   No_Data : constant String := "";

   function Get (URL        : in String;
                 User       : in String := No_Data;
                 Pwd        : in String := No_Data;
                 Proxy      : in String := No_Data;
                 Proxy_User : in String := No_Data;
                 Proxy_Pwd  : in String := No_Data) return Response.Data;
   --  retreive the message data given a specific URL. It open a connection
   --  with the server and ask for the ressource specified in the URL it then
   --  return it in the Response.Data structure.
   --  If User/Pwd are given then it uses it to access the URL.
   --
   --  Eventually it connect through a PROXY using if necessary the Proxy
   --  authentification Proxy_User:Proxy_Pwd.
   --
   --  Only Basic authetification is supported (i.e. Digest is not).

end AWS.Client;
