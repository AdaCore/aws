------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2002                          --
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

--  This package is used when parsing the HTTP protocol from the client. It is
--  used to keep the values for the currently handled HTTP parameters.

package AWS.Status.Set is

   procedure Reset (D : in out Data);
   --  Reset the status data for a new use.

   procedure Free (D : in out Data);
   --  Free all allocated memory.

   procedure Read_Header (Socket : in Net.Socket_Type'Class; D : in out Data);
   --  Read all header data from the socket and fill the appropriate
   --  data's fields.

   procedure Keep_Alive
     (D    : in out Data;
      Flag : in     Boolean);
   --  Set the Keep-Alive flag for the current HTTP connection.

   procedure Session
     (D : in out Data);
   --  Generate new Session ID

   procedure Peername
     (D        : in out Data;
      Peername : in     String);
   --  Set peername field

   procedure Request
     (D            : in out Data;
      Method       : in     Request_Method;
      URI          : in     String;
      HTTP_Version : in     String);
   --  Set values for the request line:
   --
   --  GET URI[?parametrers] [HTTP/1.0 or HTTP/1.1]
   --  POST URI [HTTP/1.0 or HTTP/1.1]

   procedure Parameters (D : in out Data; Set : in AWS.Parameters.List);
   --  Associate the parameters in Set to the status data

   procedure Binary
     (D         : in out Data;
      Parameter : in     Stream_Element_Array);
   --  This procedure is used to store any binary data sent with the
   --  request. For example this will be used by the PUT method if a binary
   --  file is sent to the server.

   procedure Socket
     (D    : in out Data;
      Sock : in     Net.Socket_Access);
   --  Set the Socket for the status. User callback can then retreive the
   --  Socket for whatever it want. For example for passing it to the 'push'
   --  server.

   procedure Payload
     (D       : in out Data;
      Payload : in     String);
   --  Set the XML Payload message.

end AWS.Status.Set;
