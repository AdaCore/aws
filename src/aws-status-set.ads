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

--  This package is used when parsing the HTTP protocol from the client. It is
--  used to keep the values for the currently handled HTTP parameters.

package AWS.Status.Set is

   procedure Reset (D : in out Data);
   --  Reset the status data for a new use.

   procedure Authorization (D             : in out Data;
                            Authorization : in     String);
   --  Set value for "Authorization:" parameter

   procedure Connection (D : in out Data; Connection : in String);
   --  Set value for "Connection:" parameter

   procedure Content_Length (D              : in out Data;
                             Content_Length : in     Natural);
   --  Set value for "Content-Length:" parameter

   procedure Content_Type (D            : in out Data;
                           Content_Type : in     String);
   --  Set value for "Content-Type:" parameter

   procedure Multipart_Boundary (D        : in out Data;
                                 Boundary : in     String);
   --  Set value for "Content-Type: ...; boundary=..." parameter

   procedure Session (D  : in out Data;
                      ID : in     String);
   --  Set Session to ID.

   procedure File_Up_To_Date (D               : in out Data;
                              File_Up_To_Date : in     Boolean);
   --  File_Up_To_Date is true if the file to be transfered is already
   --  up-to-date on the client side.

   procedure Host (D : in out Data; Host : in String);
   --  Set value for "Host:" parameter

   procedure If_Modified_Since (D                 : in out Data;
                                If_Modified_Since : in     String);
   --  Set value for "If-Modified-Since:" parameter

   procedure Peername (D        : in out Data;
                       Peername : in     String);
   --  Set peername field.

   procedure Request (D            : in out Data;
                      Method       : in     Request_Method;
                      URI          : in     String;
                      HTTP_Version : in     String;
                      Parameters   : in     String := "");
   --  Set values for the request line:
   --
   --  GET URI[?parametrers] [HTTP/1.0 or HTTP/1.1]
   --  POST URI [HTTP/1.0 or HTTP/1.1]
   --
   --  the parameters for a POST method are passed in the message body. See
   --  procedure below to set them afterward.

   procedure Parameters (D : in out Data; Name, Value : in String);
   --  Add one parameter into D. Value is associated to key Name.

   procedure Parameters (D : in out Data; Parameters : in String);
   --  Set parameters for the current request. This is used for a POST method
   --  because the parameters are found in the message body and are not known
   --  when we parse the request line. The Parameters string has the form
   --  "name1=value1&name2=value2...". The paramaters are added to the list.

   procedure Parameters (D         : in out Data;
                         Parameter : in     Stream_Element_Array);
   --  This procedure is used to store any binary data sent with the
   --  request. For example this will be used by the PUT method if a binary
   --  file is sent to the server.

end AWS.Status.Set;
