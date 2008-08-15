------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2008, AdaCore                     --
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

--  This package is used when parsing the HTTP protocol from the client. It is
--  used to keep the values for the currently handled HTTP parameters.

package AWS.Status.Set is

   procedure Reset (D : in out Data);
   --  Reset the status data for a new use

   procedure Connection_Data
     (D        : in out Data;
      Host     : in     String;
      Port     : in     Positive;
      Security : in     Boolean);
   --  Set the connection data as used by the server

   procedure Free (D : in out Data);
   --  Free all allocated memory

   procedure Read_Header (Socket : in Net.Socket_Type'Class; D : in out Data);
   --  Read all header data from the socket and fill the appropriate
   --  data's fields.

   procedure Read_Body (Socket : in Net.Socket_Type'Class; D : in out Data);
   --  Read message body from the socket and fill the appropriate data's field

   procedure Append_Body
     (D      : in out Data;
      Buffer : in     Stream_Element_Array;
      Trim   : in     Boolean := False);
   --  Append data to the message body. Trim parameter is for memory
   --  optimization, on last data chunk, we should set Trim to True.

   procedure Keep_Alive (D : in out Data; Flag : in Boolean);
   --  Set the Keep-Alive flag for the current HTTP connection

   procedure Session (D : in out Data);
   --  Generate new Session ID

   procedure Request
     (D            : in out Data;
      Method       : in     String;
      URI          : in     String;
      HTTP_Version : in     String);
   --  Set values for the request line:
   --
   --  GET URI[?parametrers] [HTTP/1.0 or HTTP/1.1]
   --  POST URI [HTTP/1.0 or HTTP/1.1]
   --
   --  Save the calendar time of the request.

   procedure Parameters (D : in out Data; Set : in AWS.Parameters.List);
   --  Associate the parameters in Set to the status data

   procedure Case_Sensitive_Parameters (D : in out Data; Mode : in Boolean);
   pragma Inline (Case_Sensitive_Parameters);

   procedure Add_Parameter
     (D           : in out Data;
      Name, Value : in     String;
      Decode      : in     Boolean := True);
   pragma Inline (Add_Parameter);
   --  Add one parameter into the internal parameters list.
   --  The Name and Value should be already decoded.
   --  If Decode is true, decodes Name and Value. This is used when handling
   --  multipart/form-data for example.

   procedure Add_Parameters (D : in out Data; Parameters : in String);
   pragma Inline (Add_Parameters);
   --  Parse and add parameters into the internal parameters list

   procedure Binary (D : in out Data; Parameter : in Stream_Element_Array);
   --  This procedure is used to store any binary data sent with the
   --  request. For example this will be used by the PUT method if a binary
   --  file is sent to the server.

   procedure Socket (D : in out Data; Sock : in Net.Socket_Access);
   --  Set the Socket for the status. User callback can then retrieve the
   --  Socket for whatever it want. For example for passing it to the 'push'
   --  server.

   procedure Attachments
     (D : in out Data; Attachments : in AWS.Attachments.List);
   --  Adds a list of Attachments to D

   procedure Authenticate
     (D                      : in out Data;
      Authorization_Mode     : in     Authorization_Type;
      Authorization_Name     : in     String;
      Authorization_Password : in     String);
   --  Set the authentication parameters associated with the request. This is
   --  mostly intended for automatic testsuite, since AWS will properly
   --  set those from the headers of the request as read from the socket.

end AWS.Status.Set;
