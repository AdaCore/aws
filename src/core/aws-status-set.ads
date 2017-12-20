------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2017, AdaCore                     --
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

--  This package is used when parsing the HTTP protocol from the client. It is
--  used to keep the values for the currently handled HTTP parameters.

package AWS.Status.Set is

   use type Net.Socket_Access;

   procedure Reset (D : in out Data) with
     Post => Socket (D) = null;
   --  Reset the status data for a new use

   procedure Connection_Data
     (D        : in out Data;
      Host     : String;
      Port     : Positive;
      Security : Boolean);
   --  Set the connection data as used by the server

   procedure Free (D : in out Data);
   --  Free all allocated memory

   procedure Read_Header (Socket : Net.Socket_Type'Class; D : in out Data);
   --  Read all header data from the socket and fill the appropriate
   --  data's fields.

   procedure Read_Body
     (Socket   : Net.Socket_Type'Class;
      D        : in out Data;
      Boundary : String := "");
   --  Read message body from the socket and fill the appropriate data's field

   procedure Append_Body
     (D      : in out Data;
      Buffer : Stream_Element_Array;
      Trim   : Boolean := False);
   --  Append data to the message body. If Trim is set to True the unused
   --  memory space in body's buffer is released. Ideally Trim should be
   --  set to True only when appending the last chunk of data.

   procedure Keep_Alive (D : in out Data; Flag : Boolean) with
     Post => Status.Keep_Alive (D) = Flag;
   --  Set the Keep-Alive flag for the current HTTP connection

   procedure Session (D : in out Data);
   --  Generate new Session ID

   procedure Session_Timed_Out (D : in out Data; Timed_Out : Boolean);
   --  Set to true when the session has timed out

   procedure Delete_Idle_Session (D : in out Data);
   --  If session just created and user callback has not used it to store data,
   --  this routine delete it. Need to avoid too many idle sessions created
   --  by the clients ignoring Set-Cookie header field.

   procedure Request
     (D            : in out Data;
      Method       : String;
      URI          : String;
      HTTP_Version : String);
   --  Set values for the request line:
   --
   --  GET URI[?parametrers] [HTTP/1.0 or HTTP/1.1]
   --  POST URI [HTTP/1.0 or HTTP/1.1]
   --
   --  Save the calendar time of the request.

   procedure Parameters (D : in out Data; Set : AWS.Parameters.List);
   --  Associate the parameters in Set to the status data

   procedure Parameters_From_Body (D : in out Data) with Inline;
   --  Get HTTP parameters from message body for POST form processing.
   --  This routine allow to move big message body into HTTP parameters set
   --  with low stack usage.

   procedure Case_Sensitive_Parameters (D : in out Data; Mode : Boolean)
     with Inline;

   procedure Add_Parameter
     (D           : in out Data;
      Name, Value : String;
      Decode      : Boolean := True;
      Replace     : Boolean := False)
   with Inline;
   --  Add or replace one parameter into the internal parameters list.
   --  The Name and Value should be already decoded.
   --  If Decode is true, decodes Name and Value. This is used when handling
   --  multipart/form-data for example. If Replace is True the paramater named
   --  Name will be set with Value (or added if not already present).

   procedure Add_Parameter
     (D           : in out Data;
      Name, Value : Unbounded_String;
      Decode      : Boolean := True;
      Replace     : Boolean := False)
   with Inline;
   --  The same as above but with Unbounded_String Name and Value

   procedure Add_Parameters (D : in out Data; Parameters : String)
     with Inline;
   --  Parse and add parameters into the internal parameters list

   procedure Query (D : in out Data; Parameters : String)
     with Inline;
   --  Query is a parameters only from request line (RFC-2616 3.2.2)

   procedure Binary (D : in out Data; Parameter : Stream_Element_Array) with
     Post => Binary_Size (D) = Parameter'Length;
   --  This procedure is used to store any binary data sent with the
   --  request. For example this will be used by the PUT method if a binary
   --  file is sent to the server.

   procedure Socket (D : in out Data; Sock : Net.Socket_Access) with
     Post => Status.Socket (D) = Sock;
   --  Set the Socket for the status. User callback can then retrieve the
   --  Socket for whatever it want. For example for passing it to the 'push'
   --  server.

   procedure Attachments
     (D : in out Data; Attachments : AWS.Attachments.List);
   --  Adds a list of Attachments to D

   procedure Authenticate
     (D                      : in out Data;
      Authorization_Mode     : Authorization_Type;
      Authorization_Name     : String;
      Authorization_Password : String);
   --  Set the authentication parameters associated with the request. This is
   --  mostly intended for automatic testsuite, since AWS will properly
   --  set those from the headers of the request as read from the socket.

   procedure Uploaded (D : in out Data);
   --  Server calls this on complete upload

end AWS.Status.Set;
