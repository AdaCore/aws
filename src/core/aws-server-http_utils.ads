------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2005-2015, AdaCore                     --
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

with Ada.IO_Exceptions;

with AWS.Resources;
with AWS.Response;
with AWS.Status;

package AWS.Server.HTTP_Utils is

   Name_Error   : exception renames Ada.IO_Exceptions.Name_Error;
   Device_Error : exception renames Ada.IO_Exceptions.Device_Error;

   Wrong_Request_Line : exception;
   --  Raised when a non well formed request line is detected

   procedure Answer_To_Client
     (HTTP_Server  : in out AWS.Server.HTTP;
      Line_Index   : Positive;
      C_Stat       : in out AWS.Status.Data;
      Socket_Taken : in out Boolean;
      Will_Close   : in out Boolean);
   --  This procedure use the C_Stat status data to build the correct answer
   --  to the client. If Force_Answer is not Empty it will be sent back to the
   --  client's browser, otherwise the answer will be retrieved from user's
   --  callback.

   procedure Get_Message_Data
     (HTTP_Server  : AWS.Server.HTTP;
      Line_Index   : Positive;
      C_Stat       : in out AWS.Status.Data;
      Expect_100   : Boolean);
   --  If the client sent us some data read them.
   --  This procedure fill in the C_Stat status data.

   procedure Get_Request_Line (C_Stat : in out AWS.Status.Data);
   --  Parse HTTP request line. This procedure fills in the C_Stat status
   --  data.

   procedure Parse_Request_Line
     (Command : String; C_Stat : in out AWS.Status.Data);
   --  Parse the request line:
   --  Request-Line = Method SP Request-URI SP HTTP-Version CRLF

   procedure Send
     (Answer       : in out Response.Data;
      HTTP_Server  : in out AWS.Server.HTTP;
      Line_Index   : Positive;
      C_Stat       : AWS.Status.Data;
      Socket_Taken : in out Boolean;
      Will_Close   : in out Boolean);
   --  Send Answer to the client's browser

   procedure Send_Resource
     (Answer      : in out Response.Data;
      Method      : Status.Request_Method;
      File        : in out Resources.File_Type;
      Length      : in out Resources.Content_Length_Type;
      HTTP_Server : AWS.Server.HTTP;
      Line_Index  : Positive;
      C_Stat      : AWS.Status.Data);
   --  Send the last header line Transfer-Encoding and Content_Length if
   --  necessary and send the file content. Length is the size of the
   --  resource/file as known before the call, Length returned value is the
   --  actual number of bytes sent. Close the resource only if Close is set to
   --  True.

   procedure Set_Close_Status
     (C_Stat     : AWS.Status.Data;
      Keep_Alive : Boolean;
      Will_Close : in out Boolean);
   --  Set Will_Close properly depending on the HTTP version and current
   --  request status. This routine must be called after Get_Message_header as
   --  the request header must have been parsed.

end AWS.Server.HTTP_Utils;
