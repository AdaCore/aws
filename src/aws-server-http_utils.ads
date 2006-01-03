------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                            Copyright (C) 2005                            --
--                                 AdaCore                                  --
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

with AWS.Server;
with AWS.Status;
with AWS.Parameters;
with AWS.Resources;
with AWS.Response;

package AWS.Server.HTTP_Utils is

   HTTP_10 : constant String := "HTTP/1.0";

   procedure Answer_To_Client
     (HTTP_Server  : in out AWS.Server.HTTP;
      Index        : in     Positive;
      C_Stat       : in out AWS.Status.Data;
      P_List       : in     AWS.Parameters.List;
      Socket_Taken : in out Boolean;
      Will_Close   : in out Boolean;
      Data_Sent    : in out Boolean);
   --  This procedure use the C_Stat status data to build the correct answer
   --  to the client. If Force_Answer is not Empty it will be sent back to the
   --  client's browser, otherwise the answer will be retrieved from user's
   --  callback.

   procedure Get_Message_Data
     (HTTP_Server            : in out AWS.Server.HTTP;
      Protocol_Handler_Index : in     Positive;
      C_Stat                 : in out AWS.Status.Data;
      P_List                 : in out AWS.Parameters.List);
   --  If the client sent us some data read them. Right now only the
   --  POST method is handled. This procedure fill in the C_Stat status
   --  data.

   procedure Get_Message_Header
     (HTTP_Server  : in     AWS.Server.HTTP;
      Index        : in     Positive;
      C_Stat       : in out AWS.Status.Data;
      P_List       : in out AWS.Parameters.List);
   --  Parse HTTP message header. This procedure fill in the C_Stat status
   --  data.

   function Is_Valid_HTTP_Date (HTTP_Date : in String) return Boolean;
   --  Check the date format as some Web brower seems to return invalid date
   --  field.

   procedure Parse_Request_Line
     (Command : in     String;
      C_Stat  : in out AWS.Status.Data;
      P_List  : in out AWS.Parameters.List);
   --  Parse the request line:
   --  Request-Line = Method SP Request-URI SP HTTP-Version CRLF

   procedure Send
     (Answer       : in out Response.Data;
      HTTP_Server  : in out AWS.Server.HTTP;
      Index        : in     Positive;
      C_Stat       : in     AWS.Status.Data;
      Socket_Taken : in out Boolean;
      Will_Close   : in out Boolean;
      Data_Sent    : in out Boolean);
   --  Send Answer to the client's browser

   procedure Send_Resource
     (Method      : in     Status.Request_Method;
      Close       : in     Boolean;
      File        : in out Resources.File_Type;
      Length      : in out Resources.Content_Length_Type;
      HTTP_Server : in     AWS.Server.HTTP;
      Index       : in     Positive;
      C_Stat      : in     AWS.Status.Data);
   --  Send the last header line Transfer-Encoding and Content_Length if
   --  necessary and send the file content. Length is the size of the
   --  resource/file as known before the call, Length returned value is the
   --  actual number of bytes sent. Close the resource only if Close is set to
   --  True.

   procedure Set_Close_Status
     (C_Stat     : in     AWS.Status.Data;
      Keep_Alive : in     Boolean;
      Will_Close : in out Boolean);
   --  Set Will_Close properly depending on the HTTP version and current
   --  request status. This routine must be called after Get_Message_header as
   --  the request header must have been parsed.

end AWS.Server.HTTP_Utils;
