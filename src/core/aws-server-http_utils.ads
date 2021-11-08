------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2005-2021, AdaCore                     --
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

with Ada.Calendar;
with Ada.IO_Exceptions;
with Ada.Streams;

with AWS.Attachments;
with AWS.Headers;
with AWS.Resources;
with AWS.Response;
with AWS.Status;

package AWS.Server.HTTP_Utils is

   use Ada.Streams;

   Name_Error   : exception renames Ada.IO_Exceptions.Name_Error;
   Device_Error : exception renames Ada.IO_Exceptions.Device_Error;

   Wrong_Request_Line : exception;
   --  Raised when a non well formed request line is detected

   function Build_Answer
     (HTTP_Server : in out AWS.Server.HTTP;
      C_Stat      : in out AWS.Status.Data) return Response.Data;
   --  Build the Answer that should be sent to the client's browser

   function Call_For_Dispatcher
     (HTTP_Server : in out AWS.Server.HTTP;
      C_Stat      : AWS.Status.Data) return Response.Data;
   --  Call for the user defined dispatcher in server

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

   procedure Split_Path
     (Path                   : String;
      Path_Last, Query_First : out Positive);
   --  Splits path from query string if exists.
   --  If query part does not exist then Path_Last := Path'Last and
   --  Query_First := Path'Last + 1.

   procedure Send
     (Answer       : in out Response.Data;
      HTTP_Server  : in out AWS.Server.HTTP;
      Line_Index   : Positive;
      C_Stat       : AWS.Status.Data;
      Socket_Taken : in out Boolean;
      Will_Close   : in out Boolean);
   --  Send Answer to the client's browser

   procedure Log_Commit
     (HTTP_Server : in out AWS.Server.HTTP;
      Answer      : Response.Data;
      C_Stat      : AWS.Status.Data;
      Length      : Response.Content_Length_Type);
   --  Write log data into log file

   type Resource_Status is (Changed, Up_To_Date, Not_Found);

   function Get_Resource_Status
     (C_Stat    : Status.Data;
      Filename  : String;
      File_Time : out Ada.Calendar.Time) return Resource_Status;
   --  Get resource status

   generic
      with procedure Data
        (Content   : Stream_Element_Array;
         Next_Size : in out Stream_Element_Count);
   procedure Send_File_G
     (HTTP_Server : access AWS.Server.HTTP;
      Line_Index  : Positive;
      File        : in out Resources.File_Type;
      Start       : Stream_Element_Offset;
      Chunk_Size  : Stream_Element_Count;
      Length      : in out Resources.Content_Length_Type);

   procedure Parse_Content_Range
     (H_Value : String;
      Length  : Stream_Element_Offset;
      First   : out Stream_Element_Offset;
      Last    : out Stream_Element_Offset);
   --  Parse a Content-Ranges header value in H_Value and return the First
   --  and Last byte to be sent given the Length of the resource.

   generic
      with procedure Data
        (Content : Stream_Element_Array;
         Next_Size : in out Stream_Element_Count);
      with procedure Send_File
        (HTTP_Server : access AWS.Server.HTTP;
         Line_Index  : Positive;
         File        : in out Resources.File_Type;
         Start       : Stream_Element_Offset;
         Chunk_Size  : Stream_Element_Count;
         Length      : in out Resources.Content_Length_Type);
      Is_H2 : Boolean;
   procedure Send_File_Ranges_G
     (HTTP_Server : access AWS.Server.HTTP;
      Line_Index  : Positive;
      File        : in out Resources.File_Type;
      Ranges      : String;
      Chunk_Size  : Stream_Element_Count;
      Length      : in out Resources.Content_Length_Type;
      Answer      : in out Response.Data);

   procedure Send_Resource
     (Answer      : in out Response.Data;
      File        : in out Resources.File_Type;
      Length      : in out Resources.Content_Length_Type;
      HTTP_Server : access AWS.Server.HTTP;
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
   --  request status. This routine must be called after Get_Message_Header as
   --  the request header must have been parsed.

   generic
      Is_H2         : Boolean;
      Server_Config : AWS.Config.Object;
      with function Get_Line return String;
      with procedure Read (Buffer : out Stream_Element_Array);
      with procedure Read_Body (Stat : in out Status.Data; Boundary : String);
      with procedure Check_Data_Timeout;
      --  Check for data timeout, Server.Slots.Check_Data_Timeout (Index);
   package Multipart_Message_G is

      type Message_Mode is
        (Root_Attachment,   -- Read the root attachment
         Attachment,        -- Read an attachment
         File_Upload);      -- Read a file upload

      procedure File_Upload
        (C_Stat                       : in out Status.Data;
         Attachments                  : in out AWS.Attachments.List;
         Start_Boundary, End_Boundary : String;
         Parse_Boundary               : Boolean);
      --  Handle file upload data coming from the client browser

      function Get_File_Upload_UID return String;
      --  Returns a unique id for each file upload

      procedure Get_File_Data
        (C_Stat          : in out Status.Data;
         Attachments     : in out AWS.Attachments.List;
         Server_Filename : String;
         Filename        : String;
         Start_Boundary  : String;
         Mode            : Message_Mode;
         Headers         : AWS.Headers.List;
         End_Found       : out Boolean);
      --  Read file data from the stream, set End_Found if the end-boundary
      --  signature has been read. Server_Filename is the filename to be used
      --  for on-disk content (Attachment and File_Upload mode).

      procedure Store_Attachments
        (C_Stat                       : in out Status.Data;
         Attachments                  : in out AWS.Attachments.List;
         Start_Boundary, End_Boundary : String;
         Parse_Boundary               : Boolean;
         Multipart_Boundary           : String;
         Root_Part_CID                : String);
      --  Store attachments coming from the client browser

   end Multipart_Message_G;

end AWS.Server.HTTP_Utils;
