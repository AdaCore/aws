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

with Ada.Calendar;

package AWS.Messages is

   use Ada;

   HTTP_Token : constant String := "HTTP/";

   Authorization_Token : constant String := "Authorization: ";
   subtype Authorization_Range is Positive range Authorization_Token'Range;

   Connection_Token : constant String := "Connection: ";
   subtype Connection_Range is Positive range Connection_Token'Range;

   Content_Length_Token : constant String := "Content-Length: ";
   subtype Content_Length_Range is Positive range Content_Length_Token'Range;

   Content_Type_Token : constant String := "Content-Type: ";
   subtype Content_Type_Range is Positive range Content_Type_Token'Range;

   Get_Token : constant String := "GET ";
   subtype Get_Range is Positive range Get_Token'Range;

   Head_Token : constant String := "HEAD ";
   subtype Head_Range is Positive range Head_Token'Range;

   Host_Token : constant String := "Host: ";
   subtype Host_Range is Positive range Host_Token'Range;

   If_Modified_Since_Token : constant String := "If-Modified-Since: ";
   subtype If_Modified_Since_Range is
     Positive range If_Modified_Since_Token'Range;

   Location_Token : constant String := "Location: ";
   subtype Location_Range is Positive range Location_Token'Range;

   Post_Token : constant String := "POST ";
   subtype Post_Range is Positive range Post_Token'Range;

   Transfer_Encoding_Token : constant String := "Transfer-Encoding: ";
   subtype Transfer_Encoding_Range is
     Positive range Transfer_Encoding_Token'Range;

   Form_Data : constant String := "application/x-www-form-urlencoded";

   type Status_Code is
     (S100, S101,
      --  1xx : Informational - Request received, continuing process

      S200, S201, S202, S203, S204, S205, S206,
      --  2xx : Success - The action was successfully receivedn understood and
      --  accepted

      S300, S301, S302, S303, S304, S305, S307,
      --  3xx : Redirection - Further action must be taken in order to
      --  complete the request

      S400, S401, S402, S403, S404, S405, S406, S407, S408, S409,
      S410, S411, S412, S413, S414, S415, S416, S417,
      --  4xx : Client Error - The request contains bad syntax or cannot be
      --  fulfilled

      S500, S501, S502, S503, S504, S505
      --  5xx : Server Error - The server failed to fulfill an apparently
      --  valid request
      );

   function Image (S : in Status_Code) return String;

   function Reason_Phrase (S : in Status_Code) return String;


   --  HTTP message constructors

   function Status_Line (Code : in Status_Code) return String;

   function Content_Length (Size : in Natural) return String;

   function Content_Type (Format : in String) return String;

   function Connection (Mode : in String) return String;

   function Www_Authenticate (Realm : in String) return String;

   --  helper functions

   function Is_Match (Str, Pattern : in String) return Boolean;
   pragma Inline (Is_Match);
   --  returns True if Pattern matches the begining of Str. The test is not
   --  case sensitive.

   function To_HTTP_Date (Time : in Calendar.Time) return String;

   function To_Time (HTTP_Date : in String) return Calendar.Time;

end AWS.Messages;
