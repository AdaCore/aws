------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2002                          --
--                               ACT-Europe                                 --
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

with Ada.Calendar;

package AWS.Messages is

   use Ada;

   -----------------
   -- HTTP tokens --
   -----------------

   HTTP_Token : constant String := "HTTP/";
   Get_Token  : constant String := "GET ";
   Head_Token : constant String := "HEAD ";
   Post_Token : constant String := "POST ";

   ------------------------
   -- HTTP header tokens --
   ------------------------

   --  General header tokens RFC 2616
   Cache_Control_Token       : constant String := "Cache-Control";
   Connection_Token          : constant String := "Connection";
   Date_Token                : constant String := "Date";
   Pragma_Token              : constant String := "Pragma";
   Trailer_Token             : constant String := "Trailer";
   Transfer_Encoding_Token   : constant String := "Transfer-Encoding";
   Upgrade_Token             : constant String := "Upgrade";
   Via_Token                 : constant String := "Via";
   Warning_Token             : constant String := "Warning";

   --  Request header tokens RFC 2616
   Accept_Token              : constant String := "Accept";
   Accept_Charset_Token      : constant String := "Accept-Charset";
   Accept_Encoding_Token     : constant String := "Accept-Encoding";
   Accept_Language_Token     : constant String := "Accept-Language";
   Authorization_Token       : constant String := "Authorization";
   Expect_Token              : constant String := "Expect";
   From_Token                : constant String := "From";
   Host_Token                : constant String := "Host";
   If_Match_Token            : constant String := "If-Match";
   If_Modified_Since_Token   : constant String := "If-Modified-Since";
   If_None_Match_Token       : constant String := "If-None-Match";
   If_Range_Token            : constant String := "If-Range";
   If_Unmodified_Since_Token : constant String := "If-Unmodified-Since";
   Max_Forwards_Token        : constant String := "Max-Forwards";
   Proxy_Authorization_Token : constant String := "Proxy-Authorization";
   Range_Token               : constant String := "Range";
   Referer_Token             : constant String := "Referer";
   TE_Token                  : constant String := "TE";
   User_Agent_Token          : constant String := "User-Agent";

   --  Response header tokens RFC 2616
   Accept_Ranges_Token       : constant String := "Accept-Ranges";
   Age_Token                 : constant String := "Age";
   ETag_Token                : constant String := "ETag";
   Location_Token            : constant String := "Location";
   Proxy_Authenticate_Token  : constant String := "Proxy-Authenticate";
   Retry_After_Token         : constant String := "Retry-After";
   Server_Token              : constant String := "Server";
   Vary_Token                : constant String := "Vary";
   WWW_Authenticate_Token    : constant String := "WWW-Authenticate";

   --  Entity header tokens RFC 2616
   Allow_Token               : constant String := "Allow";
   Content_Encoding_Token    : constant String := "Content-Encoding";
   Content_Language_Token    : constant String := "Content-Language";
   Content_Length_Token      : constant String := "Content-Length";
   Content_Location_Token    : constant String := "Content-Location";
   Content_MD5_Token         : constant String := "Content-MD5";
   Content_Range_Token       : constant String := "Content-Range";
   Content_Type_Token        : constant String := "Content-Type";
   Expires_Token             : constant String := "Expires";
   Last_Modified_Token       : constant String := "Last-Modified";

   --  Other tokens.
   Proxy_Connection_Token    : constant String := "Proxy-Connection";
   Content_Disposition_Token : constant String := "Content-Disposition";
   Cookie_Token              : constant String := "Cookie";
   Set_Cookie_Token          : constant String := "Set-Cookie";
   SOAPAction_Token          : constant String := "SOAPAction";

   -----------------
   -- Status Code --
   -----------------

   type Status_Code is
     (S100, S101,
      --  1xx : Informational - Request received, continuing process

      S200, S201, S202, S203, S204, S205, S206,
      --  2xx : Success - The action was successfully received, understood and
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
   --  Returns Status_Code image. This value does not contain the leading S.

   function Reason_Phrase (S : in Status_Code) return String;
   --  Returns the reason phrase for the status code S, see [RFC 2616 - 6.1.1]

   -------------------
   -- Cache_Control --
   -------------------

   type Cache_Option is new String;

   Unspecified : constant Cache_Option;
   No_Cache    : constant Cache_Option;
   No_Store    : constant Cache_Option;

   -------------------------------
   -- HTTP message constructors --
   -------------------------------

   function Accept_Type (Mode : in String) return String;
   pragma Inline (Accept_Type);

   function Accept_Language (Mode : in String) return String;
   pragma Inline (Accept_Language);

   function Authorization (Mode, Password : in String) return String;
   pragma Inline (Authorization);

   function Connection (Mode : in String) return String;
   pragma Inline (Connection);

   function Content_Length (Size : in Natural) return String;
   pragma Inline (Content_Length);

   function Cookie (Value : in String) return String;
   pragma Inline (Cookie);

   function Content_Type
     (Format   : in String;
      Boundary : in String := "")
      return String;
   pragma Inline (Content_Type);

   function Cache_Control (Option : in Cache_Option) return String;
   pragma Inline (Cache_Control);

   function Content_Disposition
     (Format   : in String;
      Name     : in String;
      Filename : in String)
      return String;
   pragma Inline (Content_Disposition);
   --  Note that this is not part of HTTP/1.1 standard, it is there because
   --  there is a lot of implementation around using it. This header is used
   --  in multipart data.

   function Host (Name : in String) return String;
   pragma Inline (Host);

   function Last_Modified (Date : in Calendar.Time) return String;
   pragma Inline (Last_Modified);

   function Location (URL : in String) return String;
   pragma Inline (Location);

   function Proxy_Authorization (Mode, Password : in String) return String;
   pragma Inline (Proxy_Authorization);

   function Proxy_Connection (Mode : in String) return String;
   pragma Inline (Proxy_Connection);

   function SOAPAction (URI : in String) return String;
   pragma Inline (SOAPAction);

   function Status_Line (Code : in Status_Code) return String;
   pragma Inline (Status_Line);

   function Transfer_Encoding (Encoding : in String) return String;
   pragma Inline (Transfer_Encoding);

   function User_Agent (Name : in String) return String;
   pragma Inline (User_Agent);

   function WWW_Authenticate (Realm : in String) return String;
   pragma Inline (WWW_Authenticate);
   --  Basic authentication request.

   function WWW_Authenticate
     (Realm : in String;
      Nonce : in String;
      Stale : in Boolean)
      return String;
   pragma Inline (WWW_Authenticate);
   --  Digest authentication request.

   --  helper functions

   function Match (Str, Pattern : in String) return Boolean;
   pragma Inline (Match);
   --  Returns True if Pattern matches the begining of Str. The test is not
   --  case sensitive.

   function Does_Not_Match (Str, Pattern : in String) return Boolean;
   pragma Inline (Does_Not_Match);
   --  Returns True if Pattern does not matches the begining of Str. The test
   --  is not case sensitive.

   function To_HTTP_Date (Time : in Calendar.Time) return String;
   --  Returns an Ada time as a string using the HTTP normalized format.
   --  Format is RFC 822, updated by RFC 1123.

   function To_Time (HTTP_Date : in String) return Calendar.Time;
   --  Returns an Ada time from an HTTP one. This is To_HTTP_Date opposite
   --  function.

private

   Unspecified : constant Cache_Option := "";
   No_Cache    : constant Cache_Option := "no-cache";
   No_Store    : constant Cache_Option := "no-store";

end AWS.Messages;
