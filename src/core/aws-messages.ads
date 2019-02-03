------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2018, AdaCore                     --
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

with Ada.Calendar;
with Ada.Streams;
with Ada.Strings.Unbounded;

package AWS.Messages is

   use Ada;
   use Ada.Streams;
   use Ada.Strings.Unbounded;

   -----------------
   -- HTTP tokens --
   -----------------

   HTTP_Token    : constant String := "HTTP/";
   Options_Token : constant String := "OPTIONS";
   Get_Token     : constant String := "GET";
   Head_Token    : constant String := "HEAD";
   Post_Token    : constant String := "POST";
   Put_Token     : constant String := "PUT";
   Delete_Token  : constant String := "DELETE";
   Trace_Token   : constant String := "TRACE";
   Connect_Token : constant String := "CONNECT";
   --  Sorted like in RFC 2616 Method definition

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

   --  Cross-Origin Resource Sharing request header tokens
   Access_Control_Request_Headers_Token : constant String :=
                                            "Access-Control-Request-Headers";
   Access_Control_Request_Method_Token  : constant String :=
                                            "Access-Control-Request-Method";
   Origin_Token                         : constant String := "Origin";

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

   --  Cross-Origin Resource Sharing response header tokens
   Access_Control_Allow_Credentials_Token : constant String :=
     "Access-Control-Allow-Credentials";
   Access_Control_Allow_Headers_Token     : constant String :=
                                              "Access-Control-Allow-Headers";
   Access_Control_Allow_Methods_Token     : constant String :=
                                              "Access-Control-Allow-Methods";
   Access_Control_Allow_Origin_Token      : constant String :=
                                              "Access-Control-Allow-Origin";
   Access_Control_Expose_Headers_Token    : constant String :=
                                              "Access-Control-Expose-Headers";
   Access_Control_Max_Age_Token           : constant String :=
                                              "Access-Control-Max-Age";

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

   --  Cookie token RFC 2109
   Cookie_Token              : constant String := "Cookie";
   Set_Cookie_Token          : constant String := "Set-Cookie";
   Comment_Token             : constant String := "Comment";
   Domain_Token              : constant String := "Domain";
   Max_Age_Token             : constant String := "Max-Age";
   Path_Token                : constant String := "Path";
   Secure_Token              : constant String := "Secure";
   HTTP_Only_Token           : constant String := "HttpOnly";

   --  Other tokens
   Proxy_Connection_Token          : constant String := "Proxy-Connection";
   Content_Disposition_Token       : constant String := "Content-Disposition";
   SOAPAction_Token                : constant String := "SOAPAction";
   Content_Id_Token                : constant String := "Content-ID";
   Content_Transfer_Encoding_Token : constant String :=
                                       "Content-Transfer-Encoding";

   --  WebSockets tokens
   Websocket_Token              : constant String := "WebSocket";
   Sec_WebSocket_Accept_Token   : constant String := "Sec-WebSocket-Accept";
   Sec_WebSocket_Protocol_Token : constant String := "Sec-WebSocket-Protocol";
   Sec_WebSocket_Key_Token      : constant String := "Sec-WebSocket-Key";
   Sec_WebSocket_Key1_Token     : constant String := "Sec-WebSocket-Key1";
   Sec_WebSocket_Key2_Token     : constant String := "Sec-WebSocket-Key2";
   Sec_WebSocket_Version_Token  : constant String := "Sec-WebSocket-Version";
   Sec_WebSocket_Origin_Token   : constant String := "Sec-WebSocket-Origin";
   Sec_WebSocket_Location_Token : constant String := "Sec-WebSocket-Location";
   Chat_Token                   : constant String := "chat";

   S100_Continue : constant String := "100-continue";
   --  Supported expect header value

   -----------------
   -- Status Code --
   -----------------

   type Status_Code is
     (S100, S101, S102,
      --  1xx : Informational - Request received, continuing process

      S200, S201, S202, S203, S204, S205, S206, S207,
      --  2xx : Success - The action was successfully received, understood and
      --  accepted

      S300, S301, S302, S303, S304, S305, S307,
      --  3xx : Redirection - Further action must be taken in order to
      --  complete the request

      S400, S401, S402, S403, S404, S405, S406, S407, S408, S409,
      S410, S411, S412, S413, S414, S415, S416, S417, S422, S423, S424,
      --  4xx : Client Error - The request contains bad syntax or cannot be
      --  fulfilled

      S500, S501, S502, S503, S504, S505, S507
      --  5xx : Server Error - The server failed to fulfill an apparently
      --  valid request
      );

   subtype Informational is Status_Code range S100 .. S102;
   subtype Success       is Status_Code range S200 .. S207;
   subtype Redirection   is Status_Code range S300 .. S307;
   subtype Client_Error  is Status_Code range S400 .. S424;
   subtype Server_Error  is Status_Code range S500 .. S507;

   function Image (S : Status_Code) return String;
   --  Returns Status_Code image. This value does not contain the leading S

   function Reason_Phrase (S : Status_Code) return String;
   --  Returns the reason phrase for the status code S, see [RFC 2616 - 6.1.1]

   function With_Body (S : Status_Code) return Boolean;
   --  Returns True if message with status can have a body

   ----------------------
   -- Content encoding --
   ----------------------

   type Content_Encoding is (Identity, GZip, Deflate);
   --  Encoding mode for the response, Identity means that no encoding is
   --  done, Gzip/Deflate to select the Gzip or Deflate encoding algorithm.

   -------------------
   -- Cache_Control --
   -------------------

   type Cache_Option is new String;
   --  Cache_Option is a string and any specific option can be specified. We
   --  define four options:
   --
   --  Unspecified   : No cache option will used.
   --  No_Cache      : Ask browser and proxy to not cache data (no-cache,
   --                  max-age, and s-maxage are specified).
   --  No_Store      : Ask browser and proxy to not store any data. This can be
   --                  used to protect sensitive data.
   --  Prevent_Cache : Equivalent to No_Store + No_Cache

   Unspecified   : constant Cache_Option;
   No_Cache      : constant Cache_Option;
   No_Store      : constant Cache_Option;
   Prevent_Cache : constant Cache_Option;

   type Cache_Kind is (Request, Response);

   type Delta_Seconds is new Integer range -1 .. Integer'Last;
   --  Represents a delta-seconds parameter for some Cache_Data fields like
   --  max-age, max-stale (value -1 is used for Unset).

   Unset         : constant Delta_Seconds;
   No_Max_Stale  : constant Delta_Seconds;
   Any_Max_Stale : constant Delta_Seconds;

   type Private_Option is new Unbounded_String;

   All_Private   : constant Private_Option;
   Private_Unset : constant Private_Option;

   --  Cache_Data is a record that represents cache control information

   type Cache_Data (CKind : Cache_Kind) is record
      No_Cache       : Boolean       := False;
      No_Store       : Boolean       := False;
      No_Transform   : Boolean       := False;
      Max_Age        : Delta_Seconds := Unset;

      case CKind is
         when Request =>
            Max_Stale      : Delta_Seconds := Unset;
            Min_Fresh      : Delta_Seconds := Unset;
            Only_If_Cached : Boolean       := False;

         when Response =>
            S_Max_Age        : Delta_Seconds  := Unset;
            Public           : Boolean        := False;
            Private_Field    : Private_Option := Private_Unset;
            Must_Revalidate  : Boolean        := False;
            Proxy_Revalidate : Boolean        := False;
      end case;
   end record;

   function To_Cache_Option (Data : Cache_Data) return Cache_Option;
   --  Returns a cache control value for an HTTP request/response, fields are
   --  described into RFC 2616 [14.9 Cache-Control].

   function To_Cache_Data
     (Kind : Cache_Kind; Value : Cache_Option) return Cache_Data;
   --  Returns a Cache_Data record parsed out of Cache_Option

   ----------
   -- ETag --
   ----------

   type ETag_Value is new String;

   function Create_ETag
     (Name : String; Weak : Boolean := False) return ETag_Value;
   --  Returns an ETag value (strong by default and Weak if specified). For a
   --  discussion about ETag see RFC 2616 [3.11 Entity Tags] and [14.19 ETag].

   -------------------------------
   -- HTTP message constructors --
   -------------------------------

   function Accept_Encoding (Encoding : String) return String with Inline;

   function Accept_Type (Mode : String) return String with Inline;

   function Accept_Language (Mode : String) return String with Inline;

   function Authorization (Mode, Password : String) return String with Inline;

   function Connection (Mode : String) return String with Inline;

   function Content_Length (Size : Stream_Element_Offset) return String
     with Inline;

   function Cookie (Value : String) return String with Inline;

   function Content_Type (Format : String) return String with Inline;

   function Content_Type
     (Format : String; Boundary : String) return String with Inline;

   function Cache_Control (Option : Cache_Option) return String with Inline;

   function Cache_Control (Data : Cache_Data) return String with Inline;

   function Content_Disposition
     (Format, Name, Filename : String) return String with Inline;
   --  Note that this is not part of HTTP/1.1 standard, it is there because
   --  there is a lot of implementation around using it. This header is used
   --  in multipart data.

   function ETag (Value : ETag_Value) return String with Inline;

   function Expires (Date : Calendar.Time) return String with Inline;
   --  The date should not be more than a year in the future, see RFC 2616
   --  [14.21 Expires].

   function Host (Name : String) return String with Inline;

   function Last_Modified (Date : Calendar.Time) return String with Inline;

   function Location (URL : String) return String with Inline;

   function Proxy_Authorization (Mode, Password : String) return String
     with Inline;

   function Proxy_Connection (Mode : String) return String with Inline;

   function Data_Range (Value : String) return String with Inline;

   function SOAPAction (URI : String) return String with Inline;

   function Status_Line
     (Code          : Status_Code;
      Reason_Phrase : String := "") return String with Inline;

   function Transfer_Encoding (Encoding : String) return String with Inline;

   function User_Agent (Name : String) return String with Inline;

   function WWW_Authenticate (Realm : String) return String with Inline;
   --  Basic authentication request

   function WWW_Authenticate
     (Realm, Nonce : String; Stale : Boolean) return String with Inline;
   --  Digest authentication request

   function Sec_WebSocket_Accept (Key : String) return String with Inline;

   -----------------------
   --  helper functions --
   -----------------------

   function To_HTTP_Date (Time : Calendar.Time) return String;
   --  Returns an Ada time as a string using the HTTP normalized format.
   --  Format is RFC 822, updated by RFC 1123.

   function To_Time (HTTP_Date : String) return Calendar.Time;
   --  Returns an Ada time from an HTTP one. This is To_HTTP_Date opposite
   --  function.

private

   function With_Body (S : Status_Code) return Boolean is
     (S not in Informational | S204 | S304);

   Unspecified   : constant Cache_Option := "";
   No_Cache      : constant Cache_Option := "no-cache, max-age=0, s-maxage=0";
   No_Store      : constant Cache_Option := "no-store";
   Prevent_Cache : constant Cache_Option := No_Store & ", " & No_Cache;

   Unset         : constant Delta_Seconds := -1;
   No_Max_Stale  : constant Delta_Seconds := Unset;
   Any_Max_Stale : constant Delta_Seconds := Delta_Seconds'Last;

   All_Private   : constant Private_Option := To_Unbounded_String ("*");
   Private_Unset : constant Private_Option :=
                     Private_Option (Null_Unbounded_String);

   function Content_Type (Format : String) return String is
     (Content_Type (Format, ""));

end AWS.Messages;
