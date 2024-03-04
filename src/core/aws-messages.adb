------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2024, AdaCore                     --
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

with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Directories;

with AWS.Headers.Values;
with AWS.Utils;

package body AWS.Messages is

   package Status is
     new Containers.Indefinite_Ordered_Maps (Status_Code, String);

   Status_Messages : constant Status.Map :=
                       [S100 => "Continue",
                        S101 => "Switching Protocols",
                        S102 => "Processing",

                        --  Introduced in the WebDAV HTTP extension,
                        --  Refer to RFC2518.

                        S200 => "OK",
                        S201 => "Created",
                        S202 => "Accepted",
                        S203 => "Non-Authoritative Information",
                        S204 => "No Content",
                        S205 => "Reset Content",
                        S206 => "Partial Content",
                        S207 => "Multi-Status", -- WebDAV
                        S208 => "Already Reported", -- WebDAV
                        S226 => "IM Used",

                        S300 => "Multiple Choices",
                        S301 => "Moved Permanently",
                        S302 => "Found",
                        S303 => "See Other",
                        S304 => "Not Modified",
                        S305 => "Use Proxy",
                        S306 => "Switch Proxy",
                        S307 => "Temporary Redirect",
                        S308 => "Permanent Redirect",

                        S400 => "Bad Request",
                        S401 => "Unauthorized",
                        S402 => "Payment Required",
                        S403 => "Forbidden",
                        S404 => "Not Found",
                        S405 => "Method Not Allowed",
                        S406 => "Not Acceptable",
                        S407 => "Proxy Authentication Required",
                        S408 => "Request Time-out",
                        S409 => "Conflict",
                        S410 => "Gone",
                        S411 => "Length Required",
                        S412 => "Precondition Failed",
                        S413 => "Request Entity Too Large",
                        S414 => "Request-URI Too Large",
                        S415 => "Unsupported Media Type",
                        S416 => "Requestd range not satisfiable",
                        S417 => "Expectation Failed",
                        S418 => "I'm a teapot", -- RFC 7168

                        S421 => "Misdirected Request",
                        S422 => "Unprocessable Entity",
                        S423 => "Locked",
                        S424 => "Failed Dependency", -- WebDAV
                        S425 => "Too Early",
                        S426 => "Upgrade Required",
                        S428 => "Precondition Required",
                        S429 => "Too Many Requests",
                        S431 => "Request Header Fields Too Large",
                        S451 => "Unavailable For Legal Reasons",

                        S500 => "Internal Server Error",
                        S501 => "Not Implemented",
                        S502 => "Bad Gateway",
                        S503 => "Service Unavailable",
                        S504 => "Gateway Time-out",
                        S505 => "HTTP Version not supported",
                        S506 => "Variant Also Negotiates",
                        S507 => "Insufficient Storage", -- WebDAV
                        S508 => "Loop Detected",
                        S510 => "Not Extended",
                        S511 => "Network Authentication Required",
                        S520 => "Unknown Error",
                        S521 => "Web Server Is Down",
                        S522 => "Connection Timed Out",
                        S523 => "Origin Is Unreachable",
                        S524 => "A timeout occurred",
                        S525 => "SSL Handshake Failed",
                        S526 => "Invalid SSL Certificate" ];

   Month_Names : constant array (Calendar.Month_Number) of String (1 .. 3) :=
                   ["Jan", "Feb", "Mar", "Apr", "May", "Jun",
                    "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"];

   Day_Names  : constant array (Integer range 0 .. 6) of String (1 .. 3) :=
                  ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"];

   HD : constant String := ": ";
   --  Header delimiter with space for well formatting

   ---------------------
   -- Accept_Encoding --
   ---------------------

   function Accept_Encoding (Encoding : String) return String is
   begin
      return Accept_Encoding_Token & HD & Encoding;
   end Accept_Encoding;

   ---------------------
   -- Accept_Language --
   ---------------------

   function Accept_Language (Mode : String) return String is
   begin
      return Accept_Language_Token & HD & Mode;
   end Accept_Language;

   -----------------
   -- Accept_Type --
   -----------------

   function Accept_Type (Mode : String) return String is
   begin
      return Accept_Token & HD & Mode;
   end Accept_Type;

   -------------------
   -- Authorization --
   -------------------

   function Authorization (Mode, Password : String) return String is
   begin
      return Authorization_Token & HD & Mode & ' ' & Password;
   end Authorization;

   -------------------
   -- Cache_Control --
   -------------------

   function Cache_Control (Option : Cache_Option) return String is
   begin
      return Cache_Control_Token & HD & String (Option);
   end Cache_Control;

   function Cache_Control (Data : Cache_Data) return String is
   begin
      return Cache_Control (To_Cache_Option (Data));
   end Cache_Control;

   ----------------
   -- Connection --
   ----------------

   function Connection (Mode : String) return String is
   begin
      return Connection_Token & HD & Mode;
   end Connection;

   -------------------------
   -- Content_Disposition --
   -------------------------

   function Content_Disposition
     (Format, Name, Filename : String) return String is
   begin
      if Filename = "" then
         return Content_Disposition_Token & HD & Format
           & "; name=""" & Name & '"';
      else
         return Content_Disposition_Token & HD & Format
           & "; name=""" & Name & """; filename="""
           & Directories.Simple_Name (Filename) & '"';
      end if;
   end Content_Disposition;

   --------------------
   -- Content_Length --
   --------------------

   function Content_Length (Size : Stream_Element_Offset) return String is
   begin
      return Content_Length_Token & ':' & Stream_Element_Offset'Image (Size);
   end Content_Length;

   ------------------
   -- Content_Type --
   ------------------

   function Content_Type
     (Format : String; Boundary : String) return String is
   begin
      if Boundary = "" then
         return Content_Type_Token & HD & Format;

      else
         return Content_Type_Token & HD & Format
           & "; boundary=""" & Boundary & '"';
      end if;
   end Content_Type;

   ------------
   -- Cookie --
   ------------

   function Cookie (Value : String) return String is
   begin
      return Cookie_Token & HD & Value;
   end Cookie;

   -----------------
   -- Create_ETag --
   -----------------

   function Create_ETag
     (Name : String; Weak : Boolean := False) return ETag_Value
   is
      ET : constant ETag_Value := '"' & ETag_Value (Name) & '"';
   begin
      if Weak then
         return "W/" & ET;
      else
         return ET;
      end if;
   end Create_ETag;

   ----------------
   -- Data_Range --
   ----------------

   function Data_Range (Value : String) return String is
   begin
      return Range_Token & HD & Value;
   end Data_Range;

   ----------
   -- Date --
   ----------

   function Date (Date : Calendar.Time) return String is
   begin
      return Date_Token & HD & To_HTTP_Date (Date);
   end Date;

   ----------
   -- ETag --
   ----------

   function ETag (Value : ETag_Value) return String is
   begin
      return ETag_Token & HD & String (Value);
   end ETag;

   -------------
   -- Expires --
   -------------

   function Expires (Date : Calendar.Time) return String is
   begin
      return Expires_Token & HD & To_HTTP_Date (Date);
   end Expires;

   ----------
   -- Host --
   ----------

   function Host (Name : String) return String is
   begin
      return Host_Token & HD & Name;
   end Host;

   -----------
   -- Image --
   -----------

   function Image (S : Status_Code) return String is
      Img : constant String := Status_Code'Image (S);
   begin
      return Img (Img'First + 1 .. Img'Last);
   end Image;

   -------------------
   -- Last_Modified --
   -------------------

   function Last_Modified (Date : Calendar.Time) return String is
   begin
      return Last_Modified_Token & HD & To_HTTP_Date (Date);
   end Last_Modified;

   --------------
   -- Location --
   --------------

   function Location (URL : String) return String is
   begin
      return Location_Token & HD & URL;
   end Location;

   -------------------------
   -- Proxy_Authorization --
   -------------------------

   function Proxy_Authorization (Mode, Password : String) return String is
   begin
      return Proxy_Authorization_Token & HD & Mode & ' ' & Password;
   end Proxy_Authorization;

   ----------------------
   -- Proxy_Connection --
   ----------------------

   function Proxy_Connection (Mode : String) return String is
   begin
      return Proxy_Connection_Token & HD & Mode;
   end Proxy_Connection;

   -------------------
   -- Reason_Phrase --
   -------------------

   function Reason_Phrase (S : Status_Code) return String is
   begin
      return Status_Messages (S);
   end Reason_Phrase;

   --------------------------
   -- Sec_WebSocket_Accept --
   --------------------------

   function Sec_WebSocket_Accept (Key : String) return String is
   begin
      return Sec_WebSocket_Accept_Token & HD & Key;
   end Sec_WebSocket_Accept;

   ----------------
   -- SOAPAction --
   ----------------

   function SOAPAction (URI : String) return String is
   begin
      return SOAPAction_Token & HD & '"' & URI & '"';
   end SOAPAction;

   -----------------
   -- Status_Line --
   -----------------

   function Status_Line
     (Code          : Status_Code;
      Reason_Phrase : String := "") return String is
   begin
      return HTTP_Version & ' '
        & Image (Code) & ' '
        & (if Reason_Phrase = ""
           then Messages.Reason_Phrase (Code)
           else Reason_Phrase);
   end Status_Line;

   ------------------
   -- Status_Value --
   ------------------

   function Status_Value
     (Code          : Status_Code;
      Reason_Phrase : String := "") return String is
   begin
      return Image (Code) & ' '
        & (if Reason_Phrase = ""
           then Messages.Reason_Phrase (Code)
           else Reason_Phrase);
   end Status_Value;

   -------------------
   -- To_Cache_Data --
   -------------------

   function To_Cache_Data
     (Kind : Cache_Kind; Value : Cache_Option) return Cache_Data
   is
      Result : Cache_Data (Kind);

      procedure Simple_Value (Item : String; Quit : in out Boolean);
      --  Receives un-named value

      procedure Named_Value (Name, Value : String; Quit : in out Boolean);
      --  Receives named value

      -----------------
      -- Named_Value --
      -----------------

      procedure Named_Value (Name, Value : String; Quit : in out Boolean) is
      begin
         Quit := False;

         if Utils.Is_Number (Value) then
            if Name = "max-stale" then
               Result.Max_Stale := Delta_Seconds'Value (Value);

            elsif Name = "min-fresh" then
               Result.Min_Fresh := Delta_Seconds'Value (Value);

            elsif Name = "max-age" then
               Result.Max_Age := Delta_Seconds'Value (Value);

            elsif Name = "s-maxage" then
               Result.S_Max_Age := Delta_Seconds'Value (Value);
            end if;

         else
            if Name = "private" then
               Result.Private_Field := To_Unbounded_String (Value);
            end if;
         end if;
      end Named_Value;

      -----------
      -- Value --
      -----------

      procedure Simple_Value (Item : String; Quit : in out Boolean) is
      begin
         Quit := False;

         if Item = "no-cache" then
            Result.No_Cache := True;

         elsif Item = "no-store" then
            Result.No_Store := True;

         elsif Item = "no-transform" then
            Result.No_Transform := True;

         elsif Item = "only-if-cached" then
            Result.Only_If_Cached := True;

         elsif Item = "public" then
            Result.Public := True;

         elsif Item = "private" then
            Result.Private_Field := All_Private;

         elsif Item = "must-revalidate" then
            Result.Must_Revalidate := True;

         elsif Item = "proxy-revalidate" then
            Result.Proxy_Revalidate := True;
         end if;
      end Simple_Value;

      procedure Parse is new Headers.Values.Parse (Simple_Value, Named_Value);

   begin
      Parse (String (Value));
      return Result;
   end To_Cache_Data;

   ---------------------
   -- To_Cache_Option --
   ---------------------

   function To_Cache_Option (Data : Cache_Data) return Cache_Option is
      Result : Unbounded_String;
   begin
      if Data.No_Cache then
         Utils.Append_With_Sep (Result, "no-cache");
      end if;

      if Data.No_Store then
         Utils.Append_With_Sep (Result, "no-store");
      end if;

      if Data.No_Transform then
         Utils.Append_With_Sep (Result, "no-transform");
      end if;

      if Data.Max_Age /= Unset then
         Utils.Append_With_Sep
           (Result, "max-age=" & Utils.Image (Integer (Data.Max_Age)));
      end if;

      case Data.CKind is
         when Request =>
            if Data.Max_Stale /= Unset then
               if Data.Max_Stale = Any_Max_Stale then
                  Utils.Append_With_Sep (Result, "max-stale");
               else
                  Utils.Append_With_Sep
                    (Result, "max-stale="
                     & Utils.Image (Integer (Data.Max_Stale)));
               end if;
            end if;

            if Data.Min_Fresh /= Unset then
               Utils.Append_With_Sep
                 (Result, "min-fresh="
                  & Utils.Image (Integer (Data.Min_Fresh)));
            end if;

            if Data.Only_If_Cached then
               Utils.Append_With_Sep (Result, "only-if-cached");
            end if;

         when Response =>
            if Data.S_Max_Age /= Unset then
               Utils.Append_With_Sep
                 (Result, "s-maxage="
                  & Utils.Image (Integer (Data.S_Max_Age)));
            end if;

            if Data.Public then
               Utils.Append_With_Sep (Result, "public");
            end if;

            if Data.Private_Field /= Private_Unset then
               if Data.Private_Field = All_Private then
                  Utils.Append_With_Sep (Result, "private");
               else
                  Utils.Append_With_Sep
                    (Result, "private=" & To_String (Data.Private_Field));
               end if;
            end if;

            if Data.Must_Revalidate then
               Utils.Append_With_Sep (Result, "must-revalidate");
            end if;

            if Data.Proxy_Revalidate then
               Utils.Append_With_Sep (Result, "proxy-revalidate");
            end if;
      end case;

      return Cache_Option (To_String (Result));
   end To_Cache_Option;

   ------------------
   -- To_HTTP_Date --
   ------------------

   function To_HTTP_Date (Time : Calendar.Time) return String is

      function Truncation (S : Calendar.Day_Duration) return Natural;
      --  returns the integral value of S

      function Image (V : Natural) return String;
      --  returns V image without the leading space and with leading zero if
      --  only one digit

      function Weekday (Date : Calendar.Time) return String;
      --  returns the weekday as a 3 letters string for the Date

      -----------
      -- Image --
      -----------

      function Image (V : Natural) return String is
         V_Image : constant String := Natural'Image (V);
      begin
         if V_Image'Length = 2 then
            --  only one digit add a leading zero
            return '0' & V_Image (2 .. V_Image'Last);
         else
            return V_Image (2 .. V_Image'Last);
         end if;
      end Image;

      ----------------
      -- Truncation --
      ----------------

      function Truncation (S : Calendar.Day_Duration) return Natural is
      begin
         if S = 0.0 then
            return 0;
         else
            return Natural (S - 0.5);
         end if;
      end Truncation;

      -------------
      -- Weekday --
      -------------

      function Weekday (Date : Calendar.Time) return String is
         D : constant Integer := Calendar.Day (Date);
         Y : Integer := Calendar.Year (Date);
         M : Integer := Calendar.Month (Date);
         C : Integer;

      begin
         --  Calculate day of week by using Zeller's congruence
         if M < 3 then
            Y := @ - 1;
            M := @ + 10;
         else
            M := @ - 2;
         end if;

         C := Y / 100;     --  first two digits of Year
         Y := @ mod 100;   --  last two digits of Year

         return Day_Names (((26 * M - 2) / 10
                            + D
                            + Y
                            + Y / 4
                            + C / 4
                            - 2 * C) mod 7);
      end Weekday;

      Day  : constant String  := Image (Calendar.Day (Time));
      Mon  : constant String  := Month_Names (Calendar.Month (Time));
      Year : constant String  := Image (Calendar.Year (Time));

      Secs : constant Natural := Truncation (Calendar.Seconds (Time));

      Tmp  : constant Natural := Secs mod 3600;

      H    : constant String  := Image (Secs / 3600);
      M    : constant String  := Image (Tmp / 60);
      S    : constant String  := Image (Tmp mod 60);
   begin
      return Weekday (Time) & ", " & Day & ' ' & Mon & ' ' & Year & ' '
        & H & ':' & M & ':' & S & " GMT";
   end To_HTTP_Date;

   -------------
   -- To_Time --
   -------------

   function To_Time (HTTP_Date : String) return Calendar.Time is

      function Month_Number
        (Month_Name : String) return Calendar.Month_Number;
      --  returns the month number given a 3 letter month name

      F : constant Positive := HTTP_Date'First;

      ------------------
      -- Month_Number --
      ------------------

      function Month_Number
        (Month_Name : String) return Calendar.Month_Number is
      begin
         for I in Calendar.Month_Number loop
            if Month_Name = Messages.Month_Names (I) then
               return I;
            end if;
         end loop;

         raise Constraint_Error
           with "Month_Number: Wrong Month name (" & Month_Name & ')';
      end Month_Number;

   begin
      return Calendar.Time_Of
        (Year    => Calendar.Year_Number'Value (HTTP_Date (F + 12 .. F + 15)),
         Month   => Month_Number (HTTP_Date (F + 8 .. F + 10)),
         Day     => Calendar.Day_Number'Value (HTTP_Date (F + 5 .. F + 6)),
         Seconds => Calendar.Day_Duration
         (Natural'Value (HTTP_Date (F + 17 .. F + 18)) * 3600
          + Natural'Value (HTTP_Date (F + 20 .. F + 21)) * 60
          + Natural'Value (HTTP_Date (F + 23 .. F + 24))));
   end To_Time;

   -----------------------
   -- Transfer_Encoding --
   -----------------------

   function Transfer_Encoding (Encoding : String) return String is
   begin
      return Transfer_Encoding_Token & HD & Encoding;
   end Transfer_Encoding;

   ----------------
   -- User_Agent --
   ----------------

   function User_Agent (Name : String) return String is
   begin
      return User_Agent_Token & HD & Name;
   end User_Agent;

   ----------------------
   -- Www_Authenticate --
   ----------------------

   function WWW_Authenticate (Realm : String) return String is
   begin
      return WWW_Authenticate_Token & HD & "Basic realm=""" & Realm & """";
   end WWW_Authenticate;

   function WWW_Authenticate
     (Realm, Nonce : String; Stale : Boolean) return String is
   begin
      return WWW_Authenticate_Token & HD
        & "Digest qop=""auth"", realm=""" & Realm
        & """, stale=""" & Boolean'Image (Stale)
        & """, nonce=""" & Nonce & """";
   end WWW_Authenticate;

end AWS.Messages;
