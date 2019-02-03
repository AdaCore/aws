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

with Ada.Directories;

with AWS.Headers.Values;
with AWS.Utils;

package body AWS.Messages is

   subtype Status_Code_Image is String (1 .. 3);

   S100_Message : aliased constant String := "Continue";
   S101_Message : aliased constant String := "Switching Protocols";

   S102_Message : aliased constant String := "Processing";
   --  Introduced in the WebDAV HTTP extension, refer to RFC2518

   S200_Message : aliased constant String := "OK";
   S201_Message : aliased constant String := "Created";
   S202_Message : aliased constant String := "Accepted";
   S203_Message : aliased constant String := "Non-Authoritative Information";
   S204_Message : aliased constant String := "No Content";
   S205_Message : aliased constant String := "Reset Content";
   S206_Message : aliased constant String := "Partial Content";

   S207_Message : aliased constant String := "Multi-Status";
   --  Introduced in the WebDAV HTTP extension, refer to RFC4918

   S300_Message : aliased constant String := "Multiple Choices";
   S301_Message : aliased constant String := "Moved Permanently";
   S302_Message : aliased constant String := "Found";
   S303_Message : aliased constant String := "See Other";
   S304_Message : aliased constant String := "Not Modified";
   S305_Message : aliased constant String := "Use Proxy";
   S307_Message : aliased constant String := "Temporary Redirect";

   S400_Message : aliased constant String := "Bad Request";
   S401_Message : aliased constant String := "Unauthorized";
   S402_Message : aliased constant String := "Payment Required";
   S403_Message : aliased constant String := "Forbidden";
   S404_Message : aliased constant String := "Not Found";
   S405_Message : aliased constant String := "Method Not Allowed";
   S406_Message : aliased constant String := "Not Acceptable";
   S407_Message : aliased constant String := "Proxy Authentication Required";
   S408_Message : aliased constant String := "Request Time-out";
   S409_Message : aliased constant String := "Conflict";
   S410_Message : aliased constant String := "Gone";
   S411_Message : aliased constant String := "Length Required";
   S412_Message : aliased constant String := "Precondition Failed";
   S413_Message : aliased constant String := "Request Entity Too Large";
   S414_Message : aliased constant String := "Request-URI Too Large";
   S415_Message : aliased constant String := "Unsupported Media Type";
   S416_Message : aliased constant String := "Requestd range not satisfiable";
   S417_Message : aliased constant String := "Expectation Failed";

   S422_Message : aliased constant String := "Unprocessable Entity";
   S423_Message : aliased constant String := "Locked";
   S424_Message : aliased constant String := "Failed Dependency";
   --  Introduced in the WebDAV HTTP extension, refer to RFC4918

   S500_Message : aliased constant String := "Internal Server Error";
   S501_Message : aliased constant String := "Not Implemented";
   S502_Message : aliased constant String := "Bad Gateway";
   S503_Message : aliased constant String := "Service Unavailable";
   S504_Message : aliased constant String := "Gateway Time-out";
   S505_Message : aliased constant String := "HTTP Version not supported";

   S507_Message : aliased constant String := "Insufficient Storage";
   --  Introduced in the WebDAV HTTP extension, refer to RFC4918

   type Status_Data is record
      Code          : Status_Code_Image;
      Reason_Phrase : not null access constant String;
   end record;

   Status_Messages : constant array (Status_Code) of Status_Data :=
                       (S100 => ("100", S100_Message'Access),
                        S101 => ("101", S101_Message'Access),
                        S102 => ("102", S102_Message'Access),
                        S200 => ("200", S200_Message'Access),
                        S201 => ("201", S201_Message'Access),
                        S202 => ("202", S202_Message'Access),
                        S203 => ("203", S203_Message'Access),
                        S204 => ("204", S204_Message'Access),
                        S205 => ("205", S205_Message'Access),
                        S206 => ("206", S206_Message'Access),
                        S207 => ("207", S207_Message'Access),
                        S300 => ("300", S300_Message'Access),
                        S301 => ("301", S301_Message'Access),
                        S302 => ("302", S302_Message'Access),
                        S303 => ("303", S303_Message'Access),
                        S304 => ("304", S304_Message'Access),
                        S305 => ("305", S305_Message'Access),
                        S307 => ("307", S307_Message'Access),
                        S400 => ("400", S400_Message'Access),
                        S401 => ("401", S401_Message'Access),
                        S402 => ("402", S402_Message'Access),
                        S403 => ("403", S403_Message'Access),
                        S404 => ("404", S404_Message'Access),
                        S405 => ("405", S405_Message'Access),
                        S406 => ("406", S406_Message'Access),
                        S407 => ("407", S407_Message'Access),
                        S408 => ("408", S408_Message'Access),
                        S409 => ("409", S409_Message'Access),
                        S410 => ("410", S410_Message'Access),
                        S411 => ("411", S411_Message'Access),
                        S412 => ("412", S412_Message'Access),
                        S413 => ("413", S413_Message'Access),
                        S414 => ("414", S414_Message'Access),
                        S415 => ("415", S415_Message'Access),
                        S416 => ("416", S416_Message'Access),
                        S417 => ("417", S417_Message'Access),
                        S422 => ("422", S422_Message'Access),
                        S423 => ("422", S423_Message'Access),
                        S424 => ("422", S424_Message'Access),
                        S500 => ("500", S500_Message'Access),
                        S501 => ("501", S501_Message'Access),
                        S502 => ("502", S502_Message'Access),
                        S503 => ("503", S503_Message'Access),
                        S504 => ("504", S504_Message'Access),
                        S505 => ("505", S505_Message'Access),
                        S507 => ("507", S507_Message'Access));

   Month_Name : constant array (Calendar.Month_Number) of String (1 .. 3) :=
                  ("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                   "Jul", "Aug", "Sep", "Oct", "Nov", "Dec");

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
   begin
      return Status_Messages (S).Code;
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
      return Status_Messages (S).Reason_Phrase.all;
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
        & Status_Messages (Code).Code & ' '
        & (if Reason_Phrase = ""
           then Status_Messages (Code).Reason_Phrase.all
           else Reason_Phrase);
   end Status_Line;

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

         Day_Names : constant array (Integer range 0 .. 6) of String (1 .. 3)
           := ("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat");

         C         : Integer;
         Y         : Integer := Calendar.Year (Date);
         M         : Integer := Calendar.Month (Date);
         D         : constant Integer := Calendar.Day (Date);

      begin
         --  Calculate day of week by using Zeller's congruence
         if M < 3 then
            Y := Y - 1;
            M := M + 10;
         else
            M := M - 2;
         end if;

         C := Y / 100;     --  first two digits of Year
         Y := Y mod 100;   --  last two digits of Year

         return Day_Names (((26 * M - 2) / 10
                            + D
                            + Y
                            + Y / 4
                            + C / 4
                            - 2 * C) mod 7);
      end Weekday;

      Day  : constant String  := Image (Calendar.Day (Time));
      Mon  : constant String  := Month_Name (Calendar.Month (Time));
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
            if Month_Name = Messages.Month_Name (I) then
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
