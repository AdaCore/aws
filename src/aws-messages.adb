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

with Ada.Characters.Handling;
with Ada.Exceptions;

package body AWS.Messages is

   type String_Access is access constant String;

   subtype Status_Code_Image is String (1 .. 3);

   S100_Message : aliased constant String := "Continue";
   S101_Message : aliased constant String := "Switching Protocols";
   S200_Message : aliased constant String := "OK";
   S201_Message : aliased constant String := "Create";
   S202_Message : aliased constant String := "Accepted";
   S203_Message : aliased constant String := "Non-Authoritative Information";
   S204_Message : aliased constant String := "No Content";
   S205_Message : aliased constant String := "Reset Content";
   S206_Message : aliased constant String := "Partial Content";
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
   S407_Message : aliased constant String := "Proxy Authentification Required";
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
   S500_Message : aliased constant String := "Internal Server Error";
   S501_Message : aliased constant String := "Not Implemented";
   S502_Message : aliased constant String := "Bad Gateway";
   S503_Message : aliased constant String := "Service Unavailable";
   S504_Message : aliased constant String := "Gateway Time-out";
   S505_Message : aliased constant String := "HTTP Version not supported";

   type Status_Data is record
      Code          : Status_Code_Image;
      Reason_Phrase : String_Access;
   end record;

   Status_Messages : array (Status_Code) of Status_Data
     := (S100 => ("100", S100_Message'Access),
         S101 => ("101", S101_Message'Access),
         S200 => ("200", S200_Message'Access),
         S201 => ("201", S201_Message'Access),
         S202 => ("202", S202_Message'Access),
         S203 => ("203", S203_Message'Access),
         S204 => ("204", S204_Message'Access),
         S205 => ("205", S205_Message'Access),
         S206 => ("206", S206_Message'Access),
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
         S500 => ("500", S500_Message'Access),
         S501 => ("501", S501_Message'Access),
         S502 => ("502", S502_Message'Access),
         S503 => ("503", S503_Message'Access),
         S504 => ("504", S504_Message'Access),
         S505 => ("505", S505_Message'Access));

   Month_Name : constant array (Calendar.Month_Number)
     of String (1 .. 3) := ("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                            "Jul", "Aug", "Sep", "Oct", "Nov", "Dec");

   ---------------------
   -- Accept_Language --
   ---------------------

   function Accept_Language (Mode : in String) return String is
   begin
      return Accept_Language_Token & Mode;
   end Accept_Language;

   -----------------
   -- Accept_Type --
   -----------------

   function Accept_Type (Mode : in String) return String is
   begin
      return Accept_Token & Mode;
   end Accept_Type;

   -------------------
   -- Authorization --
   -------------------

   function Authorization (Mode, Password : in String) return String is
   begin
      return Authorization_Token & Mode & ' ' & Password;
   end Authorization;

   ----------------
   -- Connection --
   ----------------

   function Connection (Mode : in String) return String is
   begin
      return Connection_Token & Mode;
   end Connection;

   --------------------
   -- Content_Length --
   --------------------

   function Content_Length (Size : in Natural) return String is
   begin
      return Content_Length_Token & Natural'Image (Size);
   end Content_Length;

   ------------------
   -- Content_Type --
   ------------------

   function Content_Type (Format : in String) return String is
   begin
      return Content_Type_Token & Format;
   end Content_Type;

   --------------------
   -- Does_Not_Match --
   --------------------

   function Does_Not_Match (Str, Pattern : in String) return Boolean is
      use Ada.Characters;
      U_Str     : constant String := Handling.To_Upper (Str);
      U_Pattern : constant String := Handling.To_Upper (Pattern);
   begin
      return Pattern'Length > Str'Length
        or else U_Str (1 .. Pattern'Length) /= U_Pattern;
   end Does_Not_Match;

   ----------
   -- Host --
   ----------

   function Host (Name : in String) return String is
   begin
      return Host_Token & Name;
   end Host;

   -----------
   -- Image --
   -----------

   function Image (S : in Status_Code) return String is
   begin
      return Status_Messages (S).Code;
   end Image;

   --------------
   -- Is_Match --
   --------------

   function Is_Match (Str, Pattern : in String) return Boolean is
      use Ada.Characters;
      U_Str     : constant String := Handling.To_Upper (Str);
      U_Pattern : constant String := Handling.To_Upper (Pattern);
   begin
      return Pattern'Length <= Str'Length
        and then U_Str (1 .. Pattern'Length) = U_Pattern;
   end Is_Match;

   --------------
   -- Location --
   --------------

   function Location (URL : in String) return String is
   begin
      return "Location: " & URL;
   end Location;

   -------------------------
   -- Proxy_Authorization --
   -------------------------

   function Proxy_Authorization (Mode, Password : in String) return String is
   begin
      return Proxy_Authorization_Token & Mode & ' ' & Password;
   end Proxy_Authorization;

   ----------------------
   -- Proxy_Connection --
   ----------------------

   function Proxy_Connection (Mode : in String) return String is
   begin
      return Proxy_Connection_Token & Mode;
   end Proxy_Connection;

   -------------------
   -- Reason_Phrase --
   -------------------

   function Reason_Phrase (S : in Status_Code) return String is
   begin
      return Status_Messages (S).Reason_Phrase.all;
   end Reason_Phrase;

   -----------------
   -- Status_Line --
   -----------------

   function Status_Line (Code : in Status_Code) return String is
   begin
      return HTTP_Version & ' '
        & Status_Messages (Code).Code & ' '
        & Status_Messages (Code).Reason_Phrase.all;
   end Status_Line;

   ------------------
   -- To_HTTP_Date --
   ------------------

   function To_HTTP_Date (Time : in Calendar.Time) return String is

      function Truncation (S : in Calendar.Day_Duration) return Natural;
      --  returns the integral value of S.

      function Image (V : in Natural) return String;
      --  returns V image without the leading space and with leading zero if
      --  only one digit

      function Weekday (Date : Calendar.Time) return String;
      --  returns the weekday as a 3 letters string for the Date.

      ----------------
      -- Truncation --
      ----------------

      function Truncation (S : in Calendar.Day_Duration) return Natural is
      begin
         if S = 0.0 then
            return 0;
         else
            return Natural (S - 0.5);
         end if;
      end Truncation;

      -----------
      -- Image --
      -----------

      function Image (V : in Natural) return String is
         V_Image : constant String := Natural'Image (V);
      begin
         if V_Image'Length = 2 then
            --  only one digit add a leading zero
            return '0' & V_Image (2 .. V_Image'Last);
         else
            return V_Image (2 .. V_Image'Last);
         end if;
      end Image;

      -------------
      -- Weekday --
      -------------

      function Weekday (Date : Calendar.Time) return String is
         C         : Integer;
         Y         : Integer := Calendar.Year (Date);
         M         : Integer := Calendar.Month (Date);
         D         : Integer := Calendar.Day (Date);

         Day_Names : constant array (Integer range 0 .. 6) of String (1 .. 3)
           := ("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat");
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

   function To_Time (HTTP_Date : in String) return Calendar.Time is

      function Month_Number (Month_Name : in String)
                            return Calendar.Month_Number;
      --  returns the month number given a 3 letter month name.

      F : constant Positive := HTTP_Date'First;

      function Month_Number (Month_Name : in String)
                            return Calendar.Month_Number is
      begin
         for I in Calendar.Month_Number loop
            if Month_Name = Messages.Month_Name (I) then
               return I;
            end if;
         end loop;
         Exceptions.Raise_Exception (Internal_Error'Identity,
                                     "Month_Number: Month name not found");
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

   ----------------
   -- User_Agent --
   ----------------

   function User_Agent (Name : in String) return String is
   begin
      return User_Agent_Token & Name;
   end User_Agent;

   ----------------------
   -- Www_Authenticate --
   ----------------------

   function Www_Authenticate (Realm : in String) return String is
   begin
      return "Www-Authenticate: Basic realm=""" & Realm & """";
   end Www_Authenticate;

end AWS.Messages;
