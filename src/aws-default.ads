------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2008, AdaCore                     --
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

--  This package contains the default AWS configuration values. These values
--  are used to initialize the configuration objects. Users should not modify
--  the values here, see AWS.Config.* API.

package AWS.Default is

   pragma Pure;

   --  All times are in seconds

   One_Hour      : constant := 3_600.0;
   One_Minute    : constant :=    60.0;

   Eight_Hours   : constant :=  8.0 * One_Hour;
   Three_Hours   : constant :=  3.0 * One_Hour;

   Three_Minutes : constant :=  3.0 * One_Minute;
   Five_Minutes  : constant :=  5.0 * One_Minute;
   Ten_Minutes   : constant := 10.0 * One_Minute;

   --  Server configuration

   Server_Name                     : constant String  := "AWS Module";
   WWW_Root                        : constant String  := "./";
   Admin_URI                       : constant String  := "";
   Server_Port                     : constant         := 8080;
   Hotplug_Port                    : constant         := 8888;
   Max_Connection                  : constant         := 5;
   Free_Slots_Keep_Alive_Limit     : constant         := 1;
   Keep_Alive_Force_Limit          : constant         := 0;
   Keep_Alive_Close_Limit          : constant         := 0;
   Accept_Queue_Size               : constant         := 64;
   Upload_Directory                : constant String  := "";
   Upload_Size_Limit               : constant         := Natural'Last;
   Line_Stack_Size                 : constant         := 16#150_000#;
   Case_Sensitive_Parameters       : constant Boolean := True;
   Input_Line_Size_Limit           : constant         := 16#4000#;
   Max_Concurrent_Download         : constant         := 25;
   Reuse_Address                   : constant Boolean := False;

   --  Client configuration

   User_Agent                      : constant String
     := "AWS (Ada Web Server) v" & Version;

   --  Log values. The character '@' in the error log filename prefix is
   --  replaced by the running program name.

   Log_File_Directory              : constant String := "./";

   Log_Split_Mode                  : constant String := "NONE";
   Log_Filename_Prefix             : constant String := "@";

   Error_Log_Split_Mode            : constant String := "NONE";
   Error_Log_Filename_Prefix       : constant String := "@_error";

   --  Session

   Session                         : constant Boolean  := False;
   Session_Name                    : constant String   := "AWS";
   Session_Cleanup_Interval        : constant Duration := Five_Minutes;
   Session_Lifetime                : constant Duration := Ten_Minutes;

   --  Transient pages

   Transient_Cleanup_Interval      : constant Duration := Three_Minutes;
   Transient_Lifetime              : constant Duration := Five_Minutes;

   --  Server's timeouts

   Cleaner_Wait_For_Client_Timeout : constant Duration := 80.0;
   Cleaner_Client_Header_Timeout   : constant Duration := 7.0;
   Cleaner_Client_Data_Timeout     : constant Duration := Eight_Hours;
   Cleaner_Server_Response_Timeout : constant Duration := Eight_Hours;

   Force_Wait_For_Client_Timeout   : constant Duration := 2.0;
   Force_Client_Header_Timeout     : constant Duration := 2.0;
   Force_Client_Data_Timeout       : constant Duration := Three_Hours;
   Force_Server_Response_Timeout   : constant Duration := Three_Hours;

   Send_Timeout                    : constant Duration := 40.0;
   Receive_Timeout                 : constant Duration := 30.0;

   --  Directory template

   Directory_Browser_Page          : constant String := "aws_directory.thtml";

   --  Status page

   Status_Page                     : constant String := "aws_status.thtml";
   Up_Image                        : constant String := "aws_up.png";
   Down_Image                      : constant String := "aws_down.png";
   Logo_Image                      : constant String := "aws_logo.png";

   --  Security

   Security                        : constant Boolean := False;
   Security_Mode                   : constant String  := "SSLv23";
   Certificate                     : constant String  := "cert.pem";
   Key                             : constant String  := "";
   Client_Certificate              : constant String  := "";
   Exchange_Certificate            : constant Boolean := False;
   Check_URL_Validity              : constant Boolean := True;

end AWS.Default;
