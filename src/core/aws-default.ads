------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2020, AdaCore                     --
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

--  This package contains the default AWS configuration values. These values
--  are used to initialize the configuration objects. Users should not modify
--  the values here, see AWS.Config.* API.

with System;

package AWS.Default with Pure is

   use System;

   --  All times are in seconds

   Ten_Years     : constant := 86_400.0 * 365 * 10;

   One_Hour      : constant := 3_600.0;
   One_Minute    : constant :=    60.0;

   Eight_Hours   : constant :=  8.0 * One_Hour;
   Three_Hours   : constant :=  3.0 * One_Hour;

   Three_Minutes : constant :=  3.0 * One_Minute;
   Five_Minutes  : constant :=  5.0 * One_Minute;
   Ten_Minutes   : constant := 10.0 * One_Minute;

   --  Server configuration

   Server_Name                       : constant String   := "AWS Module";
   WWW_Root                          : constant String   := "./";
   Admin_URI                         : constant String   := "";
   Admin_Password                    : constant String   := "";
   Admin_Realm                       : constant String   := "AWS Admin Page";
   Protocol_Family                   : constant String   := "FAMILY_UNSPEC";
   IPv6_Only                         : constant Boolean  := False;
   Server_Port                       : constant          := 8080;
   Hotplug_Port                      : constant          := 8888;
   Max_Connection                    : constant          := 5;
   Max_WebSocket_Handler             : constant          := 2;
   Max_WebSocket                     : constant          := 512;
   WebSocket_Message_Queue_Size      : constant          := 10;
   WebSocket_Send_Message_Queue_Size : constant          := 30;
   WebSocket_Timeout                 : constant Duration := Eight_Hours;
   Send_Buffer_Size                  : constant          := 0;
   TCP_No_Delay                      : constant Boolean  := False;
   Free_Slots_Keep_Alive_Limit       : constant          := 1;
   Keep_Alive_Force_Limit            : constant          := 0;
   Keep_Alive_Close_Limit            : constant          := 0;
   Accept_Queue_Size                 : constant          := 64;
   Upload_Directory                  : constant String   := "";
   Upload_Size_Limit                 : constant          := 16#500_000#;
   Line_Stack_Size                   : constant          := 16#150_000#;
   Case_Sensitive_Parameters         : constant Boolean  := True;
   Input_Line_Size_Limit             : constant          := 16#4000#;
   Max_POST_Parameters               : constant          := 100;
   Max_Concurrent_Download           : constant          := 25;
   Reuse_Address                     : constant Boolean  := False;
   MIME_Types                        : constant String   := "aws.mime";

   --  Client configuration

   User_Agent                      : constant String :=
                                       "AWS (Ada Web Server) v" & Version;
   Server_Header                   : constant String :=
                                       User_Agent;

   --  Log values. The character '@' in the error log filename prefix is
   --  replaced by the running program name.

   Log_Activated                   : constant Boolean := False;
   Log_File_Directory              : constant String := "./";

   Log_Split_Mode                  : constant String := "NONE";
   Log_Filename_Prefix             : constant String := "@";

   Error_Log_Activated             : constant Boolean := False;
   Error_Log_Split_Mode            : constant String := "NONE";
   Error_Log_Filename_Prefix       : constant String := "@_error";

   Log_Size_Limit                  : constant Natural := 0;

   --  Session

   Session                         : constant Boolean  := False;
   Session_Name                    : constant String   := "AWS";
   Session_Private_Name            : constant String   := "AWS_Private";
   Session_Cleanup_Interval        : constant Duration := Five_Minutes;
   Session_Lifetime                : constant Duration := Ten_Minutes;
   Session_Id_Length               : constant Positive := 11;

   --  Context

   Context_Lifetime                : constant Duration := Eight_Hours;

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
   Security_Mode                   : constant String  := "TLS";
   Config_Directory                : constant String  := ".config/ada-web-srv";
   Disable_Program_Ini             : constant Boolean := False;
   Cipher_Priorities               : constant String  := "";
   TLS_Ticket_Support              : constant Boolean := False;
   Certificate                     : constant String  := "cert.pem";
   Key                             : constant String  := "";
   Client_Certificate              : constant String  := "";
   Exchange_Certificate            : constant Boolean := False;
   Certificate_Required            : constant Boolean := False;
   Trusted_CA                      : constant String  := "";
   CRL_File                        : constant String  := "";
   Check_URL_Validity              : constant Boolean := True;
   SSL_Session_Cache_Size          : constant         := 16#4000#;

   --  Priorities

   Server_Priority                 : constant Any_Priority := Default_Priority;
   WebSocket_Priority              : constant Any_Priority := Default_Priority;
   Session_Cleaner_Priority        : constant Any_Priority := Default_Priority;
   Service_Priority                : constant Any_Priority := Default_Priority;

end AWS.Default;
