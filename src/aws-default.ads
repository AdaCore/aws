------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2001                          --
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

--  This is the default AWS configuration values.

package AWS.Default is

   pragma Pure;

   Server_Name         : constant String := "AWS Module";
   WWW_Root            : constant String := "./";
   Admin_URI           : constant String := "";
   Server_Port         : constant        := 8080;
   Hotplug_Port        : constant        := 8888;
   Max_Connection      : constant        := 5;
   Accept_Queue_Size   : constant        := 64;
   Log_File_Directory  : constant String := "./";
   Log_Filename_Prefix : constant String := "";
   Log_Split_Mode      : constant String := "NONE";
   Upload_Directory    : constant String := "./";

   --  All times are in seconds

   One_Hour     : constant := 3_600.0;
   One_Minute   : constant :=    60.0;

   Eight_Hours  : constant :=  8.0 * One_Hour;
   Three_Hours  : constant :=  3.0 * One_Hour;
   Five_Minutes : constant :=  5.0 * One_Minute;
   Ten_Minutes  : constant := 10.0 * One_Minute;

   Session_Cleanup_Interval        : constant Duration := Five_Minutes;
   Session_Lifetime                : constant Duration := Ten_Minutes;

   Cleaner_Wait_For_Client_Timeout : constant Duration := 80.0;
   Cleaner_Client_Header_Timeout   : constant Duration := 20.0;
   Cleaner_Client_Data_Timeout     : constant Duration := Eight_Hours;
   Cleaner_Server_Response_Timeout : constant Duration := Eight_Hours;

   Force_Wait_For_Client_Timeout   : constant Duration := 2.0;
   Force_Client_Header_Timeout     : constant Duration := 3.0;
   Force_Client_Data_Timeout       : constant Duration := Three_Hours;
   Force_Server_Response_Timeout   : constant Duration := Three_Hours;

   Send_Timeout    : constant Duration := 40.0;
   Receive_Timeout : constant Duration := 30.0;

   Status_Page     : constant String := "aws_status.thtml";
   Up_Image        : constant String := "aws_up.png";
   Down_Image      : constant String := "aws_down.png";
   Logo_Image      : constant String := "aws_logo.png";

   Security                  : constant Boolean := False;
   Session                   : constant Boolean := False;
   Case_Sensitive_Parameters : constant Boolean := True;
   Check_URL_Validity        : constant Boolean := True;
   Line_Stack_Size           : constant         := 200_000;

end AWS.Default;
