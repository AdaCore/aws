------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                            Copyright (C) 2000                            --
--                Dmitriy Anisimkov, Sune Falck, Pascal Obry                --
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

--  Use direct calls to Win32 system routines instead of the POSIX library
--  and get the current UTC/GMT time from the system.

with Interfaces.C;

package body AWS.OS_Lib is

   ------------------------
   -- Win32 declarations --
   ------------------------

   type System_Time is record
      Year         : Interfaces.Unsigned_16;
      Month        : Interfaces.Unsigned_16;
      Day_Of_Week  : Interfaces.Unsigned_16;
      Day          : Interfaces.Unsigned_16;
      Hour         : Interfaces.Unsigned_16;
      Minute       : Interfaces.Unsigned_16;
      Second       : Interfaces.Unsigned_16;
      Milli_Second : Interfaces.Unsigned_16;
   end record;

   ------------------------------------------------------------

   type File_Time is record
      Low_Date_Time  : Interfaces.Unsigned_32;
      High_Date_Time : Interfaces.Unsigned_32;
   end record;

   type File_Time_Access is access all File_Time;

   ------------------------------------------------------------

   type Handle is new Interfaces.Unsigned_32;

   ------------------------------------------------------------

   type Unsigned_Access is access all Interfaces.Unsigned_32;

   ------------------------------------------------------------

   procedure GetSystemTime (Time : out System_Time);
   pragma Import (Stdcall, GetSystemTime, "GetSystemTime");

   ------------------------------------------------------------

   procedure FileTimeToSystemTime (File_Date   : in  File_Time;
                                   System_Date : out System_Time);
   pragma Import (Stdcall, FileTimeToSystemTime, "FileTimeToSystemTime");

   ------------------------------------------------------------

   function GetFileTime (File_Handle : in Handle;
                         Created     : in File_Time_Access;
                         Accessed    : in File_Time_Access;
                         Modified    : in File_Time_Access)
      return Interfaces.Unsigned_32;
   pragma Import (Stdcall, GetFileTime, "GetFileTime");

   ------------------------------------------------------------

   function GetFileSize (File_Handle    : in Handle;
                         High_Word : in Unsigned_Access := null)
      return Interfaces.Unsigned_32;
   pragma Import (Stdcall, GetFileSize, "GetFileSize");

   ------------------------------------------------------------
   function CreateFile (File_Name     : in Interfaces.C.char_array;
                        Access_Mode   : in Interfaces.Unsigned_32;
                        Share_Mode    : in Interfaces.Unsigned_32;
                        Security      : in Unsigned_Access;    -- Not used
                        Creation      : in Interfaces.Unsigned_32;
                        Flags         : in Interfaces.Unsigned_32;
                        Template      : in Handle)
      return Handle;
   pragma Import (Stdcall, CreateFile, "CreateFileA");

   GENERIC_READ         : constant := 16#80000000#;
   OPEN_EXISTING        : constant := 16#00000003#;
   FILE_SHARE_READ      : constant := 16#00000001#;
   FILE_SHARE_WRITE     : constant := 16#00000002#;
   INVALID_HANDLE_VALUE : constant := 16#FFFFFFFF#;

   ------------------------------------------------------------

   procedure Close (File_Handle : Handle);
   pragma Import (Stdcall, Close, "CloseHandle");

   ------------------------------------------------------------

   type File_Attribute is new Interfaces.Unsigned_32;

   FILE_ATTRIBUTE_READONLY   : constant File_Attribute := 16#00000001#;
   FILE_ATTRIBUTE_HIDDEN     : constant File_Attribute := 16#00000002#;
   FILE_ATTRIBUTE_SYSTEM     : constant File_Attribute := 16#00000004#;
   FILE_ATTRIBUTE_DIRECTORY  : constant File_Attribute := 16#00000010#;
   FILE_ATTRIBUTE_ARCHIVE    : constant File_Attribute := 16#00000020#;
   FILE_ATTRIBUTE_NORMAL     : constant File_Attribute := 16#00000080#;
   FILE_ATTRIBUTE_TEMPORARY  : constant File_Attribute := 16#00000100#;
   FILE_ATTRIBUTE_COMPRESSED : constant File_Attribute := 16#00000800#;
   FILE_ATTRIBUTE_OFFLINE    : constant File_Attribute := 16#00001000#;
   FILE_ATTRIBUTE_NO_FILE    : constant File_Attribute := 16#FFFFFFFF#;

   function GetFileAttributes (Filename : in Interfaces.C.char_array)
      return File_Attribute;
   pragma Import (Stdcall, GetFileAttributes, "GetFileAttributesA");

   ------------------------------------------------------------

   function Open (File_Name : in String) return Handle;

   function Systime_To_Time (UTC : System_Time) return Ada.Calendar.Time;

   ----------
   -- Open --
   ----------

   function Open (File_Name : in String) return Handle is
      use type Interfaces.Unsigned_32;
   begin
      return CreateFile (Interfaces.C.To_C (File_Name),
                         GENERIC_READ, FILE_SHARE_READ or FILE_SHARE_WRITE,
                         null, OPEN_EXISTING, 0, 0);
   end Open;

   ---------------------
   -- Systime_To_Time --
   ---------------------

   function Systime_To_Time (UTC : System_Time) return Ada.Calendar.Time
   is
   begin
      return Ada.Calendar.Time_Of (Integer (UTC.Year),
                                   Integer (UTC.Month),
                                   Integer (UTC.Day),
                                   Duration (Integer (UTC.Hour)   * 3600 +
                                             Integer (UTC.Minute) * 60 +
                                             Integer (UTC.Second)) +
                                   Duration (UTC.Milli_Second) / 1000);
   end Systime_To_Time;

   ---------------------
   -- Is_Regular_File --
   ---------------------

   function Is_Regular_File (Filename : in String) return Boolean
   is
      Attributes : File_Attribute;
   begin
      Attributes := GetFileAttributes (Interfaces.C.To_C (Filename));
      return Attributes /= FILE_ATTRIBUTE_NO_FILE and then
         ((Attributes and (FILE_ATTRIBUTE_HIDDEN or
                          FILE_ATTRIBUTE_SYSTEM or
                          FILE_ATTRIBUTE_DIRECTORY or
                          FILE_ATTRIBUTE_OFFLINE)) = 0);
   end Is_Regular_File;

   ------------------
   -- Is_Directory --
   ------------------

   function Is_Directory (Filename : in String) return Boolean
   is
      Attributes : File_Attribute;
   begin
      Attributes := GetFileAttributes (Interfaces.C.To_C (Filename));
      return Attributes /= FILE_ATTRIBUTE_NO_FILE and then
            (Attributes and FILE_ATTRIBUTE_DIRECTORY) /= 0;
   end Is_Directory;

   --------------------
   -- File_Timestamp --
   --------------------

   function File_Timestamp (Filename : in String) return Ada.Calendar.Time
   is
      File_Handle : Handle;
      Modified    : aliased File_Time;
      UTC         : System_Time;
      Status      : Interfaces.Unsigned_32;
   begin
      File_Handle := Open (Filename);
      if File_Handle /= INVALID_HANDLE_VALUE then
         Status := GetFileTime (File_Handle, null, null,
                                Modified'Unchecked_Access);
         Close (File_Handle);
         FileTimeToSystemTime (Modified, UTC);
      else
         raise No_Such_File;
      end if;
      return Systime_To_Time (UTC);
   end File_Timestamp;

   ---------------
   -- File_Size --
   ---------------

   function File_Size (Filename : in String)
     return Ada.Streams.Stream_Element_Offset
   is
      File_Handle : Handle;
      Low         : Interfaces.Unsigned_32;
   begin
      File_Handle := Open (Filename);
      if File_Handle /= INVALID_HANDLE_VALUE then
         Low := GetFileSize (File_Handle, null);
         Close (File_Handle);
      else
         raise No_Such_File;
      end if;
      return Ada.Streams.Stream_Element_Offset (Low);
   end File_Size;

   ---------------
   -- GMT_Clock --
   ---------------

   function GMT_Clock return Ada.Calendar.Time is
      UTC : System_Time;
   begin
      GetSystemTime (UTC);
      return Systime_To_Time (UTC);
   end GMT_Clock;

end AWS.OS_Lib;
