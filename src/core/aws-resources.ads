------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2002-2014, AdaCore                     --
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
--  covered by the GNU Public License.                                      --
------------------------------------------------------------------------------

with Ada.Calendar;
with Ada.Streams;
with AWS.Utils;

private with Ada.Unchecked_Deallocation;

package AWS.Resources is

   use Ada.Streams;

   Resource_Error : exception;

   type File_Type is limited private;

   type File_Instance is (None, Plain, GZip, Both);
   --  None  : No instance of this file present.
   --  Plain : A non-compressed version of this file exists.
   --  GZip  : A gzip encoded version of this file exists.
   --  Both  : Both versions of this file exists.

   function "or" (I1, I2 : File_Instance) return File_Instance;
   --  Returns the union of I1 and I2

   subtype Content_Length_Type is Stream_Element_Offset;

   Undefined_Length : constant Content_Length_Type;
   --  Undefined length could be used when we do not know the message stream
   --  length at the start of transfer. The end of message could be determined
   --  by the chunked transfer-encoding in the HTTP/1.1, or by the closing
   --  connection in the HTTP/1.0.

   procedure Open
     (File : out File_Type;
      Name : String;
      Form : String    := "");
   --  Open file in mode In_File. Only reading from the file is supported.
   --  This procedure open the in-memory (embedded) file if present, otherwise
   --  the file on disk is opened. Note that if Name file is not found, it
   --  checks for Name & ".gz" and unzipped the file content in this case.

   procedure Open
     (File : out File_Type;
      Name : String;
      Form : String    := "";
      GZip : in out Boolean);
   --  Open file in mode In_File. Only reading from the file is supported.
   --  This procedure open the in-memory (embedded) file if present, otherwise
   --  the file on disk is opened. If GZip parameter is False this call is
   --  equivalent to the Open routine above. If GZip is True this routine will
   --  first check for the compressed version of the resource (Name & ".gz"),
   --  if found GZip output value will remain True. If GZip value is True and
   --  the compressed version of the resource does not exist it looks for
   --  non-compressed version and set GZip value to False.

   procedure Reset (Resource : in out File_Type);
   --  Reset the file, reading will restart at the beginning

   procedure Set_Index
     (Resource : in out File_Type;
      To       : Stream_Element_Offset);
   --  Set the position in the stream, next Read will start at the position
   --  whose index is To. If To is outside the content the index is set to
   --  Last + 1 to ensure that next End_Of_File will return True.

   procedure Close (Resource : in out File_Type);
   --  Close the file

   procedure Read
     (Resource : in out File_Type;
      Buffer   : out Stream_Element_Array;
      Last     : out Stream_Element_Offset);
   --  Returns a set of bytes from the file

   procedure Get_Line
     (Resource  : in out File_Type;
      Buffer    : out String;
      Last      : out Natural);
   --  Returns a line from the file. A line is a set of character terminated
   --  by ASCII.LF (UNIX style EOL) or ASCII.CR+ASCII.LF (DOS style EOL).

   function End_Of_File (Resource : File_Type) return Boolean;
   --  Returns true if there is no more data to read

   function LF_Terminated (Resource : File_Type) return Boolean;
   --  Returns True if last line returned by Get_Line was terminated with a LF
   --  or CR+LF on DOS based systems.

   function Size (Resource : File_Type) return Content_Length_Type;
   --  Returns the size (in bytes) of the resource. If the size of the
   --  resource is not defined, the routine Size returns Undefined_Length
   --  value.

   function Exist (Name : String) return File_Instance;
   --  Return GZip if only file Name & ".gz" exists.
   --  Return Plain if only file Name exists.
   --  Return Both if both file Name and Name & ".gz" exists.
   --  Return None if files neither Name nor Name & ".gz" exist.

   function Is_Regular_File (Name : String) return Boolean;
   --  Returns True if Filename is a regular file and is readable. Checks
   --  first for in memory file then for disk file.

   function File_Size (Name : String) return Utils.File_Size_Type;
   --  Returns Filename's size in bytes. Checks first for in memory file
   --  then for disk file.

   function File_Timestamp (Name : String) return Ada.Calendar.Time;
   --  Get the time for last modification to a file in UTC/GMT. Checks first
   --  for in memory file then for disk file.

private

   Undefined_Length : constant Content_Length_Type := -1;

   GZip_Ext : constant String := ".gz";

   type File_Tagged is abstract tagged limited record
      LFT : Boolean; -- LF terminated state
   end record;

   --  Abstract file, operations below must be implemented. The goal here is
   --  to abstract the file location. Currently there is two implementations,
   --  one for files on a hard disk and files in memory (array of bytes).

   type File_Type is access all File_Tagged'Class;

   function Is_GZip (Name : String) return Boolean;
   --  Return true if filename is with .gz extension

   function End_Of_File (Resource : File_Tagged) return Boolean is abstract;

   procedure Read
     (Resource : in out File_Tagged;
      Buffer   : out Stream_Element_Array;
      Last     : out Stream_Element_Offset) is abstract;

   function Size
     (Resource : File_Tagged) return Stream_Element_Offset is abstract;

   procedure Close (File : in out File_Tagged) is abstract;

   procedure Reset (File : in out File_Tagged) is abstract;

   procedure Set_Index
     (File : in out File_Tagged;
      To   : Stream_Element_Offset) is abstract;

   procedure Unchecked_Free is
      new Ada.Unchecked_Deallocation (Resources.File_Tagged'Class, File_Type);

end AWS.Resources;
