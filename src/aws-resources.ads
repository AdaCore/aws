------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2002-2003                          --
--                                ACT-Europe                                --
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
--  covered by the GNU Public License.                                      --
------------------------------------------------------------------------------

--  $Id$

with Ada.Calendar;
with Ada.Streams;
with Ada.Unchecked_Deallocation;

package AWS.Resources is

   use Ada.Streams;

   Resource_Error : exception;

   type File_Type is limited private;

   subtype Content_Length_Type is Stream_Element_Offset;

   Undefined_Length : constant Content_Length_Type;
   --  Undefined length could be used when we do not know the message stream
   --  length at the start of transfer. The end of message could be determined
   --  by the chunked transfer-encoding in the HTTP/1.1, or by the closing
   --  connection in the HTTP/1.0.

   procedure Open
     (File :    out File_Type;
      Name : in     String;
      Form : in     String    := "");
   --  Open file in mode In_File. Only reading from the file is supported.
   --  This procedure open the in-memory (embedded) file if present, otherwise
   --  the file on disk is opened. Note that file content could be gotten from
   --  file (embedded or on-disk) with the filename Name & ".gz" if file with
   --  filename Name does not exists, output would be unzipped in this case.

   procedure Open
     (File :    out File_Type;
      Name : in     String;
      Form : in     String    := "";
      GZip : in out Boolean);
   --  Open file in mode In_File. Only reading from the file is supported.
   --  This procedure open the in-memory (embedded) file if present, otherwise
   --  the file on disk is opened. If in value of GZip parameter is False
   --  data output would be the same as in previous Open routine. If GZip in
   --  value is True routine would try to open file with name Name & ".gz"
   --  first and provide GZipped output for File. GZip out value would be True
   --  in this case. If in GZip value is True and file with name Name & ".gz"
   --  does not exists, routine would try to open file with name Name and
   --  put out GZip value to False.
   --  Note, if both files Name and Name & ".gz" exists, file opened is depend
   --  on GZip in value. The user should be responsible for the WWW_Root
   --  folder and embedded resources contents integrity. Sometimes it would be
   --  usable to provide 2 different content for HTTP clients supported and
   --  not suported gzip decoding. For example web designer want to give
   --  access to big html file for all http clients. /file.html URI would
   --  give either direct access to file.html.gz or to the small page with
   --  link to file.html.gz depend on http client gzip decoding support.

   procedure Reset (Resource : in out File_Type);
   --  Reset the file, reading will restart at the beginning

   procedure Close (Resource : in out File_Type);
   --  Close the file

   procedure Read
     (Resource : in out File_Type;
      Buffer   :    out Stream_Element_Array;
      Last     :    out Stream_Element_Offset);
   --  Returns a set of bytes from the file

   procedure Get_Line
     (Resource  : in out File_Type;
      Buffer    :    out String;
      Last      :    out Natural);
   --  Returns a line from the file. A line is a set of character terminated
   --  by ASCII.LF (UNIX style EOF) or ASCII.CR+ASCII.LF (DOS style EOF).

   function End_Of_File (Resource : in File_Type) return Boolean;
   --  Returns true if there is no more data to read.

   function LF_Terminated (Resource : in File_Type) return Boolean;
   --  Returns True if last line returned by Get_Line was terminated with a LF
   --  or CR+LF on DOS based systems.

   function Size (Resource : in File_Type) return Content_Length_Type;
   --  Returns the size (in bytes) of the resource. If the size of the
   --  resource is not defined, the routine Size returns Undefined_Length
   --  value.

   function Is_Regular_File (Name : in String) return Boolean;
   --  Returns True if Filename is a regular file and is readable. Checks
   --  first for in memory file then for disk file.

   function File_Size (Name : in String) return Stream_Element_Offset;
   --  Returns Filename's size in bytes. Checks first for in memory file
   --  then for disk file.

   function File_Timestamp (Name : in String) return Ada.Calendar.Time;
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
   --  Return true if filename is with .gz extension.

   function End_Of_File
     (Resource : in File_Tagged)
      return Boolean
      is abstract;

   procedure Read
     (Resource : in out File_Tagged;
      Buffer   :    out Stream_Element_Array;
      Last     :    out Stream_Element_Offset)
      is abstract;

   function Size
     (Resource : in File_Tagged)
      return Stream_Element_Offset
      is abstract;

   procedure Close (File : in out File_Tagged)
      is abstract;

   procedure Reset (File : in out File_Tagged)
      is abstract;

   procedure Free is
      new Ada.Unchecked_Deallocation (Resources.File_Tagged'Class, File_Type);

end AWS.Resources;
