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

--  This package handle the logging facility for AWS. The log file is named
--  '<progname>-Y-M-D.log' and is written by default in the directory where
--  the server is launched, see configuration file.
--
--  Note that this package is used internally by AWS to log server requests
--  but it can also be used by users to handle application's log.
--
--  This package is thread safe.

with Ada.Finalization;
with Ada.Text_IO;
with Ada.Strings.Unbounded;

with AWS.Containers.String_Vectors;
with AWS.Headers;
with AWS.Status;
with AWS.Response;
with AWS.Messages;
with AWS.Utils;

private with Ada.Containers.Indefinite_Hashed_Maps;
private with Ada.Strings.Hash;

package AWS.Log is

   type Object is limited private;
   --  A log object. It must be activated by calling Start below

   type Split_Mode is (None, Each_Run, Daily, Monthly);
   --  It specifies when to create a new log file.
   --  None     : all log info gets accumulated into the same file.
   --  Each_Run : a new log file is created each time the server is started.
   --  Daily    : a new log file is created each day.
   --  Monthly  : a new log file is created each month.

   type Fields_Table is private;
   --  Type to keep record for Extended Log File Format

   Empty_Fields_Table : constant Fields_Table;

   Not_Specified : constant String;

   procedure Start
     (Log             : in out Object;
      Split           : in     Split_Mode := None;
      File_Directory  : in     String     := Not_Specified;
      Filename_Prefix : in     String     := Not_Specified;
      Auto_Flush      : in     Boolean    := False);
   --  Activate server's activity logging. Split indicate the way the log file
   --  should be created. Log_File_Prefix is the log filename prefix. If it is
   --  not specified the default prefix is the program name.
   --  Set Auto_Flush to True if you want every write to the log to be flushed
   --  (not buffered). Auto_Flush should be set to True only for logs with few
   --  entries per second as the flush has a performance penalty.

   procedure Register_Field (Log : in out Object; Id : in String);
   --  Register field to be written into extended log format

   procedure Set_Field
     (Log : in Object; Data : in out Fields_Table; Id, Value : in String);
   --  Set field value into the extended log record. Data could be used only
   --  in one task and with one log file. Different tasks could write own Data
   --  using the Write routine with Fields_Table parameter type.

   procedure Set_Header_Fields
     (Log    : in     Object;
      Data   : in out Fields_Table;
      Prefix : in     String;
      Header : in     AWS.Headers.List);
   --  Set header fields into extended log record.
   --  Name of the header fields would be <Prefix>(<Header_Name>).
   --  Prefix should be "cs" - Client to Server or "sc" - Server to Client.

   procedure Write (Log : in out Object; Data : in out Fields_Table);
   --  Write extended format record to log file and prepare record for the next
   --  data. It is not allowed to use same Fields_Table with different extended
   --  logs.

   procedure Write
     (Log          : in out Object;
      Connect_Stat : in     Status.Data;
      Answer       : in     Response.Data);
   --  Write log info if activated (i.e. Start routine above has been called)

   procedure Write
     (Log            : in out Object;
      Connect_Stat   : in     Status.Data;
      Status_Code    : in     Messages.Status_Code;
      Content_Length : in     Response.Content_Length_Type);
   --  Write log info if activated (i.e. Start routine above has been called).
   --  This version separated the Content_Length from Status.Data, this is
   --  required for example in the case of a user defined stream content. See
   --  AWS.Resources.Stream.

   procedure Write
     (Log          : in out Object;
      Connect_Stat : in     Status.Data;
      Data         : in     String);
   --  Write user's log info if activated.  (i.e. Start routine above has been
   --  called).

   procedure Write (Log : in out Object; Data : in String);
   --  Write Data into the log file. This Data is unstructured, only a time
   --  tag prefix is prepended to Data. This routine is designed to be used
   --  for user's info in error log file.

   procedure Flush (Log : in out Object);
   --  Flush the data to the Log file, for be able to see last logged
   --  messages.

   procedure Stop (Log : in out Object);
   --  Stop logging activity

   function Is_Active (Log : in Object) return Boolean;
   --  Returns True if Log is activated

   function Filename (Log : in Object) return String;
   --  Returns current log filename or the empty string if the log is not
   --  activated.

   function Mode (Log : in Object) return Split_Mode;
   --  Returns the split mode. None will be returned if log is not activated

private

   use Ada;
   use Ada.Strings.Unbounded;

   package Strings_Positive is new Ada.Containers.Indefinite_Hashed_Maps
     (String, Positive, Ada.Strings.Hash, "=", "=");

   package SV renames AWS.Containers.String_Vectors;

   type Fields_Table is record
      Values : SV.Vector;
   end record;

   Empty_Fields_Table : constant Fields_Table := (Values => SV.Empty_Vector);

   Not_Specified : constant String := "";

   type Object is new Ada.Finalization.Limited_Controlled with record
      File            : Text_IO.File_Type;
      Extended_Fields : Strings_Positive.Map;
      Header_Written  : Boolean;
      File_Directory  : Unbounded_String;
      Filename_Prefix : Unbounded_String;
      Split           : Split_Mode := None;
      Current_Tag     : Positive;
      Semaphore       : Utils.Semaphore;
      Auto_Flush      : Boolean;
   end record;

   overriding procedure Finalize (Log : in out Object);

end AWS.Log;
