------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2001                          --
--                               ACT-Europe                                 --
--                                                                          --
--  Authors: Dmitriy Anisimov - Pascal Obry                                 --
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

--  This package handle the logging facility for AWS. The log file is named
--  '<progname>-Y-M-D.log' and is written by default in the directory where
--  the server is launched, see configuration file.

with Ada.Text_IO;
with Ada.Strings.Unbounded;

with AWS.Messages;
with AWS.Status;

package AWS.Log is

   type Object is limited private;

   type Split_Mode is (None, Each_Run, Daily, Monthly);
   --  It specifies when to create a new log file.
   --  None     : all log info gets accumulated into the same file.
   --  Each_Run : a new log file is created each time the server is started.
   --  Daily    : a new log file is created each day.
   --  Monthly  : a new log file is created each month.

   Not_Specified : constant String := "";

   procedure Start
     (Log             : in out Object;
      Split           : in     Split_Mode := None;
      File_Directory  : in     String     := Not_Specified;
      Filename_Prefix : in     String     := Not_Specified);
   --  Activate server's activity logging. Split indicate the way the log file
   --  should be created. Log_File_Prefix is the log filename prefix. If it is
   --  not specified the default prefix is the program name.

   procedure Write
     (Log          : in out Object;
      Connect_Stat : in     Status.Data;
      Answer_Stat  : in     Messages.Status_Code;
      Peername     : in     String);
   --  Write log info if activated (i.e. Start routine above as been called).

   procedure Stop (Log : in out Object);
   --  Stop logging activity.

   function Is_Active (Log : in Object) return Boolean;
   --  Returns True if Log is activated.

   function Filename (Log : in Object) return String;
   --  Returns current log filename or the empty string if the log is not
   --  activated.

   function Mode (Log : in Object) return Split_Mode;
   --  Returns the split mode. None will be returned if log is not activated.

private

   use Ada;
   use Ada.Strings.Unbounded;

   type Object is limited record
      File            : Text_IO.File_Type;
      File_Directory  : Unbounded_String;
      Filename_Prefix : Unbounded_String;
      Split           : Split_Mode;
      Current_Tag     : Positive;
   end record;

end AWS.Log;
