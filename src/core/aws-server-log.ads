------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2003-2012, AdaCore                     --
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

with AWS.Log;

package AWS.Server.Log is

   ------------------
   -- Standard Log --
   ------------------

   procedure Start
     (Web_Server      : in out HTTP;
      Split_Mode      : AWS.Log.Split_Mode := AWS.Log.None;
      Filename_Prefix : String             := "";
      Auto_Flush      : Boolean            := False);
   --  Activate server's logging activity. See AWS.Log. If Auto_Flush is True
   --  the file will be flushed after all written data.

   procedure Start
     (Web_Server : in out HTTP;
      Callback   : AWS.Log.Callback;
      Name       : String);
   --  Activate the Web_Server access log and direct all data to the Callback.
   --  The Name String is returned when the Name function is called. It is a
   --  simple identifier, that serves no other purpose than to give the
   --  Callback a label.

   function Name (Web_Server : HTTP) return String;
   --  Return the name of the Log or an empty string if one is not active. If
   --  an external writer is used to handle the access log, then the name of
   --  that writer is returned. See the Start procedure for starting the access
   --  log with a Callback.

   procedure Stop (Web_Server : in out HTTP);
   --  Stop server's logging activity. See AWS.Log

   function Is_Active (Web_Server : HTTP) return Boolean;
   --  Returns True if the Web Server log has been activated

   procedure Flush (Web_Server : in out HTTP);
   --  Flush the server log.
   --  Note that error log does not need to be flushed because it is always
   --  flushed by default. If a Callback procedure is used to handle the log
   --  data, then calling Flush does nothing.

   ---------------
   -- Error Log --
   ---------------

   procedure Start_Error
     (Web_Server      : in out HTTP;
      Split_Mode      : AWS.Log.Split_Mode := AWS.Log.None;
      Filename_Prefix : String             := "");
   --  Activate server's logging activity. See AWS.Log

   procedure Start_Error
     (Web_Server : in out HTTP;
      Callback   : AWS.Log.Callback;
      Name       : String);
   --  Activate the Web_Server error log and direct all data to the Callback.
   --  The Name String is returned when the Error_Name function is called. It
   --  is a simple identifier, that serves no other purpose than to give the
   --  Callback a label.

   function Error_Name (Web_Server : HTTP) return String;
   --  Return the name of the Error Log or an empty string if one is not
   --  active. If a Callback is used to handle the error log, then the name of
   --  the Callback is returned. See the Start_Error procedure for starting the
   --  error log with a Callback.

   procedure Stop_Error (Web_Server : in out HTTP);
   --  Stop server's logging activity. See AWS.Log

   function Is_Error_Active (Web_Server : HTTP) return Boolean;
   --  Returns True if the Web Server error log has been activated

end AWS.Server.Log;
