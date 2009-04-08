------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2003-2009, AdaCore                     --
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

   function Name (Web_Server : HTTP) return String;
   --  Return the name of the Log or an empty string if one is not active

   procedure Stop (Web_Server : in out HTTP);
   --  Stop server's logging activity. See AWS.Log

   function Is_Active (Web_Server : HTTP) return Boolean;
   --  Returns True if the Web Server log has been activated

   procedure Flush (Web_Server : in out HTTP);
   --  Flush the server log.
   --  Note that error log does not need to be flushed because it is always
   --  flushed by default.

   ---------------
   -- Error Log --
   ---------------

   procedure Start_Error
     (Web_Server      : in out HTTP;
      Split_Mode      : AWS.Log.Split_Mode := AWS.Log.None;
      Filename_Prefix : String             := "");
   --  Activate server's logging activity. See AWS.Log

   function Error_Name (Web_Server : HTTP) return String;
   --  Return the name of the Error Log or an empty string if one is not active

   procedure Stop_Error (Web_Server : in out HTTP);
   --  Stop server's logging activity. See AWS.Log

   function Is_Error_Active (Web_Server : HTTP) return Boolean;
   --  Returns True if the Web Server error log has been activated

end AWS.Server.Log;
