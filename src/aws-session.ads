------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                            Copyright (C) 2000                            --
--                      Dmitriy Anisimov - Pascal Obry                      --
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

with Ada.Calendar;

package AWS.Session is

   type ID is private;

   Default_Session_Lifetime : constant Duration := 10.0 * 60.0;
   --  This is the default lifetime for session data. A session data that have
   --  been not used during this time will be removed from the server. It is
   --  possible to change the lifetime if session data. See below.

   function Create return ID;
   --  Create a new uniq Session ID.

   function Image (SID : in ID) return String;
   --  Return ID image

   function Value (SID : in String) return ID;
   --  Build an ID from a String.

   function Exist (SID : in ID) return Boolean;
   --  Returns True if SID exist

   procedure Set (SID   : in ID;
                  Key   : in String;
                  Value : in String);
   --  Set key/pair value for the SID

   procedure Set (SID   : in ID;
                  Key   : in String;
                  Value : in Integer);
   --  Set key/pair value for the SID

   procedure Set (SID   : in ID;
                  Key   : in String;
                  Value : in Float);
   --  Set key/pair value for the SID

   function Get (SID : in ID;
                 Key : in String)
     return String;
   --  Returns the Value for Key in the session SID or the emptry string if
   --  key does not exist.

   function Get (SID : in ID;
                 Key : in String)
     return Integer;
   --  Returns the Value for Key in the session SID or the integer value 0 if
   --  key does not exist.

   function Get (SID : in ID;
                 Key : in String)
     return Float;
   --  Returns the Value for Key in the session SID or the float value 0.0 if
   --  key does not exist.

   function Exist (SID : in ID;
                   Key : in String)
     return Boolean;
   --  Returns True if Key exist in session SID.

   generic
      with procedure Action (N          : in     Positive;
                             SID        : in     ID;
                             Time_Stamp : in     Ada.Calendar.Time;
                             Quit       : in out Boolean);
   procedure For_Every_Session;
   --  Iterator which call Action for every active session.

   generic
      with procedure Action (N          : in     Positive;
                             Key, Value : in     String;
                             Quit       : in out Boolean);
   procedure For_Every_Session_Data (SID : in ID);
   --  Iterator which returns all the key/value pair defined for session SID.

   procedure Set_Session_Lifetime (Seconds : in Duration);
   --  Set the lifetime for session data.

   procedure Start;
   --  Start session cleaner task.

private

   type ID is new Integer;

end AWS.Session;
