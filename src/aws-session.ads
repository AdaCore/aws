------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2003                          --
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
--  covered by the  GNU Public License.                                     --
------------------------------------------------------------------------------

--  $Id$

--  This is the API to handle session data for each client connected.

with Ada.Calendar;

package AWS.Session is

   type ID is private;

   No_Session : constant ID;

   function Create return ID;
   --  Create a new uniq Session ID.

   procedure Delete (SID : in ID);
   --  Delete session, does nothing if SID does not exists.

   function Image (SID : in ID) return String;
   pragma Inline (Image);
   --  Return ID image

   function Value (SID : in String) return ID;
   pragma Inline (Value);
   --  Build an ID from a String, returns No_Session if SID is not recongnized
   --  as an AWS session ID.

   function Exist (SID : in ID) return Boolean;
   --  Returns True if SID exist

   procedure Touch (SID : in ID);
   --  Update to current time the timestamp associated with SID. Does nothing
   --  if SID does not exists.

   procedure Set
     (SID   : in ID;
      Key   : in String;
      Value : in String);
   --  Set key/pair value for the SID.

   procedure Set
     (SID   : in ID;
      Key   : in String;
      Value : in Integer);
   --  Set key/pair value for the SID.

   procedure Set
     (SID   : in ID;
      Key   : in String;
      Value : in Float);
   --  Set key/pair value for the SID.

   procedure Set
     (SID   : in ID;
      Key   : in String;
      Value : in Boolean);
   --  Set key/pair value for the SID.

   function Get (SID : in ID; Key : in String) return String;
   pragma Inline (Get);
   --  Returns the Value for Key in the session SID or the emptry string if
   --  key does not exist.

   function Get (SID : in ID; Key : in String) return Integer;
   pragma Inline (Get);
   --  Returns the Value for Key in the session SID or the integer value 0 if
   --  key does not exist or is not an integer.

   function Get (SID : in ID; Key : in String) return Float;
   pragma Inline (Get);
   --  Returns the Value for Key in the session SID or the float value 0.0 if
   --  key does not exist or is not a float.

   function Get (SID : in ID; Key : in String) return Boolean;
   pragma Inline (Get);
   --  Returns the Value for Key in the session SID or the boolean False if
   --  key does not exist or is not a boolean.

   procedure Remove
     (SID : in ID;
      Key : in String);
   --  Removes Key from the specified session.

   function Exist
     (SID : in ID;
      Key : in String)
      return Boolean;
   --  Returns True if Key exist in session SID.

   ---------------
   -- Iterators --
   ---------------

   generic
      with procedure Action
        (N          : in     Positive;
         SID        : in     ID;
         Time_Stamp : in     Ada.Calendar.Time;
         Quit       : in out Boolean);
   procedure For_Every_Session;
   --  Iterator which call Action for every active session. N is the SID
   --  order. Time_Stamp is the time when SID was updated for the last
   --  time. Quit is set to False by default, it is possible to control the
   --  iterator termination by setting its value to True. Note that in the
   --  Action procedure it is possible to use routines that read session's
   --  data (Get, Exist) but any routines which modify the data will block
   --  (i.e. Touch, Set, Remove, Delete will dead lock).

   generic
      with procedure Action
        (N          : in     Positive;
         Key, Value : in     String;
         Quit       : in out Boolean);
   procedure For_Every_Session_Data (SID : in ID);
   --  Iterator which returns all the key/value pair defined for session SID.
   --  Quit is set to False by default, it is possible to control the iterator
   --  termination by setting its value to True. Note that in the Action
   --  procedure it is possible to use routines that read session's data (Get,
   --  Exist) but any routines which modify the data will block (i.e. Touch,
   --  Set, Remove, Delete will dead lock).

   --------------
   -- Lifetime --
   --------------

   procedure Set_Lifetime (Seconds : in Duration);
   --  Set the lifetime for session data.

   function Get_Lifetime return Duration;
   --  Get current session lifetime for session data.

   ----------------------
   -- Session Callback --
   ----------------------

   type Callback is access procedure (SID : in ID);
   --  Callback procedure called when a sesssion is deleted from the server.

   procedure Set_Callback (Callback : in Session.Callback);
   --  Set the callback procedure to call when a session is deleted from the
   --  server. If Callback is Null the session's callback will be removed.

   ----------------
   -- Session IO --
   ----------------

   procedure Save (File_Name : in String);
   --  Save all sessions data into File_Name.

   procedure Load (File_Name : in String);
   --  Restore all sessions data from File_Name.

private

   type ID is new String (1 .. 11);

   No_Session : constant ID := (others => ' ');

   task type Cleaner is
      entry Stop;
   end Cleaner;
   --  Call Database.Clean every Session_Lifetime seconds.

   type Cleaner_Access is access Cleaner;

   Cleaner_Task : Cleaner_Access;

   ---------------------
   -- Cleaner_Control --
   ---------------------

   protected Cleaner_Control is

      procedure Start
        (Session_Check_Interval : in Duration;
         Session_Lifetime       : in Duration);
      --  Launch the cleaner task the first time and does nothing after.

      procedure Stop (Need_Release : out Boolean);
      --  Stop the cleaner task when there is no more server using it. Release
      --  is set to True if the Cleaner_Task can be released.

   private
      Server_Count : Natural := 0;
   end Cleaner_Control;

end AWS.Session;
