------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2020, AdaCore                     --
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

pragma Ada_2012;

--  This is the API to handle session data for each client connected

with Ada.Calendar;

private with AWS.Config;

package AWS.Session is

   use Ada;

   type Id is private;

   type Value_Kind is (Int, Str, Real, Bool, User);

   No_Session : constant Id;

   function Create return Id with
     Post => Create'Result /= No_Session;
   --  Create a new uniq Session Id

   function Creation_Stamp (SID : Id) return Calendar.Time;
   --  Returns the creation date of this session

   function Private_Key (SID : Id) return String;
   --  Return the private key for this session

   procedure Delete (SID : Id) with
     Post => not Exist (SID);
   --  Delete session, does nothing if SID does not exist.
   --  In most cases, the client browser will still send the cookie identifying
   --  the session on its next request. In such a case, the function
   --  AWS.Status.Timed_Out will return True, same as when the session was
   --  deleted automatically by AWS when it expired.
   --  The recommended practice is therefore to call
   --  AWS.Response.Set.Clear_Session when you send a response to the customer
   --  after deleting the session, so that the cookie is not sent again.

   function Delete_If_Empty (SID : Id) return Boolean;
   --  Delete session only if there is no key/value pairs.
   --  Returns True if session deleted.
   --  Need to delete not used just created session to avoid too many empty
   --  session creation.

   function Image (SID : Id) return String with Inline;
   --  Return ID image

   function Value (SID : String) return Id with Inline;
   --  Build an ID from a String, returns No_Session if SID is not recongnized
   --  as an AWS session ID.

   function Exist (SID : Id) return Boolean;
   --  Returns True if SID exist

   procedure Touch (SID : Id);
   --  Update to current time the timestamp associated with SID. Does nothing
   --  if SID does not exist.

   procedure Set (SID : Id; Key : String; Value : String);
   --  Set key/value pair for the SID

   procedure Set (SID : Id; Key : String; Value : Integer);
   --  Set key/value pair for the SID

   procedure Set (SID : Id; Key : String; Value : Float);
   --  Set key/value pair for the SID

   procedure Set (SID : Id; Key : String; Value : Boolean);
   --  Set key/value pair for the SID

   function Get (SID : Id; Key : String) return String with
     Inline => True,
     Post   => (not Exist (SID, Key) and then Get'Result'Length = 0)
               or else Exist (SID, Key);
   --  Returns the Value for Key in the session SID or the emptry string if
   --  key does not exist.

   function Get (SID : Id; Key : String) return Integer with
     Inline => True,
     Post   => (not Exist (SID, Key) and then Get'Result = 0)
               or else Exist (SID, Key);
   --  Returns the Value for Key in the session SID or the integer value 0 if
   --  key does not exist or is not an integer.

   function Get (SID : Id; Key : String) return Float with
     Inline => True,
     Post   => (not Exist (SID, Key) and then Get'Result = 0.0)
               or else Exist (SID, Key);
   --  Returns the Value for Key in the session SID or the float value 0.0 if
   --  key does not exist or is not a float.

   function Get (SID : Id; Key : String) return Boolean with
     Inline => True,
     Post   => (not Exist (SID, Key) and then Get'Result = False)
               or else Exist (SID, Key);
   --  Returns the Value for Key in the session SID or the boolean False if
   --  key does not exist or is not a boolean.

   generic
      type Data is private;
      Null_Data : Data;
   package Generic_Data is

      procedure Set (SID : Id; Key : String; Value : Data);
      --  Set key/value pair for the SID

      function Get (SID : Id; Key : String) return Data with Inline;
      --  Returns the Value for Key in the session SID or Null_Data if
      --  key does not exist.

   end Generic_Data;

   procedure Remove (SID : Id; Key : String) with
     Post => not Exist (SID, Key);
   --  Removes Key from the specified session

   function Exist (SID : Id; Key : String) return Boolean;
   --  Returns True if Key exist in session SID

   function Server_Count return Natural;
   --  Returns number of servers with sessions support

   function Length return Natural;
   --  Returns number of sessions

   function Length (SID : Id) return Natural;
   --  Returns number of key/value pairs in session SID

   procedure Clear with Post => Length = 0;
   --  Removes all sessions data

   ---------------
   -- Iterators --
   ---------------

   generic
      with procedure Action
        (N          : Positive;
         SID        : Id;
         Time_Stamp : Ada.Calendar.Time;
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
        (N          : Positive;
         Key, Value : String;
         Kind       : Value_Kind;
         Quit       : in out Boolean);
   procedure For_Every_Session_Data (SID : Id);
   --  Iterator which returns all the key/value pair defined for session SID.
   --  Quit is set to False by default, it is possible to control the iterator
   --  termination by setting its value to True. Note that in the Action
   --  procedure it is possible to use routines that read session's data (Get,
   --  Exist) but any routines which modify the data will block (i.e. Touch,
   --  Set, Remove, Delete will dead lock).

   --------------
   -- Lifetime --
   --------------

   procedure Set_Lifetime (Seconds : Duration);
   --  Set the lifetime for session data. At the point a session is deleted,
   --  reusing the session ID makes AWS.Status.Session_Timed_Out return True.

   function Get_Lifetime return Duration;
   --  Get current session lifetime for session data

   function Has_Expired (SID : Id) return Boolean;
   --  Returns true if SID should be considered as expired (ie there hasn't
   --  been any transaction on it since Get_Lifetime seconds. Such a session
   --  should be deleted. Calling this function is mostly internal to AWS, and
   --  sessions are deleted automatically when they expire.

   ----------------------
   -- Session Callback --
   ----------------------

   type Callback is access procedure (SID : Id);
   --  Callback procedure called when a sesssion is deleted from the server

   procedure Set_Callback (Callback : Session.Callback);
   --  Set the callback procedure to call when a session is deleted from the
   --  server. If Callback is Null the session's callback will be removed.

   ----------------
   -- Session IO --
   ----------------

   procedure Save (File_Name : String);
   --  Save all sessions data into File_Name

   procedure Load (File_Name : String);
   --  Restore all sessions data from File_Name

private

   type Id is new String (1 .. Config.Session_Id_Length);

   No_Session : constant Id := (others => ' ');

   task type Cleaner with Priority => Config.Session_Cleaner_Priority is
      entry Stop;
      entry Force;
   end Cleaner;
   --  Call Database.Clean every Session_Lifetime seconds

   type Cleaner_Access is access Cleaner;

   Cleaner_Task : Cleaner_Access;

   ---------------------
   -- Cleaner_Control --
   ---------------------

   protected Cleaner_Control is

      procedure Start
        (Check_Interval : Duration;
         Lifetime       : Duration;
         Init_Cleaner   : out Boolean);
      --  Launch the cleaner task the first time and does nothing after

      procedure Stop (Need_Release : out Boolean);
      --  Stop the cleaner task when there is no more server using it. Release
      --  is set to True if the Cleaner_Task can be released.

      function Server_Count return Natural;
      --  Returns number of servers with sessions support

   private
      S_Count : Natural := 0;
   end Cleaner_Control;

end AWS.Session;
