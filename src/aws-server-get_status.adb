------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2001                          --
--                                ACT-Europe                                --
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

with Templates_Parser;
with GNAT.Calendar.Time_IO;

with AWS.Session;
with AWS.Hotplug.Get_Status;
with AWS.Utils;

function AWS.Server.Get_Status (Server : in HTTP) return String is

   use Ada;
   use Templates_Parser;

   function Slot_Table return Translate_Table;
   --  returns the information for each slot

   function Session_Table return Translate_Table;
   --  returns session information

   -------------------
   -- Session_Table --
   -------------------

   function Session_Table return Translate_Table is

      Sessions           : Vector_Tag;
      Sessions_TS        : Vector_Tag;
      Sessions_Terminate : Vector_Tag;
      Keys               : Vector_Tag;
      Values             : Vector_Tag;
      M_Keys             : Matrix_Tag;
      M_Values           : Matrix_Tag;

      procedure For_Each_Key_Value
        (N          : in     Positive;
         Key, Value : in     String;
         Quit       : in out Boolean);
      --  add key/value pair to the list

      procedure For_Each_Session
        (N          : in Positive;
         SID        : in     Session.ID;
         Time_Stamp : in     Calendar.Time;
         Quit       : in out Boolean);
      --  add session SID to the list

      ------------------------
      -- For_Each_Key_Value --
      ------------------------

      procedure For_Each_Key_Value
        (N          : in     Positive;
         Key, Value : in     String;
         Quit       : in out Boolean) is
      begin
         Keys   := Keys & Key;
         Values := Values & Value;
      end For_Each_Key_Value;

      --------------------------
      -- Build_Key_Value_List --
      --------------------------

      procedure Build_Key_Value_List is
         new Session.For_Every_Session_Data (For_Each_Key_Value);

      ----------------------
      -- For_Each_Session --
      ----------------------

      procedure For_Each_Session
        (N          : in Positive;
         SID        : in     Session.ID;
         Time_Stamp : in     Calendar.Time;
         Quit       : in out Boolean)
      is
         use type Calendar.Time;
      begin
         Sessions    := Sessions & Session.Image (SID);

         Sessions_TS := Sessions_TS
           & GNAT.Calendar.Time_IO.Image (Time_Stamp, "%a %D %T");

         Sessions_Terminate := Sessions_Terminate
           & GNAT.Calendar.Time_IO.Image
           (Time_Stamp + Session.Get_Lifetime, "%a %D %T");

         Build_Key_Value_List (SID);

         M_Keys   := M_Keys & Keys;
         M_Values := M_Values & Values;

         Clear (Keys);
         Clear (Values);
      end For_Each_Session;

      ------------------------
      -- Build_Session_List --
      ------------------------

      procedure Build_Session_List is
         new Session.For_Every_Session (For_Each_Session);

   begin
      Build_Session_List;

      return Translate_Table'
        (Assoc ("SESSIONS_V",           Sessions),
         Assoc ("SESSIONS_TS_V",        Sessions_TS),
         Assoc ("SESSIONS_TERMINATE_V", Sessions_Terminate),
         Assoc ("KEYS_M",               M_Keys),
         Assoc ("VALUES_M",             M_Values));
   end Session_Table;

   ----------------
   -- Slot_Table --
   ----------------

   function Slot_Table return Translate_Table is

      Sock                  : Vector_Tag;
      Phase                 : Vector_Tag;
      Abortable             : Vector_Tag;
      Activity_Counter      : Vector_Tag;
      Slot_Activity_Counter : Vector_Tag;
      Activity_Time_Stamp   : Vector_Tag;
      Peer_Name             : Vector_Tag;

      Slot_Data             : Slot;

   begin
      for K in 1 .. CNF.Max_Connection (Server.Properties) loop
         Slot_Data := Server.Slots.Get (Index => K);

         if Slot_Data.Phase /= Closed then
            Sock := Sock & Integer (Sockets.Get_FD (Slot_Data.Sock.all));
         else
            Sock := Sock & '-';
         end if;

         Phase     := Phase & Slot_Phase'Image (Slot_Data.Phase);

         Abortable := Abortable
           & Server.Slots.Is_Abortable (Index => K, Mode => Force);

         Activity_Counter := Activity_Counter & Slot_Data.Activity_Counter;

         Peer_Name := Peer_Name & Slot_Data.Peername;

         Slot_Activity_Counter := Slot_Activity_Counter
           & Slot_Data.Slot_Activity_Counter;

         Activity_Time_Stamp := Activity_Time_Stamp &
           GNAT.Calendar.Time_IO.Image (Slot_Data.Phase_Time_Stamp,
                                        "%a %D %T");
      end loop;

      return Translate_Table'
        (Assoc ("SOCK_V",                  Sock),
         Assoc ("PEER_NAME_V",             Peer_Name),
         Assoc ("PHASE_V",                 Phase),
         Assoc ("ABORTABLE_V",             Abortable),
         Assoc ("SLOT_ACTIVITY_COUNTER_V", Slot_Activity_Counter),
         Assoc ("ACTIVITY_COUNTER_V",      Activity_Counter),
         Assoc ("ACTIVITY_TIME_STAMP_V",   Activity_Time_Stamp));
   end Slot_Table;

   use type Templates_Parser.Translate_Table;

   Admin_URI : constant String := CNF.Admin_URI (Server.Properties);

   Translations : constant Templates_Parser.Translate_Table
     := (Assoc ("SERVER_NAME",
                CNF.Server_Name (Server.Properties)),

         Assoc ("START_TIME",
                GNAT.Calendar.Time_IO.Image
                (Server.Start_Time, "%A %-d %B %Y, %T")),

         Assoc ("MAX_CONNECTION",
                CNF.Max_Connection (Server.Properties)),

         Assoc ("SERVER_PORT",
                CNF.Server_Port (Server.Properties)),

         Assoc ("SECURITY",
                CNF.Security (Server.Properties)),

         Assoc ("SERVER_SOCK",
                Integer (Sockets.Get_FD (Server.Sock))),

         Assoc ("VERSION",
                Version),

         Assoc ("SESSION",
                CNF.Session (Server.Properties)),

         Assoc ("SESSION_LIFETIME",
                Utils.Image (Session.Get_Lifetime)),

         Assoc ("SESSION_CLEANUP_INTERVAL",
                Utils.Image (CNF.Session_Cleanup_Interval)),

         Assoc ("LOGO",
                Admin_URI & "-logo"),

         Assoc ("LOG",
                Log.Is_Active (Server.Log)),

         Assoc ("LOG_FILE",
                Log.Filename (Server.Log)),

         Assoc ("LOG_MODE",
                Log.Split_Mode'Image (Log.Mode (Server.Log))),


         Assoc ("ADMIN",
                Admin_URI))
     & Slot_Table
     & Session_Table
     & Hotplug.Get_Status (Server.Filters);

begin
   return Templates_Parser.Parse
     (CNF.Status_Page (Server.Properties), Translations);
end AWS.Server.Get_Status;
