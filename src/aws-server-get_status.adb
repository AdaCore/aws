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

with GNAT.Calendar.Time_IO;

with AWS.Hotplug.Get_Status;
with AWS.Session;
with AWS.Templates;
with AWS.Utils;

function AWS.Server.Get_Status (Server : in HTTP) return String is

   use Ada;
   use AWS.Templates;

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
        (N          : in     Positive;
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
         Quit       : in out Boolean)
      is
         pragma Warnings (Off, N);
         pragma Warnings (Off, Quit);
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
        (N          : in     Positive;
         SID        : in     Session.ID;
         Time_Stamp : in     Calendar.Time;
         Quit       : in out Boolean)
      is
         pragma Warnings (Off, N);
         pragma Warnings (Off, Quit);
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

      --  Avoid : may be referenced before it has a value
      pragma Warnings (Off, Sock);
      pragma Warnings (Off, Phase);
      pragma Warnings (Off, Abortable);
      pragma Warnings (Off, Activity_Counter);
      pragma Warnings (Off, Slot_Activity_Counter);
      pragma Warnings (Off, Activity_Time_Stamp);
      pragma Warnings (Off, Peer_Name);

      Slot_Data             : Slot;

   begin
      for K in 1 .. CNF.Max_Connection (Server.Properties) loop
         Slot_Data := Server.Slots.Get (Index => K);

         declare
            SD : constant Socket_Data
              := Server.Slots.Get_Socket_Info (Index => K);
         begin
            Sock      := Sock      & SD.FD;
            Peer_Name := Peer_Name & SD.Peername;
         end;

         Phase     := Phase & Slot_Phase'Image (Slot_Data.Phase);

         Abortable := Abortable
           & Server.Slots.Is_Abortable (Index => K, Mode => Force);

         Activity_Counter := Activity_Counter & Slot_Data.Activity_Counter;

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

   use type Templates.Translate_Table;

   Admin_URI : constant String := CNF.Admin_URI (Server.Properties);

   Translations : constant Templates.Translate_Table
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
                Integer (Net.Std.Get_FD (Server.Sock))),

         Assoc ("VERSION",
                Version),

         Assoc ("SESSION",
                CNF.Session (Server.Properties)),

         Assoc ("SESSION_LIFETIME",
                Utils.Image (Session.Get_Lifetime)),

         Assoc ("SESSION_CLEANUP_INTERVAL",
                Utils.Image (CNF.Session_Cleanup_Interval)),

         Assoc ("CLEANER_WAIT_FOR_CLIENT_TIMEOUT",
                Utils.Image
                  (CNF.Cleaner_Wait_For_Client_Timeout (Server.Properties))),

         Assoc ("CLEANER_CLIENT_HEADER_TIMEOUT",
                Utils.Image
                  (CNF.Cleaner_Client_Header_Timeout (Server.Properties))),

         Assoc ("CLEANER_CLIENT_DATA_TIMEOUT",
                Utils.Image
                  (CNF.Cleaner_Client_Data_Timeout (Server.Properties))),

         Assoc ("CLEANER_SERVER_RESPONSE_TIMEOUT",
                Utils.Image
                  (CNF.Cleaner_Server_Response_Timeout (Server.Properties))),

         Assoc ("FORCE_WAIT_FOR_CLIENT_TIMEOUT",
                Utils.Image
                  (CNF.Force_Wait_For_Client_Timeout (Server.Properties))),

         Assoc ("FORCE_CLIENT_HEADER_TIMEOUT",
                Utils.Image
                  (CNF.Force_Client_Header_Timeout (Server.Properties))),

         Assoc ("FORCE_CLIENT_DATA_TIMEOUT",
                Utils.Image
                  (CNF.Force_Client_Data_Timeout (Server.Properties))),

         Assoc ("FORCE_SERVER_RESPONSE_TIMEOUT",
                Utils.Image
                  (CNF.Force_Server_Response_Timeout (Server.Properties))),

         Assoc ("SEND_TIMEOUT",
                Utils.Image (CNF.Send_Timeout (Server.Properties))),

         Assoc ("RECEIVE_TIMEOUT",
                Utils.Image (CNF.Receive_Timeout (Server.Properties))),

         Assoc ("ACCEPT_QUEUE_SIZE",
                Utils.Image (CNF.Accept_Queue_Size (Server.Properties))),

         Assoc ("STATUS_PAGE",
                CNF.Status_Page (Server.Properties)),

         Assoc ("LOGO",
                Admin_URI & "-logo"),

         Assoc ("LOG",
                Log.Is_Active (Server.Log)),

         Assoc ("LOG_FILE",
                Log.Filename (Server.Log)),

         Assoc ("LOG_MODE",
                Log.Split_Mode'Image (Log.Mode (Server.Log))),

         Assoc ("ADMIN",
                Admin_URI),

         Assoc ("UPLOAD_DIRECTORY",
                CNF.Upload_Directory (Server.Properties)))
   & Slot_Table
   & Session_Table
   & Hotplug.Get_Status (Server.Filters);

begin
   return Templates.Parse
     (CNF.Status_Page (Server.Properties), Translations);
end AWS.Server.Get_Status;
