------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                            Copyright (C) 2000                            --
--                      Dmitriy Anisimkov & Pascal Obry                     --
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

with Interfaces.C;
with Templates_Parser;
with GNAT.Calendar.Time_IO;

function AWS.Server.Get_Status (Server : in HTTP) return String is

   function Slot_Table return Templates_Parser.Translate_Table;
   --  returns the information for each slot

   ----------------
   -- Slot_Table --
   ----------------

   function Slot_Table return Templates_Parser.Translate_Table is
      Sock                : Unbounded_String;
      Opened              : Unbounded_String;
      Abortable           : Unbounded_String;
      Quit                : Unbounded_String;
      Activity_Counter    : Unbounded_String;
      Activity_Time_Stamp : Unbounded_String;

      Slot_Data           : Slot;
   begin
      for K in 1 .. Server.Max_Connection loop
         Slot_Data := Server.Slots.Get (Index => K);

         if K > 1 then
            Sock                := Sock   & '|';
            Opened              := Opened & '|';
            Abortable           := Abortable & '|';
            Quit                := Quit & '|';
            Activity_Counter    := Activity_Counter & '|';
            Activity_Time_Stamp := Activity_Time_Stamp & '|';
         end if;

         Sock := Sock &
           Interfaces.C.int'Image (Sockets.Get_FD (Slot_Data.Sock));

         Opened := Opened &
           Boolean'Image (Slot_Data.Opened);

         Abortable := Abortable &
           Boolean'Image (Slot_Data.Abortable);

         Quit := Quit &
           Boolean'Image (Slot_Data.Quit);

         Activity_Counter := Activity_Counter &
           Positive'Image (Slot_Data.Activity_Counter);

         Activity_Time_Stamp := Activity_Time_Stamp &
           GNAT.Calendar.Time_IO.Image (Slot_Data.Activity_Time_Stamp,
                                        "%a %D %T");
      end loop;

      return Templates_Parser.Translate_Table'
        (Templates_Parser.Assoc ("SOCK_T",
                                 To_String (Sock), True),
         Templates_Parser.Assoc ("OPENED_T",
                                 To_String (Opened), True),
         Templates_Parser.Assoc ("ABORTABLE_T",
                                 To_String (Abortable), True),
         Templates_Parser.Assoc ("QUIT_T",
                                 To_String (Quit), True),
         Templates_Parser.Assoc ("ACTIVITY_COUNTER_T",
                                 To_String (Activity_Counter), True),
         Templates_Parser.Assoc ("ACTIVITY_TIME_STAMP_T",
                                 To_String (Activity_Time_Stamp), True));
   end Slot_Table;

   use type Templates_Parser.Translate_Table;

   Translations : constant Templates_Parser.Translate_Table
     := (Templates_Parser.Assoc ("SERVER_NAME",
                                 To_String (Server.Name)),
         Templates_Parser.Assoc ("MAX_CONNECTION",
                                 Positive'Image (Server.Max_Connection)),
         Templates_Parser.Assoc ("SERVER_PORT",
                                 Positive'Image (Server.Port)),
         Templates_Parser.Assoc ("SECURITY", Server.Security),
         Templates_Parser.Assoc ("SERVER_SOCK",
                                 Interfaces.C.int'Image
                                 (Sockets.Get_FD (Server.Sock))),
         Templates_Parser.Assoc ("VERSION", Version)
         ) &
     Slot_Table;

begin
   return Templates_Parser.Parse ("status.tmplt", Translations);
end AWS.Server.Get_Status;
