------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2003-2011, AdaCore                     --
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

with Ada.Unchecked_Deallocation;

package body AWS.Services.Transient_Pages.Control is

   -----------
   -- Start --
   -----------

   procedure Register (Transient_Check_Interval : Duration) is
      Need_Start : Boolean;
   begin
      Cleaner_Control.Register (Transient_Check_Interval, Need_Start);

      if Need_Start then
         Cleaner_Task := new Cleaner;
      end if;
   end Register;

   --------------
   -- Shutdown --
   --------------

   procedure Shutdown is

      procedure Unchecked_Free is
         new Ada.Unchecked_Deallocation (Cleaner, Cleaner_Access);

      Need_Release : Boolean;

   begin
      Cleaner_Control.Stop (Need_Release);

      if Need_Release then

         Cleaner_Task.Stop;

         --  Wait for task termination

         while not Cleaner_Task'Terminated loop
            delay 0.5;
         end loop;

         --  Release memory

         Unchecked_Free (Cleaner_Task);
      end if;
   end Shutdown;

end AWS.Services.Transient_Pages.Control;
