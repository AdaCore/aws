------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2004-2016, AdaCore                     --
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

with AWS.Utils;

package body AWS.Net.Log is

   type Log_State is record
      Write     : Write_Callback;
      Event     : Event_Callback;
      Error     : Error_Callback;
      Semaphore : Utils.Semaphore;
   end record;

   State : Log_State;

   --  Note that the three boolean below are global and shared with all tasks.
   --  This is not an issue here as they are used only under a critical
   --  section which allows a single task to enter. This is to avoid recursive
   --  call if a user's callback is using a socket.

   In_Error : Boolean := False;
   In_Event : Boolean := False;
   In_Write : Boolean := False;

   -----------
   -- Error --
   -----------

   procedure Error (Socket : Socket_Type'Class; Message : String) is
   begin
      --  Draft check for State.Error before enter critical section

      if State.Error = null then
         return;
      end if;

      State.Semaphore.Seize;

      --  Explicit check for State.Error inside of critical section

      if State.Error /= null and then not In_Error then
         In_Error := True;

         begin
            --  This call must never fail, catch all exceptions
            State.Error (Socket, Message);
         exception
            when others =>
               null;
         end;

         In_Error := False;
      end if;

      State.Semaphore.Release;
   end Error;

   -----------
   -- Event --
   -----------

   procedure Event (Action : Event_Type; Socket : Socket_Type'Class) is
   begin
      State.Semaphore.Seize;

      if State.Event /= null and then not In_Event then
         In_Event := True;

         begin
            --  This call must never fail, catch all exceptions
            State.Event (Action, Socket);
         exception
            when others =>
               null;
         end;

         In_Event := False;
      end if;

      State.Semaphore.Release;
   end Event;

   ---------------
   -- Is_Active --
   ---------------

   function Is_Active return Boolean is
   begin
      return State.Write /= null
               or else State.Event /= null
               or else State.Error /= null;
   end Is_Active;

   ---------------------
   -- Is_Event_Active --
   ---------------------

   function Is_Event_Active return Boolean is
   begin
      return State.Event /= null;
   end Is_Event_Active;

   ---------------------
   -- Is_Write_Active --
   ---------------------

   function Is_Write_Active return Boolean is
   begin
      return State.Write /= null;
   end Is_Write_Active;

   -----------
   -- Start --
   -----------

   procedure Start
     (Write : Write_Callback;
      Event : Event_Callback := null;
      Error : Error_Callback := null) is
   begin
      State.Semaphore.Seize;
      State.Write := Write;
      State.Event := Event;
      State.Error := Error;
      State.Semaphore.Release;
   end Start;

   ----------
   -- Stop --
   ----------

   procedure Stop is
   begin
      State.Semaphore.Seize;
      State.Event := null;
      State.Write := null;
      State.Error := null;
      State.Semaphore.Release;
   end Stop;

   -----------
   -- Write --
   -----------

   procedure Write
     (Direction : Data_Direction;
      Socket    : Socket_Type'Class;
      Data      : Stream_Element_Array;
      Last      : Stream_Element_Offset) is
   begin
      State.Semaphore.Seize;

      if State.Write /= null and then not In_Write then
         In_Write := True;

         begin
            --  This call must never fail, catch all exceptions
            State.Write
              (Direction => Direction,
               Socket    => Socket,
               Data      => Data,
               Last      => Last);
         exception
            when others =>
               null;
         end;

         In_Write := False;
      end if;

      State.Semaphore.Release;
   end Write;

end AWS.Net.Log;
