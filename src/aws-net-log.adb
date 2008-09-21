------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2004-2008, AdaCore                     --
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

with AWS.Utils;

package body AWS.Net.Log is

   type Log_State is record
      Write     : Write_Callback;
      Event     : Event_Callback;
      Error     : Error_Callback;
      Semaphore : Utils.Semaphore;
   end record;

   State : Log_State;

   -----------
   -- Error --
   -----------

   procedure Error (Socket : in Socket_Type'Class; Message : in String) is
   begin
      --  Draft check for State.Error before enter critical section

      if State.Error = null then
         return;
      end if;

      State.Semaphore.Seize;

      --  Explicit check for State.Error inside of critical section

      if State.Error /= null then
         begin
            --  This call must never fail, catch all exceptions
            State.Error (Socket, Message);
         exception
            when others =>
               null;
         end;
      end if;

      State.Semaphore.Release;
   end Error;

   -----------
   -- Event --
   -----------

   procedure Event (Action : in Event_Type; Socket : in Socket_Type'Class) is
   begin
      State.Semaphore.Seize;

      if State.Event /= null then
         begin
            --  This call must never fail, catch all exceptions
            State.Event (Action, Socket);
         exception
            when others =>
               null;
         end;
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
     (Write : in Write_Callback;
      Event : in Event_Callback := null;
      Error : in Error_Callback := null) is
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
      State.Semaphore.Release;
   end Stop;

   -----------
   -- Write --
   -----------

   procedure Write
     (Direction : in Data_Direction;
      Socket    : in Socket_Type'Class;
      Data      : in Stream_Element_Array;
      Last      : in Stream_Element_Offset) is
   begin
      State.Semaphore.Seize;

      if State.Write /= null then
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
      end if;

      State.Semaphore.Release;
   end Write;

end AWS.Net.Log;
