------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                            Copyright (C) 2004                            --
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

with AWS.Utils;

package body AWS.Net.Log is

   type Log_State is record
      Active    : Boolean := False;
      Write     : Write_Callback;
      Semaphore : Utils.Semaphore;
   end record;

   State : Log_State;

   ---------------
   -- Is_Active --
   ---------------

   function Is_Active return Boolean is
   begin
      return State.Active;
   end Is_Active;

   -----------
   -- Start --
   -----------

   procedure Start (Write : in Write_Callback) is
   begin
      State.Semaphore.Seize;
      State.Write  := Write;
      State.Active := True;
      State.Semaphore.Release;
   end Start;

   ----------
   -- Stop --
   ----------

   procedure Stop is
   begin
      State.Semaphore.Seize;
      State.Active := False;
      State.Write  := null;
      State.Semaphore.Release;
   end Stop;

   -----------
   -- Write --
   -----------

   procedure Write
     (Direction : in Data_Direction;
      FD        : in Integer;
      Data      : in Stream_Element_Array;
      Last      : in Stream_Element_Offset) is
   begin
      State.Semaphore.Seize;

      if State.Active then
         begin
            --  This call must never fail, catch all exceptions
            State.Write
              (Direction => Direction,
               FD        => FD,
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
