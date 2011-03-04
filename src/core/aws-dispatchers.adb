------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2011, AdaCore                     --
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

package body AWS.Dispatchers is

   ------------
   -- Adjust --
   ------------

   overriding procedure Adjust (Dispatcher : in out Handler) is
   begin
      Dispatcher.Ref_Counter.all := Dispatcher.Ref_Counter.all + 1;
   end Adjust;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Dispatcher : in out Handler) is
      use type Utils.Counter_Access;
      Ref_Counter : Utils.Counter_Access := Dispatcher.Ref_Counter;
   begin
      --  Ensure call is idempotent

      Dispatcher.Ref_Counter := null;

      if Ref_Counter /= null then
         Ref_Counter.all := Ref_Counter.all - 1;
         if Ref_Counter.all = 0 then
            Utils.Free (Ref_Counter);
         end if;
      end if;
   end Finalize;

   ----------
   -- Free --
   ----------

   procedure Free (Dispatcher : in out Handler_Class_Access) is
      procedure Release is
        new Ada.Unchecked_Deallocation (Handler'Class, Handler_Class_Access);
   begin
      Release (Dispatcher);
   end Free;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (Dispatcher : in out Handler) is
   begin
      Dispatcher.Ref_Counter := new Natural'(1);
   end Initialize;

   -----------------
   -- Ref_Counter --
   -----------------

   function Ref_Counter (Dispatcher : Handler) return Natural is
      use type Utils.Counter_Access;
   begin
      if Dispatcher.Ref_Counter = null then
         return 0;
      else
         return Dispatcher.Ref_Counter.all;
      end if;
   end Ref_Counter;

end AWS.Dispatchers;
