------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2005-2011, AdaCore                     --
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

with AWS.Messages;

package body AWS.Services.Dispatchers.Linker is

   use AWS.Dispatchers;

   -----------
   -- Clone --
   -----------

   overriding function Clone (Dispatcher : Handler) return Handler is
      New_Dispatcher : Handler;
   begin
      if Dispatcher.First /= null then
         New_Dispatcher.First :=
           new AWS.Dispatchers.Handler'Class'
             (AWS.Dispatchers.Handler'Class (Dispatcher.First.Clone));
      end if;

      if Dispatcher.Second /= null then
         New_Dispatcher.Second :=
           new AWS.Dispatchers.Handler'Class'
             (AWS.Dispatchers.Handler'Class (Dispatcher.Second.Clone));
      end if;

      return New_Dispatcher;
   end Clone;

   --------------
   -- Dispatch --
   --------------

   overriding function Dispatch
     (Dispatcher : Handler;
      Request    : Status.Data) return Response.Data
   is
      use type Messages.Status_Code;
      R : Response.Data;
   begin
      R := AWS.Dispatchers.Dispatch (Dispatcher.First.all, Request);

      if Response.Status_Code (R) /= Messages.S404 then
         return R;
      else
         return AWS.Dispatchers.Dispatch (Dispatcher.Second.all, Request);
      end if;
   end Dispatch;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Dispatcher : in out Handler) is
      Ref_Counter : constant Natural := Dispatcher.Ref_Counter;
   begin
      Finalize (AWS.Dispatchers.Handler (Dispatcher));

      if Ref_Counter = 1 then
         Free (Dispatcher.First);
         Free (Dispatcher.Second);
      end if;
   end Finalize;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (Dispatcher : in out Handler) is
   begin
      Initialize (AWS.Dispatchers.Handler (Dispatcher));
   end Initialize;

   --------------
   -- Register --
   --------------

   procedure Register
     (Dispatcher    : in out Handler;
      First, Second : AWS.Dispatchers.Handler'Class) is
   begin
      Dispatcher :=
        (AWS.Dispatchers.Handler with
         new AWS.Dispatchers.Handler'Class'(First),
         new AWS.Dispatchers.Handler'Class'(Second));
   end Register;

end AWS.Services.Dispatchers.Linker;
