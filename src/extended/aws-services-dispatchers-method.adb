------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2012, AdaCore                     --
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

with AWS.Dispatchers.Callback;
with AWS.Messages;

package body AWS.Services.Dispatchers.Method is

   use AWS.Dispatchers;

   -----------
   -- Clone --
   -----------

   overriding function Clone (Dispatcher : Handler) return Handler is
      New_Dispatcher : Handler;
   begin
      if Dispatcher.Action /= null then
         New_Dispatcher.Action :=
           new AWS.Dispatchers.Handler'Class'
             (AWS.Dispatchers.Handler'Class (Dispatcher.Action.Clone));
      end if;

      for K in Dispatcher.Table'Range loop
         if Dispatcher.Table (K) /= null then
            New_Dispatcher.Table (K) :=
              new AWS.Dispatchers.Handler'Class'
                (AWS.Dispatchers.Handler'Class (Dispatcher.Table (K).Clone));
         end if;
      end loop;

      return New_Dispatcher;
   end Clone;

   --------------
   -- Dispatch --
   --------------

   overriding function Dispatch
     (Dispatcher : Handler;
      Request    : Status.Data) return Response.Data
   is
      Method : constant Status.Request_Method := Status.Method (Request);
   begin
      if Dispatcher.Table (Method) = null then
         if Dispatcher.Action = null then
            return Response.Acknowledge
              (Messages.S404,
               "<p>AWS " & Version
               & "<p>No rule found in dispatch for "
               & Status.Request_Method'Image (Method) & " method call.");
         else
            return Dispatch (Dispatcher.Action.all, Request);
         end if;

      else
         return Dispatch (Dispatcher.Table (Method).all, Request);
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
         for K in Dispatcher.Table'Range loop
            Free (Dispatcher.Table (K));
         end loop;

         Free (Dispatcher.Action);
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
     (Dispatcher : in out Handler;
      Method     : Status.Request_Method;
      Action     : AWS.Dispatchers.Handler'Class) is
   begin
      if Dispatcher.Table (Method) /= null then
         Free (Dispatcher.Table (Method));
      end if;

      Dispatcher.Table (Method) := new AWS.Dispatchers.Handler'Class'(Action);
   end Register;

   procedure Register
     (Dispatcher : in out Handler;
      Method     : Status.Request_Method;
      Action     : Response.Callback) is
   begin
      Register (Dispatcher, Method, AWS.Dispatchers.Callback.Create (Action));
   end Register;

   -------------------------------
   -- Register_Default_Callback --
   -------------------------------

   procedure Register_Default_Callback
     (Dispatcher : in out Handler;
      Action     : AWS.Dispatchers.Handler'Class) is
   begin
      if Dispatcher.Action /= null then
         Free (Dispatcher.Action);
      end if;

      Dispatcher.Action := new AWS.Dispatchers.Handler'Class'(Action);
   end Register_Default_Callback;

   ----------------
   -- Unregister --
   ----------------

   procedure Unregister
     (Dispatcher : in out Handler;
      Method     : Status.Request_Method) is
   begin
      if Dispatcher.Table (Method) = null then
         raise Constraint_Error
           with "Method distpatcher "
             & Status.Request_Method'Image (Method) & " not found";
      else
         Free (Dispatcher.Table (Method));
      end if;
   end Unregister;

end AWS.Services.Dispatchers.Method;
