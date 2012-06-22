------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2013, AdaCore                     --
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

with Ada.Strings.Fixed;

with AWS.Dispatchers.Callback;
with AWS.Messages;
with AWS.Parameters;

package body AWS.Services.Dispatchers.Virtual_Host is

   use Ada;
   use AWS.Dispatchers;

   procedure Register
     (Dispatcher       : in out Handler;
      Virtual_Hostname : String;
      Node             : VH_Node);
   --  Register Node as into the dispatcher

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

      for Cursor in Dispatcher.Table.Iterate loop
         declare
            Node : constant VH_Node := Virtual_Host_Table.Element (Cursor);
         begin
            if Node.Mode = Callback then
               New_Dispatcher.Table.Insert
                 (Key       => Virtual_Host_Table.Key (Cursor),
                  New_Item  =>
                    VH_Node'
                      (Mode   => Callback,
                       Action => new AWS.Dispatchers.Handler'Class'
                         (AWS.Dispatchers.Handler'Class (Node.Action.Clone))));

            else
               New_Dispatcher.Table.Insert
                 (Key       => Virtual_Host_Table.Key (Cursor),
                  New_Item  =>
                    VH_Node'
                      (Mode     => Host,
                       Hostname => Node.Hostname));
            end if;
         end;
      end loop;

      return New_Dispatcher;
   end Clone;

   --------------
   -- Dispatch --
   --------------

   overriding function Dispatch
     (Dispatcher : Handler;
      Request    : AWS.Status.Data) return AWS.Response.Data
   is
      Hostname : constant String := Status.Host (Request);
      Location : Unbounded_String;
      K        : Natural;
      Node     : VH_Node;
      Cursor   : Virtual_Host_Table.Cursor;
   begin
      K := Strings.Fixed.Index (Hostname, ":", Strings.Backward);

      if K = 0 then
         K := Hostname'Last;
      else
         K := K - 1;
      end if;

      Cursor := Dispatcher.Table.Find (Hostname (Hostname'First .. K));

      if Virtual_Host_Table.Has_Element (Cursor) then
         Node := Virtual_Host_Table.Element (Cursor);

         case Node.Mode is
            when Host     =>
               declare
                  P : constant Parameters.List := Status.Parameters (Request);
               begin
                  Location := To_Unbounded_String ("http://");
                  Append (Location, To_String (Node.Hostname));
                  Append (Location, Status.URI (Request));
                  Append (Location, Parameters.URI_Format (P));
               end;

               return AWS.Response.URL (To_String (Location));

            when Callback =>
               return Dispatch (Node.Action.all, Request);
         end case;
      end if;

      if Dispatcher.Action = null then
         return Response.Acknowledge
           (Messages.S404,
            "<p>Virtual Hosting is activated but no virtual host match "
            & Status.Host (Request)
            & "<p>Please check your AWS Virtual Host configuration");
      else
         return Dispatch (Dispatcher.Action.all, Request);
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
         for Node of Dispatcher.Table loop
            if Node.Mode = Callback then
               Free (Node.Action);
            end if;
         end loop;

         Dispatcher.Table.Clear;
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
     (Dispatcher       : in out Handler;
      Virtual_Hostname : String;
      Node             : VH_Node) is
   begin
      Dispatcher.Table.Include (Virtual_Hostname, Node);
   end Register;

   procedure Register
     (Dispatcher       : in out Handler;
      Virtual_Hostname : String;
      Hostname         : String)
   is
      Node : constant VH_Node := (Host, To_Unbounded_String (Hostname));
   begin
      Register (Dispatcher, Virtual_Hostname, Node);
   end Register;

   procedure Register
     (Dispatcher       : in out Handler;
      Virtual_Hostname : String;
      Action           : AWS.Dispatchers.Handler'Class)
   is
      Node : constant VH_Node :=
               (Virtual_Host.Callback,
                new AWS.Dispatchers.Handler'Class'(Action));
   begin
      Register (Dispatcher, Virtual_Hostname, Node);
   end Register;

   procedure Register
     (Dispatcher       : in out Handler;
      Virtual_Hostname : String;
      Action           : Response.Callback) is
   begin
      Register
        (Dispatcher,
         Virtual_Hostname, AWS.Dispatchers.Callback.Create (Action));
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
     (Dispatcher       : in out Handler;
      Virtual_Hostname : String) is
   begin
      Dispatcher.Table.Delete (Virtual_Hostname);
   end Unregister;

end AWS.Services.Dispatchers.Virtual_Host;
