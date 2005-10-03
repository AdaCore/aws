------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2005                          --
--                                 AdaCore                                  --
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

--  $RCSfile$
--  $Revision$ $Date$ $Author$

with Ada.Strings.Fixed;

with AWS.Dispatchers.Callback;
with AWS.MIME;
with AWS.Status;
with AWS.Parameters;

package body AWS.Services.Dispatchers.Virtual_Host is

   use Ada;
   use AWS.Dispatchers;

   procedure Register
     (Dispatcher       : in out Handler;
      Virtual_Hostname : in     String;
      Node             : in     VH_Node);
   --  Register Node as into the dispatcher

   --------------
   -- Dispatch --
   --------------

   function Dispatch
     (Dispatcher : in Handler;
      Request    : in AWS.Status.Data)
      return AWS.Response.Data
   is
      Hostname : constant String := Status.Host (Request);
      Location : Unbounded_String;
      K        : Natural;
      Node     : VH_Node;
      Cursor   : Virtual_Host_Table.Cursor;
   begin
      K := Strings.Fixed.Index (Hostname, ":");

      if K = 0 then
         K := Hostname'Last;
      else
         K := K - 1;
      end if;

      Cursor := VH_Table.Find
        (Dispatcher.Table, Hostname (Hostname'First .. K));

      if Virtual_Host_Table.Has_Element (Cursor) then
         Node := VH_Table.Element (Cursor);

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
         return AWS.Response.Build
           (MIME.Text_HTML,
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

   procedure Finalize (Dispatcher : in out Handler) is
      Cursor  : Virtual_Host_Table.Cursor;
   begin
      Finalize (AWS.Dispatchers.Handler (Dispatcher));

      if Ref_Counter (Dispatcher) = 0 then
         Cursor := VH_Table.First (Dispatcher.Table);

         while Virtual_Host_Table.Has_Element (Cursor) loop
            declare
               Node : VH_Node
                 := Virtual_Host_Table.Containers.Element (Cursor);
            begin
               if Node.Mode = Callback then
                  Free (Node.Action);
               end if;
            end;
            Virtual_Host_Table.Containers.Next (Cursor);
         end loop;

         VH_Table.Clear (Dispatcher.Table);
         Free (Dispatcher.Action);
      end if;
   end Finalize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Dispatcher : in out Handler) is
   begin
      Initialize (AWS.Dispatchers.Handler (Dispatcher));
   end Initialize;

   --------------
   -- Register --
   --------------

   procedure Register
     (Dispatcher       : in out Handler;
      Virtual_Hostname : in     String;
      Node             : in     VH_Node)
   is
      Cursor  : Virtual_Host_Table.Cursor;
      Success : Boolean;
   begin
      VH_Table.Insert
        (Dispatcher.Table, Virtual_Hostname, Node, Cursor, Success);

      if not Success then
         Virtual_Host_Table.Containers.Replace_Element (Cursor, Node);
      end if;
   end Register;

   procedure Register
     (Dispatcher       : in out Handler;
      Virtual_Hostname : in     String;
      Hostname         : in     String)
   is
      Node : constant VH_Node := (Host, To_Unbounded_String (Hostname));
   begin
      Register (Dispatcher, Virtual_Hostname, Node);
   end Register;

   procedure Register
     (Dispatcher       : in out Handler;
      Virtual_Hostname : in     String;
      Action           : in     AWS.Dispatchers.Handler'Class)
   is
      Node : constant VH_Node
        := (Virtual_Host.Callback, new AWS.Dispatchers.Handler'Class'(Action));
   begin
      Register (Dispatcher, Virtual_Hostname, Node);
   end Register;

   procedure Register
     (Dispatcher       : in out Handler;
      Virtual_Hostname : in     String;
      Action           : in     Response.Callback) is
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
      Action     : in     AWS.Dispatchers.Handler'Class) is
   begin
      Dispatcher.Action := new AWS.Dispatchers.Handler'Class'(Action);
   end Register_Default_Callback;

   ----------------
   -- Unregister --
   ----------------

   procedure Unregister
     (Dispatcher       : in out Handler;
      Virtual_Hostname : in     String) is
   begin
      VH_Table.Delete (Dispatcher.Table, Virtual_Hostname);
   end Unregister;

end AWS.Services.Dispatchers.Virtual_Host;
