------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2001                          --
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

--  $Id$

with AWS.MIME;

package body AWS.Services.Dispatchers.URI is

   type Reg_URI is new Std_URI with record
      Reg_URI : GNAT.Regexp.Regexp;
   end record;

   function Match (URI : in Reg_URI; Value : in String) return Boolean;

   procedure Free is
      new Ada.Unchecked_Deallocation (Std_URI'Class, URI_Class_Access);

   --------------
   -- Dispatch --
   --------------

   function Dispatch
     (Dispatcher : in Handler;
      Request    : in Status.Data)
      return Response.Data
   is
      URI : constant String := Status.URI (Request);
   begin
      for K in 1 .. URI_Table.Last (Dispatcher.Table) loop
         if Match (Dispatcher.Table.Table (K).all, URI) then
            return Dispatch (Dispatcher.Table.Table (K).Action.all, Request);
         end if;
      end loop;

      --  No rule found, try the default dispatcher

      if Dispatcher.Action /= null then
         return Dispatch (Dispatcher.Action.all, Request);
      end if;

      return Response.Build
        (MIME.Text_HTML,
         "<p>AWS " & Version
         & "<p> No rule found for '" URI & "'in dispatch URI call and no "
         & "default dispatcher defined.");
   end Dispatch;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Dispatcher : in out Handler) is
   begin
      Dispatcher.Ref_Counter := Dispatcher.Ref_Counter - 1;

      if Dispatcher.Ref_Counter = 0 then
         for K in 1 .. URI_Table.Last (Dispatcher.Table) loop
            Free (Dispatcher.Table.Table (K));
         end loop;
         URI_Table.Free (Dispatcher.Table);
      end if;
   end Finalize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Dispatcher : in out Handler) is
   begin
      Dispatcher.Ref_Counter := 1;
      URI_Table.Init (Dispatcher.Table);
   end Initialize;

   -----------
   -- Match --
   -----------

   function Match (URI : in Std_URI; Value : in String) return Boolean is
   begin
      return To_String (URI.URI) = Value;
   end Match;

   function Match (URI : in Reg_URI; Value : in String) return Boolean is
   begin
      return GNAT.Regexp.Match (Value, URI.Reg_URI);
   end Match;

   --------------
   -- Register --
   --------------

   procedure Register
     (Dispatcher : in out Handler;
      URI        : in     String;
      Action     : in     Dispatchers.Handler'Class)
   is
      Value : URI_Class_Access;
   begin
      Value := new Std_URI'(new Dispatchers.Handler'Class'(Action),
                            To_Unbounded_String (URI));

      URI_Table.Increment_Last (Dispatcher.Table);
      Dispatcher.Table.Table (URI_Table.Last (Dispatcher.Table)) := Value;
   end Register;

   -------------------------------
   -- Register_Default_Callback --
   -------------------------------

   procedure Register_Default_Callback
     (Dispatcher : in out Handler;
      Action     : in     Dispatchers.Handler'Class) is
   begin
      if Dispatcher.Action /= null then
         Free (Dispatcher.Action);
      end if;

      Dispatcher.Action := new Dispatchers.Handler'Class'(Action);
   end Register_Default_Callback;

   ---------------------
   -- Register_Regexp --
   ---------------------

   procedure Register_Regexp
     (Dispatcher : in out Handler;
      URI        : in     String;
      Action     : in     Dispatchers.Handler'Class)
   is
      Value : URI_Class_Access;
   begin
      Value := new Reg_URI'(new Dispatchers.Handler'Class'(Action),
                            To_Unbounded_String (URI),
                            GNAT.Regexp.Compile (URI));


      URI_Table.Increment_Last (Dispatcher.Table);
      Dispatcher.Table.Table (URI_Table.Last (Dispatcher.Table)) := Value;
   end Register_Regexp;

   ----------------
   -- Unregister --
   ----------------

   procedure Unregister
     (Dispatcher : in out Handler;
      URI        : in     String)
   is
      Last : constant Natural := URI_Table.Last (Dispatcher.Table);
   begin
      for K in 1 .. Last loop
         if To_String (Dispatcher.Table.Table (K).URI) = URI then
            Free (Dispatcher.Table.Table (K));
            Dispatcher.Table.Table (K .. Last - 1) :=
              Dispatcher.Table.Table (K + 1 .. Last);
            URI_Table.Decrement_Last (Dispatcher.Table);
            exit;
         end if;
      end loop;
   end Unregister;

end AWS.Services.Dispatchers.URI;

