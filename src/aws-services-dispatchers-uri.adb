------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2004                          --
--                                ACT-Europe                                --
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

with Ada.Unchecked_Deallocation;

with GNAT.Regexp;

with AWS.Dispatchers.Callback;
with AWS.MIME;

package body AWS.Services.Dispatchers.URI is

   use AWS.Dispatchers;

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
      use type Response.Data_Mode;

      URI    : constant String := Status.URI (Request);
      Result : Response.Data;
   begin
      for K in 1 .. URI_Table.Length (Dispatcher.Table) loop
         declare
            Item : constant URI_Class_Access
              := URI_Table.Element (Dispatcher.Table, Natural (K));
         begin
            if Match (Item.all, URI) then
               Result := Dispatch (Item.Action.all, Request);

               --  Returns response if dispatcher did return something,
               --  otherwise continue to next handler.

               if Response.Mode (Result) /= Response.No_Data then
                  return Result;
               end if;
            end if;
         end;
      end loop;

      --  No rule found, try the default dispatcher

      if Dispatcher.Action /= null then
         return Dispatch (Dispatcher.Action.all, Request);
      end if;

      return Response.Build
        (MIME.Text_HTML,
         "<p>AWS " & Version
         & "<p> No rule found for '" & URI & "' in dispatch URI call and no "
         & "default dispatcher defined.");
   end Dispatch;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Dispatcher : in out Handler) is
   begin
      Finalize (AWS.Dispatchers.Handler (Dispatcher));

      if Ref_Counter (Dispatcher) = 0 then
         for K in 1 .. URI_Table.Length (Dispatcher.Table) loop
            declare
               Item : URI_Class_Access
                 := URI_Table.Element (Dispatcher.Table, Natural (K));
            begin
               Free (Item.Action);
               Free (Item);
            end;
         end loop;

         Free (Dispatcher.Action);
         URI_Table.Clear (Dispatcher.Table);
      end if;
   end Finalize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Dispatcher : in out Handler) is
   begin
      Initialize (AWS.Dispatchers.Handler (Dispatcher));
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
      Action     : in     AWS.Dispatchers.Handler'Class)
   is
      Value : constant URI_Class_Access
        := new Std_URI'(new AWS.Dispatchers.Handler'Class'(Action),
                        To_Unbounded_String (URI));
   begin
      URI_Table.Append (Dispatcher.Table, Value);
   end Register;

   procedure Register
     (Dispatcher : in out Handler;
      URI        : in     String;
      Action     : in     Response.Callback) is
   begin
      Register (Dispatcher, URI, AWS.Dispatchers.Callback.Create (Action));
   end Register;

   -------------------------------
   -- Register_Default_Callback --
   -------------------------------

   procedure Register_Default_Callback
     (Dispatcher : in out Handler;
      Action     : in     AWS.Dispatchers.Handler'Class) is
   begin
      if Dispatcher.Action /= null then
         Free (Dispatcher.Action);
      end if;

      Dispatcher.Action := new AWS.Dispatchers.Handler'Class'(Action);
   end Register_Default_Callback;

   ---------------------
   -- Register_Regexp --
   ---------------------

   procedure Register_Regexp
     (Dispatcher : in out Handler;
      URI        : in     String;
      Action     : in     AWS.Dispatchers.Handler'Class)
   is
      Value : constant URI_Class_Access
        := new Reg_URI'(new AWS.Dispatchers.Handler'Class'(Action),
                        To_Unbounded_String (URI),
                        GNAT.Regexp.Compile (URI));
   begin
      URI_Table.Append (Dispatcher.Table, Value);
   end Register_Regexp;

   procedure Register_Regexp
     (Dispatcher : in out Handler;
      URI        : in     String;
      Action     : in     Response.Callback) is
   begin
      Register_Regexp
        (Dispatcher, URI, AWS.Dispatchers.Callback.Create (Action));
   end Register_Regexp;

   ----------------
   -- Unregister --
   ----------------

   procedure Unregister
     (Dispatcher : in out Handler;
      URI        : in     String) is
   begin
      for K in 1 .. Natural (URI_Table.Length (Dispatcher.Table)) loop
         declare
            Item : URI_Class_Access
              := URI_Table.Element (Dispatcher.Table, K);
         begin
            if To_String (Item.URI) = URI then
               Free (Item);
               URI_Table.Delete (Dispatcher.Table, K);
               exit;
            end if;
         end;
      end loop;
   end Unregister;

end AWS.Services.Dispatchers.URI;
