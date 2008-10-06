------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                        Copyright (C) 2008, AdaCore                       --
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

with Ada.Calendar;
with Ada.Exceptions;
with Ada.Text_IO;

with AWS.Session.Control;

procedure Sessions is

   use AWS.Session;

   procedure For_Each_Key_Value
     (N : in Positive; Key, Value : in String; Quit : in out Boolean);

   procedure For_Each_Session
     (N          : in     Positive;
      SID        : in     Id;
      Time_Stamp : in     Ada.Calendar.Time;
      Quit       : in out Boolean);
   --  add session SID to the list

   ------------------------
   -- For_Each_Key_Value --
   ------------------------

   procedure For_Each_Key_Value
     (N : in Positive; Key, Value : in String; Quit : in out Boolean)
   is
      pragma Unreferenced (N, Key, Value, Quit);
   begin
      delay Duration'Delta;
   end For_Each_Key_Value;

   --------------------
   -- Each_Key_Value --
   --------------------

   procedure Each_Key_Value is new For_Every_Session_Data (For_Each_Key_Value);

   ----------------------
   -- For_Each_Session --
   ----------------------

   procedure For_Each_Session
     (N          : in     Positive;
      SID        : in     Id;
      Time_Stamp : in     Ada.Calendar.Time;
      Quit       : in out Boolean)
   is
      pragma Unreferenced (N, Time_Stamp, Quit);
   begin
      Each_Key_Value (SID);
   end For_Each_Session;

   procedure Each_Session is new For_Every_Session (For_Each_Session);

   SID : Id;

begin
   Control.Start (0.5, Session_Lifetime => 0.5);

   for J in 1 .. 1000 loop
      SID := Create;

      for K in 1 .. 100 loop
         Set (SID, "K" & Integer'Image (K), Integer'Image (J));
      end loop;
   end loop;

   loop
      Each_Session;
      exit when Length = 0;
   end loop;

   Control.Shutdown;

   Ada.Text_IO.Put_Line ("OK");

exception
   when E : others =>
      Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
      Control.Shutdown;
end Sessions;
