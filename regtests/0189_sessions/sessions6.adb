------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2008-2012, AdaCore                     --
--                                                                          --
--  This is free software;  you can redistribute it  and/or modify it       --
--  under terms of the  GNU General Public License as published  by the     --
--  Free Software  Foundation;  either version 3,  or (at your option) any  --
--  later version.  This software is distributed in the hope  that it will  --
--  be useful, but WITHOUT ANY WARRANTY;  without even the implied warranty --
--  of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU     --
--  General Public License for  more details.                               --
--                                                                          --
--  You should have  received  a copy of the GNU General  Public  License   --
--  distributed  with  this  software;   see  file COPYING3.  If not, go    --
--  to http://www.gnu.org/licenses for a complete copy of the license.      --
------------------------------------------------------------------------------

with Ada.Calendar;
with Ada.Exceptions;
with Ada.Text_IO;

with AWS.Session.Control;

procedure Sessions6 is

   use Ada;
   use AWS.Session;

   procedure For_Each_Key_Value
     (N : Positive; Key, Value : String; Quit : in out Boolean);

   procedure For_Each_Session
     (N          : Positive;
      SID        : Id;
      Time_Stamp : Ada.Calendar.Time;
      Quit       : in out Boolean);
   --  Add session SID to the list

   ------------------------
   -- For_Each_Key_Value --
   ------------------------

   procedure For_Each_Key_Value
     (N : Positive; Key, Value : String; Quit : in out Boolean)
   is
      pragma Unreferenced (N, Key, Value, Quit);
   begin
      null;
   end For_Each_Key_Value;

   --------------------
   -- Each_Key_Value --
   --------------------

   procedure Each_Key_Value is new For_Every_Session_Data (For_Each_Key_Value);

   ----------------------
   -- For_Each_Session --
   ----------------------

   procedure For_Each_Session
     (N          : Positive;
      SID        : Id;
      Time_Stamp : Ada.Calendar.Time;
      Quit       : in out Boolean)
   is
      pragma Unreferenced (N, Time_Stamp, Quit);
   begin
      Each_Key_Value (SID);
   end For_Each_Session;

   procedure Each_Session is new For_Every_Session (For_Each_Session);

   SID : Id;

begin
   Control.Start (0.1, Session_Lifetime => 0.5);

   for J in 1 .. 1000 loop
      SID := Create;
      for K in 1 .. 100 loop
         Set (SID, "K" & Integer'Image (K), Integer'Image (J));
      end loop;
   end loop;

   loop
      Each_Session;
      delay 0.1;
      exit when Length = 0;
   end loop;

   Control.Shutdown;

   Text_IO.Put_Line ("OK");

exception
   when E : others =>
      Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
      Control.Shutdown;
end Sessions6;
