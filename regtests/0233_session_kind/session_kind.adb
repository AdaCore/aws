------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                       Copyright (C) 2013, AdaCore                        --
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

procedure Session_Kind is

   use Ada;
   use AWS;
   use AWS.Session;

   type Context is record
      S : String (1 .. 10);
   end record;

   Null_Context : constant Context := (S => "          ");

   package Context_Session is new Session.Generic_Data (Context, Null_Context);

   procedure For_Each_Key_Value
     (N          : Positive;
      Key, Value : String;
      Kind       : Value_Kind;
      Quit       : in out Boolean);

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
     (N          : Positive;
      Key, Value : String;
      Kind       : Value_Kind;
      Quit       : in out Boolean)
   is
      pragma Unreferenced (N, Quit);
   begin
      Text_IO.Put_Line (Key & '-' & Value & '-' & Value_Kind'Image (Kind));
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

   C   : constant Context := (S => (others => 'a'));

begin
   Control.Start (0.1, Session_Lifetime => 0.5);

   SID := Create;

   Set (SID, "V1", "a value");
   Set (SID, "V2", 12);
   Set (SID, "V3", True);
   Set (SID, "V4", 78.65);
   Context_Session.Set (SID, "V5", C);

   Each_Session;

   Control.Shutdown;

   Text_IO.Put_Line ("OK");

exception
   when E : others =>
      Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
      Control.Shutdown;
end Session_Kind;
