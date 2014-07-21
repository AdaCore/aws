------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2014, AdaCore                     --
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
with Ada.Streams;
with Ada.Strings.Unbounded;

with AWS.Response;
with AWS.Server;
with AWS.Status;
with AWS.Server.Push;

package WS_CB is

   use Ada.Calendar;
   use Ada.Streams;
   use Ada.Strings.Unbounded;

   WS : AWS.Server.HTTP;

   procedure Stop_Push_Server;

   function Service (Request : AWS.Status.Data) return AWS.Response.Data;
   --  Simple ID generator

   protected New_Client_Id is
      procedure Get (New_Id : out String);
   private
      Id : Natural := 0;
   end New_Client_Id;

   type Client_Env is record
      Start   : Time;
      Picture : Unbounded_String;
   end record;

   function To_Array
     (Time : Ada.Calendar.Time;
      Env  : Client_Env) return Stream_Element_Array;

   package Time_Push is new AWS.Server.Push
     (Client_Output_Type => Ada.Calendar.Time,
      Client_Environment => Client_Env,
      To_Stream_Array    => To_Array);

   SP : Time_Push.Object;

end WS_CB;
