------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                    Copyright (C) 2015-2018, AdaCore                      --
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
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with AWS.Client;

with testingservice.Client;
with testingservice.Types;

procedure Dl2_Client (Client : AWS.Client.HTTP_Connection) is

   use Ada, Ada.Calendar;
   use Ada.Strings.Unbounded;
   use Ada.Text_IO;

   use testingservice.Client;
   use testingservice.Types;

   D : GetQueueStatusRequest_Type;
   R : getQueueStatus_Result;

   Re : execute_Result;
   N  : RecB_Set_Type (1 .. 2) :=
          (((1.0, 1.1), 3.0, 4.0, 2, 0.9, 8, 0.6),
           ((2.0, 2.1), 3.2, 4.2, 2, 0.91, 81, 0.61));
begin
   R := getQueueStatus (Client, D);
   Put_Line ("numberQueued     =" & R.numberQueued'Img);
   Put_Line ("maximumQueueSize =" & R.maximumQueueSize'Img);

   declare
      Re : constant String := execute (Client, Calendar.Clock, N);
   begin
      Put_Line ("invocationToken = " & Re);
   end;
end Dl2_Client;
