------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2011-2012, AdaCore                     --
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

with Ada.Text_IO;
with AWS.Log;
with Logging;

procedure Log_Callback is
   use type AWS.Log.Split_Mode;
   use Logging;

   Logger : AWS.Log.Object;
begin
   AWS.Log.Start (Log    => Logger,
                  Writer => External_Logger'Access,
                  Name   => "External_Logger");

   AWS.Log.Write (Log  => Logger,
                  Data => "Yea!");

   Ada.Text_IO.Put_Line (AWS.Log.Filename (Log => Logger));

   if AWS.Log.Is_Active (Log => Logger) then
      Ada.Text_IO.Put_Line ("Active");
   end if;

   if AWS.Log.Mode (Log => Logger) = AWS.Log.None then
      Ada.Text_IO.Put_Line ("Split mode: None");
   end if;

   AWS.Log.Stop (Log => Logger);
end Log_Callback;
