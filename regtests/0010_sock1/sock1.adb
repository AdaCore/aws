------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2003-2012, AdaCore                     --
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

with Ada.Exceptions;
with Ada.Text_IO;
with AWS.Net.Std;

procedure Sock1 is

   use Ada;
   use AWS;

   Sock : Net.Std.Socket_Type
     := Net.Std.Socket_Type (Net.Socket_Type'Class'(Net.Socket (False)));

begin
   Net.Std.Bind (Sock, 1111, "azertytoto.titi_impossible.zzz");
   Text_IO.Put_Line ("Should not be there!");
exception
   when Net.Socket_Error =>
      Text_IO.Put_Line ("OK : Socket_Error");
   when E : others =>
      Text_IO.Put_Line ("NOK : " & Exceptions.Exception_Information (E));
end Sock1;
