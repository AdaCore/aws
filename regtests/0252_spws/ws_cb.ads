------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                    Copyright (C) 2014-2015, AdaCore                      --
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

with AWS.Response;
with AWS.Server;
with AWS.Status;

package WS_CB is

   use AWS;

   WS : AWS.Server.HTTP;

   procedure Websock_Start;

   function Get (Request : AWS.Status.Data) return AWS.Response.Data;

   procedure Server_Push_Send (Num : Integer);

   protected Wait is
      entry Ready;
      entry Close;
      procedure Set (Value : Boolean);

   private
      Started : Boolean := False;
   end Wait;

end WS_CB;
