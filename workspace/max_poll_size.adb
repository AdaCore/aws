------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                            Copyright (C) 2007                            --
--                                 AdaCore                                  --
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

with Ada.Text_IO;
with AWS.Net.Sets;

procedure Max_Poll_Size is
   use AWS.Net;
   Set                  : Sets.Socket_Set_Type;
   Port                 : constant Positive := 8088;
   Server, Client, Peer : Socket_Type'Class := Socket (False);

begin
   Bind (Server, Port);
   Listen (Server);

   loop
      Connect (Client, "localhost", Port);
      Accept_Socket (Server, Peer);
      Sets.Add (Set, Peer, Sets.Output);
      Sets.Add (Set, Client, Sets.Input);
      Sets.Wait (Set, 0.001);
      Ada.Text_IO.Put_Line (Sets.Count (Set)'Img);
   end loop;
end Max_Poll_Size;
