------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2006-2009, AdaCore                     --
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

--  Test for user defined communication layer

with Ada.Streams;

with AWS.Net;
with AWS.Status;
with AWS.Response;

package USock is

   use Ada.Streams;
   use AWS;

   type U_Socket is new Net.Socket_Type with private;

   function Socket (Security : Boolean) return Net.Socket_Type'Class;

   overriding procedure Bind
     (Socket        : in out U_Socket;
      Port          : Natural;
      Host          : String := "";
      Reuse_Address : Boolean := False);

   overriding procedure Listen
     (Socket     : U_Socket;
      Queue_Size : Positive := 5);

   overriding procedure Accept_Socket
     (Socket     : Net.Socket_Type'Class;
      New_Socket : in out U_Socket);

   overriding procedure Connect
     (Socket : in out U_Socket;
      Host   : String;
      Port   : Positive;
      Wait   : Boolean := True);

   overriding procedure Shutdown
     (Socket : U_Socket;
      How    : Net.Shutmode_Type := Net.Shut_Read_Write);

   overriding procedure Send
     (Socket : U_Socket;
      Data   : Stream_Element_Array;
      Last   : out Stream_Element_Offset);

   overriding procedure Receive
     (Socket : U_Socket;
      Data   : out Stream_Element_Array;
      Last   : out Stream_Element_Offset);

   overriding function Pending
     (Socket : U_Socket) return Stream_Element_Count;

   overriding function Get_FD (Socket : U_Socket) return Integer;

   overriding function Peer_Addr (Socket : U_Socket) return String;

   overriding function Peer_Port (Socket : U_Socket) return Positive;

   overriding function Get_Addr (Socket : U_Socket) return String;

   overriding function Get_Port (Socket : U_Socket) return Positive;

   overriding procedure Set_Send_Buffer_Size
     (Socket : U_Socket;
      Size   : Natural);

   overriding procedure Set_Receive_Buffer_Size
     (Socket : U_Socket;
      Size   : Natural);

   overriding function Get_Send_Buffer_Size
     (Socket : U_Socket) return Natural;

   overriding function Get_Receive_Buffer_Size
     (Socket : U_Socket) return Natural;

   overriding function Errno
     (Socket : U_Socket) return Integer;

   overriding function To_FD_Set
     (Socket : U_Socket;
      Events : Net.Wait_Event_Set;
      Size   : Positive := 1) return Net.FD_Set'Class;

   function CB (Request : Status.Data) return Response.Data;

private

   type U_Socket is new Net.Socket_Type with record
      Index : Natural := 0;
   end record;

end USock;
