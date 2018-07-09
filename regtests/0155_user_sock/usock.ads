------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2006-2012, AdaCore                     --
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

--  Test for user defined communication layer

with Ada.Streams;

with AWS.Net;
with AWS.Response;
with AWS.Status;

package USock is

   use Ada.Streams;
   use AWS;

   type U_Socket is new Net.Socket_Type with private;

   function Socket (Security : Boolean) return Net.Socket_Type'Class;

   overriding procedure Bind
     (Socket        : in out U_Socket;
      Port          : Natural;
      Host          : String := "";
      Reuse_Address : Boolean := False;
      IPv6_Only     : Boolean := False;
      Family        : Net.Family_Type := Net.Family_Unspec);

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
      Wait   : Boolean := True;
      Family : Net.Family_Type := Net.Family_Unspec);

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

   overriding function Is_Listening (Socket : U_Socket) return Boolean;

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
      Server : Boolean := False;
   end record;

end USock;
