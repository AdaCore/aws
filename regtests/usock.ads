------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                            Copyright (C) 2006                            --
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

--  Test for user defined communication layer

with Ada.Streams;

with AWS.Net;
with AWS.Status;
with AWS.Response;

package USock is

   use Ada.Streams;
   use AWS;

   type U_Socket is new Net.Socket_Type with private;

   function Socket (Security : in Boolean) return Net.Socket_Type'Class;

   procedure Bind
     (Socket : in out U_Socket;
      Port   : in     Natural;
      Host   : in     String := "");

   procedure Listen
     (Socket     : in U_Socket;
      Queue_Size : in Positive := 5);

   procedure Accept_Socket
     (Socket     : in     Net.Socket_Type'Class;
      New_Socket : in out U_Socket);

   procedure Connect
     (Socket : in out U_Socket;
      Host   : in     String;
      Port   : in     Positive;
      Wait   : in     Boolean := True);

   procedure Shutdown (Socket : in U_Socket);

   procedure Free (Socket : in out U_Socket);

   procedure Send
     (Socket : in     U_Socket;
      Data   : in     Stream_Element_Array;
      Last   :    out Stream_Element_Offset);

   procedure Receive
     (Socket : in     U_Socket;
      Data   :    out Stream_Element_Array;
      Last   :    out Stream_Element_Offset);

   function Pending
     (Socket : in U_Socket)
      return Stream_Element_Count;

   function Get_FD (Socket : in U_Socket) return Integer;

   function Peer_Addr (Socket : in U_Socket) return String;

   function Peer_Port (Socket : in U_Socket) return Positive;

   function Get_Port (Socket : in U_Socket) return Positive;

   procedure Set_Send_Buffer_Size
     (Socket : in U_Socket;
      Size   : in Natural);

   procedure Set_Receive_Buffer_Size
     (Socket : in U_Socket;
      Size   : in Natural);

   function Get_Send_Buffer_Size (Socket : in U_Socket) return Natural;

   function Get_Receive_Buffer_Size (Socket : in U_Socket) return Natural;

   function Errno (Socket : in U_Socket) return Integer;

   function CB (Request : in Status.Data) return Response.Data;

private

   type U_Socket is new Net.Socket_Type with record
      Index : Natural := 0;
   end record;

end USock;
