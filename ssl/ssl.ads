------------------------------------------------------------------------------
--                            Secure Sockets Layer                          --
--                         Binding to OpenSSL library                       --
--                                                                          --
--                         Copyright (C) 2000-2001                          --
--                                ACT-Europe                                --
--                                                                          --
--  Authors: Dmitriy Anisimkov - Pascal Obry                                --
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

--  $Id$

--  Inherit Sockets.Socket_FD to implement a SSL

with Ada.Streams;
with System;

with Sockets;

package SSL is

   Lib_Error        : exception;

   type Method is (SSLv2,
                   SSLv2_Server,
                   SSLv2_Client,
                   SSLv23,
                   SSLv23_Server,
                   SSLv23_Client,
                   Tlsv1,
                   Tlsv1_Server,
                   Tlsv1_Client,
                   SSLv3,
                   SSLv3_Server,
                   SSLv3_Client);

   procedure Init (Meth : Method);
   --  initialize the OpenSSL library with the right method.

   procedure Final;
   --  finalize the OpenSSL library by releasing all allocated memory.

   type Handle is new Sockets.Socket_FD with private;

   procedure Set_Certificate
     (Cert_Filename : in String;
      Key_Filename  : in String := "");

   procedure Set_Quiet_Shutdown (Value  : in Boolean := True);
   procedure Set_Sess_Cache_Size (Value : in Natural);

   procedure Set_Read_Ahead (Socket : in Handle; Value : in Boolean);

   procedure Accept_Socket
     (Socket     : in     Sockets.Socket_FD;
      New_Socket :    out Handle);

   procedure Socket
     (Sock   :    out Handle;
      Domain : in     Sockets.Socket_Domain := Sockets.AF_INET;
      Typ    : in     Sockets.Socket_Type   := Sockets.SOCK_STREAM);
   --  Create a socket of the given mode

   procedure Connect
     (Socket : in Handle;
      Host   : in String;
      Port   : in Positive);

   procedure Put (Socket : in Handle; Item : in String);

   procedure New_Line
     (Socket : in Handle;
      Count  : in Natural := 1);

   procedure Put_Line
     (Socket : in Handle;
      Item   : in String);

   function Receive
     (Socket : in Handle;
      Max    : in Ada.Streams.Stream_Element_Count := 4096)
     return Ada.Streams.Stream_Element_Array;

   procedure Send
     (Socket : in Handle;
      Data   : in Ada.Streams.Stream_Element_Array);

   function Pending (Socket : Handle) return Boolean;

   procedure Shutdown
     (Socket : in out Handle;
      How    : in     Sockets.Shutdown_Type := Sockets.Both);
   --  Shutdown the socket and SSL wrapper. When socket is shutdown completely
   --  (both sides) it automatically closes. Memory used by the SSL wrapper
   --  is still allocated and must be released with free procedure below.

   procedure Free (Socket : in out Handle);
   --  Releases memory associated with SSL wrapper.

   procedure Renegotiate (Socket : in Handle);

   procedure Do_Handshake (Socket : in Handle);

private

   subtype Pointer is System.Address;

   use type Pointer;

   Null_Ptr : constant Pointer := System.Null_Address;

   type Handle is new Sockets.Socket_FD with record
      H : Pointer := Null_Ptr;
   end record;

end SSL;
