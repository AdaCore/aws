------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2002-2013, AdaCore                     --
--                                                                          --
--  This library is free software;  you can redistribute it and/or modify   --
--  it under terms of the  GNU General Public License  as published by the  --
--  Free Software  Foundation;  either version 3,  or (at your  option) any --
--  later version. This library is distributed in the hope that it will be  --
--  useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                    --
--                                                                          --
--  As a special exception under Section 7 of GPL version 3, you are        --
--  granted additional permissions described in the GCC Runtime Library     --
--  Exception, version 3.1, as published by the Free Software Foundation.   --
--                                                                          --
--  You should have received a copy of the GNU General Public License and   --
--  a copy of the GCC Runtime Library Exception along with this program;    --
--  see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see   --
--  <http://www.gnu.org/licenses/>.                                         --
--                                                                          --
--  As a special exception, if other files instantiate generics from this   --
--  unit, or you link this unit with other files to produce an executable,  --
--  this  unit  does not  by itself cause  the resulting executable to be   --
--  covered by the GNU General Public License. This exception does not      --
--  however invalidate any other reasons why the executable file  might be  --
--  covered by the  GNU Public License.                                     --
------------------------------------------------------------------------------

--  This is the SSL based implementation of the Net package. The implementation
--  should depend only on AWS.Net.Std and the SSL library. It is important to
--  not call directly a socket binding here to ease porting.

with System;

with AWS.Net.Std;
with SSL.Thin;

package AWS.Net.SSL is

   Socket_Error : exception renames Net.Socket_Error;

   type Socket_Type is new Net.Std.Socket_Type with private;

   Is_Supported : constant Boolean;
   --  True if SSL supported in the current runtime

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Accept_Socket
     (Socket : Net.Socket_Type'Class; New_Socket : in out Socket_Type);
   --  Accept a connection on a socket

   overriding procedure Connect
     (Socket : in out Socket_Type;
      Host   : String;
      Port   : Positive;
      Wait   : Boolean     := True;
      Family : Family_Type := Family_Unspec);
   --  Connect a socket on a given host/port. If Wait is True Connect will wait
   --  for the connection to be established for timeout seconds, specified by
   --  Set_Timeout routine. If Wait is False Connect will return immediately,
   --  not waiting for the connection to be establised and it does not make the
   --  SSL handshake. It is possible to wait for the Connection completion by
   --  calling Wait routine with Output set to True in Events parameter.

   overriding procedure Socket_Pair (S1, S2 : out Socket_Type);
   --  Create 2 sockets and connect them together

   overriding procedure Shutdown
     (Socket : Socket_Type; How : Shutmode_Type := Shut_Read_Write);
   --  Shutdown the read, write or both side of the socket.
   --  If How is Both, close it. Does not raise Socket_Error if the socket is
   --  not connected or already shutdown.

   --------
   -- IO --
   --------

   overriding procedure Send
     (Socket : Socket_Type;
      Data   : Stream_Element_Array;
      Last   : out Stream_Element_Offset);

   overriding procedure Receive
     (Socket : Socket_Type;
      Data   : out Stream_Element_Array;
      Last   : out Stream_Element_Offset);
   pragma Inline (Receive);

   overriding function Pending
     (Socket : Socket_Type) return Stream_Element_Count;
   --  Returns the number of bytes which are available inside socket
   --  for immediate read.

   --------------------
   -- Initialization --
   --------------------

   type Method is
     (SSLv23, SSLv23_Server, SSLv23_Client,
      TLSv1,  TLSv1_Server,  TLSv1_Client,
      SSLv3,  SSLv3_Server,  SSLv3_Client);

   type Config is private;

   Null_Config : constant Config;

   procedure Initialize
     (Config               : in out SSL.Config;
      Certificate_Filename : String;
      Security_Mode        : Method     := SSLv23;
      Key_Filename         : String     := "";
      Exchange_Certificate : Boolean    := False;
      Certificate_Required : Boolean    := False;
      Trusted_CA_Filename  : String     := "";
      CRL_Filename         : String     := "";
      Session_Cache_Size   : Positive   := 16#4000#);
   --  Initialize the SSL layer into Config. Certificate_Filename must point
   --  to a valid certificate. Security mode can be used to change the
   --  security method used by AWS. Key_Filename must be specified if the key
   --  is not in the same file as the certificate. The Config object can be
   --  associated with all secure sockets sharing the same options. If
   --  Exchange_Certificate is True the client will send its certificate to
   --  the server, if False only the server will send its certificate.

   procedure Release (Config : in out SSL.Config);
   --  Release memory associated with the Config object

   procedure Set_Config
     (Socket : in out Socket_Type; Config : SSL.Config);
   --  Set the SSL configuration object for the secure socket

   function Secure_Client
     (Socket : Net.Socket_Type'Class;
      Config : SSL.Config := Null_Config) return Socket_Type;
   --  Make client side SSL connection from plain socket.
   --  SSL handshake does not performed. SSL handshake would be made
   --  automatically on first Read/Write, or explicitly by the Do_Handshake
   --  call. Do not free or close source socket after this call.

   function Secure_Server
     (Socket : Net.Socket_Type'Class;
      Config : SSL.Config := Null_Config) return Socket_Type;
   --  Make server side SSL connection from plain socket.
   --  SSL handshake does not performed. SSL handshake would be made
   --  automatically on first Read/Write, or explicitly by the Do_Handshake
   --  call. Do not free or close source socket after this call.

   procedure Do_Handshake (Socket : in out Socket_Type);
   --  Wait for a SSL/TLS handshake to take place. You need to call this
   --  routine if you have converted a standard socket to secure one and need
   --  to get the peer certificate.

   function Version (Build_Info : Boolean := False) return String;
   --  Returns version information

   procedure Clear_Session_Cache (Config : SSL.Config := Null_Config);
   --  Remove all sessions from SSL session cache from the SSL context.
   --  Null_Config mean default context.

   procedure Set_Session_Cache_Size
     (Size : Natural; Config : SSL.Config := Null_Config);
   --  Set session cache size in the SSL context.
   --  Null_Config mean default context.

private

   package TSSL renames Standard.SSL.Thin;

   Is_Supported : constant Boolean := Integer (TSSL.SSLeay) /= 0;

   subtype SSL_Handle is TSSL.SSL_Handle;

   type TS_SSL;

   type Config is access all TS_SSL;

   Null_Config : constant Config := null;

   type Socket_Type is new Net.Std.Socket_Type with record
      Config : SSL.Config := Null_Config;
      SSL    : aliased SSL_Handle := TSSL.Null_Handle;
      IO     : TSSL.BIO_Access;
   end record;

   overriding procedure Free (Socket : in out Socket_Type);
   --  Release memory associated with the socket object

   procedure Set_Verify_Callback
     (Config : in out SSL.Config; Callback : System.Address);
   --  Record verify callback address into the SSL config

end AWS.Net.SSL;
