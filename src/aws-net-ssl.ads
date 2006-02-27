------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2002-2006                          --
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

--  $Id$

--  This is the SSL based implementation of the Net package. The implementation
--  should depend only on AWS.Net.Std and the SSL library. It is important to
--  not call directly a socket binding here to ease porting.

with AWS.Net.Std;
with SSL.Thin;

package AWS.Net.SSL is

   type Socket_Type is new Net.Std.Socket_Type with private;

   Is_Supported : constant Boolean;
   --  True if SSL supported in the current runtime

   ----------------
   -- Initialize --
   ----------------

   procedure Accept_Socket
     (Socket     : in     Net.Socket_Type'Class;
      New_Socket : in out Socket_Type);
   --  Accept a connection on a socket

   procedure Connect
     (Socket : in out Socket_Type;
      Host   : in     String;
      Port   : in     Positive;
      Wait   : in     Boolean := True);
   --  Connect a socket on a given host/port. If Wait is True Connect will wait
   --  for the connection to be established for timeout seconds, specified by
   --  Set_Timeout routine. If Wait is False Connect will return immediately,
   --  not waiting for the connection to be establised and it does not make the
   --  SSL handshake. It is possible to wait for the Connection completion by
   --  calling Wait routine with Output set to True in Events parameter.

   procedure Shutdown (Socket : in Socket_Type);
   --  Shutdown both side of the socket and close it

   procedure Free (Socket : in out Socket_Type);
   --  Release memory associated with the socket object

   --------
   -- IO --
   --------

   procedure Send
     (Socket : in     Socket_Type;
      Data   : in     Stream_Element_Array;
      Last   :    out Stream_Element_Offset);

   procedure Receive
     (Socket : in     Socket_Type;
      Data   :    out Stream_Element_Array;
      Last   :    out Stream_Element_Offset);
   pragma Inline (Receive);

   function Pending (Socket : in Socket_Type) return Stream_Element_Count;
   --  Returns the number of bytes which are available inside socket
   --  for immediate read.

   --------------------
   -- Initialization --
   --------------------

   type Method is
     (SSLv2,  SSLv2_Server,  SSLv2_Client,
      SSLv23, SSLv23_Server, SSLv23_Client,
      TLSv1,  TLSv1_Server,  TLSv1_Client,
      SSLv3,  SSLv3_Server,  SSLv3_Client);

   type Config is private;

   Null_Config : constant Config;

   procedure Initialize
     (Config               : in out SSL.Config;
      Certificate_Filename : in     String;
      Security_Mode        : in     Method     := SSLv23;
      Key_Filename         : in     String     := "";
      Exchange_Certificate : in     Boolean    := False);
   --  Initialize the SSL layer into Config. Certificate_Filename must point
   --  to a valid certificate. Security mode can be used to change the
   --  security method used by AWS. Key_Filename must be specified if the key
   --  is not in the same file as the certificate. The Config object can be
   --  associated with all secure sockets sharing the same options. If
   --  Exchange_Certificate is True the client will send it's certificate to
   --  the server, if False only the server will send its certificate.

   procedure Release (Config : in out SSL.Config);
   --  Release memory associated with the Config object

   procedure Set_Config
     (Socket : in out Socket_Type;
      Config : in     SSL.Config);
   --  Set the SSL configuration object for the secure socket

   function Secure_Client
     (Socket : in Net.Socket_Type'Class;
      Config : in SSL.Config := Null_Config) return Socket_Type;
   --  Make client side SSL connection from plain socket.
   --  SSL handshake does not performed. SSL handshake would be made
   --  automatically on first Read/Write, or explicitly by the Do_Handshake
   --  call. Do not free or close source socket after this call.

   function Secure_Server
     (Socket : in Net.Socket_Type'Class;
      Config : in SSL.Config := Null_Config) return Socket_Type;
   --  Make server side SSL connection from plain socket.
   --  SSL handshake does not performed. SSL handshake would be made
   --  automatically on first Read/Write, or explicitly by the Do_Handshake
   --  call. Do not free or close source socket after this call.

   procedure Do_Handshake (Socket : in out Socket_Type);
   --  Wait for a SSL/TLS handshake to take place. You need to call this
   --  routine if you have converted a standard socket to secure one and need
   --  to get the peer certificate.

private

   package TSSL renames Standard.SSL.Thin;

   Is_Supported : constant Boolean := Integer (TSSL.SSLeay) /= 0;

   subtype SSL_Handle is TSSL.SSL_Handle;

   protected type TS_SSL is

      procedure Set_FD (Socket : in out Socket_Type);
      --  Bind the SSL socket handle with the socket

      procedure Initialize
        (Certificate_Filename : in String;
         Security_Mode        : in Method;
         Key_Filename         : in String;
         Exchange_Certificate : in Boolean);

      procedure Finalize;

   private
      Context : TSSL.SSL_CTX := TSSL.Null_Pointer;
   end TS_SSL;

   type Config is access TS_SSL;

   Null_Config : constant Config := null;

   type Socket_Type is new Net.Std.Socket_Type with record
      Config : SSL.Config;
      SSL    : SSL_Handle := TSSL.Null_Pointer;
   end record;

end AWS.Net.SSL;
