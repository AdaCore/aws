------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2002-2018, AdaCore                     --
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

pragma Ada_2012;

--  This is the SSL based implementation of the Net package. The implementation
--  should depend only on AWS.Net.Std and the SSL library. It is important to
--  not call directly a socket binding here to ease porting.

with Ada.Calendar;

with System;

with AWS.Net.Std;
with SSL.Thin;

package AWS.Net.SSL is

   Socket_Error : exception renames Net.Socket_Error;

   type Socket_Type is new Net.Std.Socket_Type with private;

   type Session_Type is private;
   --  To keep session data over plain socket reconnect

   Null_Session : constant Session_Type;

   Is_Supported : constant Boolean;
   --  True if SSL supported in the current runtime

   type Debug_Output_Procedure is access procedure (Text : String);

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
      Last   : out Stream_Element_Offset)
     with Inline;

   overriding function Pending
     (Socket : Socket_Type) return Stream_Element_Count;
   --  Returns the number of bytes which are available inside socket
   --  for immediate read.

   --------------------
   -- Initialization --
   --------------------

   type Method is
     (TLS,     TLS_Server,     TLS_Client,      -- Highest available TLS
      TLSv1,   TLSv1_Server,   TLSv1_Client,    -- TLS 1.0
      TLSv1_1, TLSv1_1_Server, TLSv1_1_Client,  -- TLS 1.1
      TLSv1_2, TLSv1_2_Server, TLSv1_2_Client); -- TLS 1.2

   SSLv23        : constant Method := TLS
     with Obsolescent => "use TLS instead";
   SSLv23_Server : constant Method := TLS_Server
     with Obsolescent => "use TLS_Server instead";
   SSLv23_Client : constant Method := TLS_Client
     with Obsolescent => "use TLS_Client instead";
   SSLv3         : constant Method := TLS
     with Obsolescent => "use TLS instead";
   SSLv3_Server  : constant Method := TLS_Server
     with Obsolescent => "use TLS_Server instead";
   SSLv3_Client  : constant Method := TLS_Client
     with Obsolescent => "use TLS_Client instead";

   type Config is private;

   Null_Config : constant Config;

   procedure Initialize
     (Config               : in out SSL.Config;
      Certificate_Filename : String;
      Security_Mode        : Method     := TLS;
      Priorities           : String     := "";
      Ticket_Support       : Boolean    := False;
      Key_Filename         : String     := "";
      Exchange_Certificate : Boolean    := False;
      Certificate_Required : Boolean    := False;
      Trusted_CA_Filename  : String     := "";
      CRL_Filename         : String     := "";
      Session_Cache_Size   : Natural    := 16#4000#);
   --  Initialize the SSL layer into Config. Certificate_Filename must point
   --  to a valid certificate. Security mode can be used to change the
   --  security method used by AWS. Key_Filename must be specified if the key
   --  is not in the same file as the certificate. The Config object can be
   --  associated with all secure sockets sharing the same options. If
   --  Exchange_Certificate is True the client will send its certificate to
   --  the server, if False only the server will send its certificate.

   procedure Add_Host_Certificate
     (Config               : SSL.Config;
      Host                 : String;
      Certificate_Filename : String;
      Key_Filename         : String := "");
   --  Support for Server name indication (SNI). Client can ask for different
   --  host names on the same IP address. This routines provide a way to have
   --  different certificates for different server host names.

   procedure Initialize_Default_Config
     (Certificate_Filename : String;
      Security_Mode        : Method     := TLS;
      Priorities           : String     := "";
      Ticket_Support       : Boolean    := False;
      Key_Filename         : String     := "";
      Exchange_Certificate : Boolean    := False;
      Certificate_Required : Boolean    := False;
      Trusted_CA_Filename  : String     := "";
      CRL_Filename         : String     := "";
      Session_Cache_Size   : Natural    := 16#4000#);
   --  As above but for the default SSL configuration which is will be used
   --  for any socket not setting explicitly an SSL config object. Not that
   --  this routine can only be called once. Subsequent calls are no-op. To
   --  be effective it must be called before any SSL socket is created.

   procedure Release (Config : in out SSL.Config);
   --  Release memory associated with the Config object

   procedure Set_Config
     (Socket : in out Socket_Type; Config : SSL.Config);
   --  Set the SSL configuration object for the secure socket

   function Get_Config (Socket : Socket_Type) return SSL.Config with Inline;
   --  Get the SSL configuration object of the secure socket

   function Secure_Client
     (Socket : Net.Socket_Type'Class;
      Config : SSL.Config := Null_Config;
      Host   : String     := "") return Socket_Type;
   --  Make client side SSL connection from plain socket.
   --  SSL handshake does not performed. SSL handshake would be made
   --  automatically on first Read/Write, or explicitly by the Do_Handshake
   --  call. Do not free or close source socket after this call.
   --  Host parameter is hostname to connect and used to send over SSL
   --  connection to server if defined.

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

   function Session_Cache_Number
     (Config : SSL.Config := Null_Config) return Natural;
   --  Returns number of sessions currently in the cache.
   --  Null_Config mean default context.

   overriding function Cipher_Description (Socket : Socket_Type) return String;

   procedure Ciphers (Cipher : not null access procedure (Name : String));
   --  Calls callback Cipher for all available ciphers

   procedure Generate_DH;
   --  Regenerates Diffie-Hellman parameters.
   --  The call could take a quite long time.
   --  Diffie-Hellman parameters should be discarded and regenerated once a
   --  week or once a month. Depends on the security requirements.
   --  (gnutls/src/serv.c).

   procedure Generate_RSA;
   --  Regenerates RSA parameters.
   --  The call could take some time.
   --  RSA parameters should be discarded and regenerated once a day, once
   --  every 500 transactions etc. Depends on the security requirements
   --  (gnutls/src/serv.c).

   procedure Abort_DH_Generation with Inline;
   --  DH generation could be for a few minutes. If it is really necessary to
   --  terminate process faster, this call should be used.
   --  GNUTLS generates DH parameters much faster than OpenSSL, at least in
   --  Linux x86_64 and does not support DH generation abort at least in
   --  version 3.2.12.

   procedure Start_Parameters_Generation
     (DH : Boolean; Logging : access procedure (Text : String) := null)
     with Inline;
   --  Start SSL parameters regeneration in background.
   --  DH is False mean only RSA parameters generated.
   --  DH is True mean RSA and DH both parameters generated.

   function Generated_Time_DH return Ada.Calendar.Time with Inline;
   --  Returns date and time when the DH parameters was generated last time.
   --  Need to decide when new regeneration would start.

   function Generated_Time_RSA return Ada.Calendar.Time with Inline;
   --  Returns date and time when the RSA parameters was generated last time.
   --  Need to decide when new regeneration would start.

   procedure Set_Debug
     (Level : Natural; Output : Debug_Output_Procedure := null);
   --  Set debug information printed level and output callback.
   --  Null output callback mean output to Ada.Text_IO.Current_Error.

   function Session_Id_Image (Session : Session_Type) return String;
   --  Returns base64 encoded session id. Could be used to recognize resumed
   --  session when it has the same Id.

   function Session_Id_Image (Socket : Socket_Type) return String;
   --  Returns base64 encoded session id of the socket

   function Session_Data (Socket : Socket_Type) return Session_Type;
   --  For the client side SSL socket returns session data to be used to
   --  resume session after socket disconnected.

   procedure Free (Session : in out Session_Type);
   --  Free session data

   procedure Set_Session_Data
     (Socket : in out Socket_Type; Data : Session_Type);
   --  For the client side SSL socket try to resume session from data taken
   --  from previosly connected socket by Session_Data routine.

   function Session_Reused (Socket : Socket_Type) return Boolean;
   --  Returns True in case session was successfully reused after
   --  Set_Session_Data and handshake.

   type Private_Key is private;

   Null_Private_Key : constant Private_Key;

   type Hash_Method is (MD5, SHA1, SHA224, SHA256, SHA384, SHA512);

   function Load (Filename : String) return Private_Key;

   procedure Free (Key : in out Private_Key) with Inline;

   function Signature
     (Data : String;
      Key  : Private_Key;
      Hash : Hash_Method) return Stream_Element_Array with Inline;

   function Signature
     (Data : Stream_Element_Array;
      Key  : Private_Key;
      Hash : Hash_Method) return Stream_Element_Array with Inline;

private

   package TSSL renames Standard.SSL.Thin;

   Is_Supported : constant Boolean := Integer (TSSL.SSLeay) /= 0;

   Shutdown_Read_Timeout : constant Duration := 0.25;

   subtype SSL_Handle is TSSL.SSL_Handle;

   type TS_SSL;

   type Session_Type is access all TSSL.Session_Record;

   Null_Session : constant Session_Type := null;

   type Private_Key is new TSSL.Private_Key;

   Null_Private_Key : constant Private_Key :=
                        Private_Key (TSSL.Null_Private_Key);

   type Config is access all TS_SSL;
   pragma No_Strict_Aliasing (Config);

   Null_Config : constant Config := null;

   type Socket_Type is new Net.Std.Socket_Type with record
      Config : SSL.Config := Null_Config;
      SSL    : aliased SSL_Handle := TSSL.Null_Handle;
      Sessn  : Session_Type; -- Put client session before next connect
      IO     : TSSL.BIO_Access;
   end record;

   overriding procedure Free (Socket : in out Socket_Type);
   --  Release memory associated with the socket object

   procedure Set_Verify_Callback
     (Config : in out SSL.Config; Callback : System.Address);
   --  Record verify callback address into the SSL config

   procedure Log_Error (Text : String);
   --  Log error into Net error log

   DH_Lock  : Utils.Test_And_Set;
   RSA_Lock : Utils.Test_And_Set;

   type Time_Index is mod 2;

   DH_Time  : array (Time_Index) of Ada.Calendar.Time :=
                (0 => Utils.AWS_Epoch, 1 => <>);
   RSA_Time : array (Time_Index) of Ada.Calendar.Time :=
                (0 => Utils.AWS_Epoch, 1 => <>);
   --  Ada.Calendar.Time could not be Atomic in 32 bit platforms. Use Atomic
   --  index instead.

   DH_Time_Idx  : Time_Index := 0 with Atomic;
   RSA_Time_Idx : Time_Index := 0 with Atomic;

   Abort_DH_Flag : Boolean := False with Atomic;

   Debug_Output : Debug_Output_Procedure with Atomic;

   function Generated_Time_RSA return Ada.Calendar.Time is
     (RSA_Time (RSA_Time_Idx));

   function Generated_Time_DH return Ada.Calendar.Time is
     (DH_Time (DH_Time_Idx));

   function Get_Config (Socket : Socket_Type) return SSL.Config is
     (Socket.Config);

   function Signature
     (Ptr  : System.Address;
      Size : Interfaces.C.size_t;
      Key  : Private_Key;
      Hash : Hash_Method) return Stream_Element_Array;

   function Signature
     (Data : String;
      Key  : Private_Key;
      Hash : Hash_Method) return Stream_Element_Array
   is (Signature (Data'Address, Data'Length, Key, Hash));

   function Signature
     (Data : Stream_Element_Array;
      Key  : Private_Key;
      Hash : Hash_Method) return Stream_Element_Array
   is (Signature (Data'Address, Data'Length, Key, Hash));

end AWS.Net.SSL;
