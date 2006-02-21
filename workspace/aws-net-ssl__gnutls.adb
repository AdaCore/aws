------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                            Copyright (C) 2006                            --
--                                ACT-Europe                                --
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

with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

with AWS.Config;
with AWS.Net.Log;
with AWS.Utils;

with Interfaces.C.Strings;
with System;

package body AWS.Net.SSL is
   use Interfaces;

   use type C.unsigned;
   use type C.int;

   subtype NSST is Net.Std.Socket_Type;

   type Mutex_Access is access all AWS.Utils.Semaphore;

   subtype Stream_Array is
     Stream_Element_Array (1 .. Stream_Element_Offset'Last);

   procedure Check_Error_Code (Code : in C.int);
   procedure Check_Error_Code (Code : in C.int; Socket : in Socket_Type'Class);

   function Push
     (Socket : in Std.Socket_Type;
      Data   : in Stream_Array;
      Length : in Stream_Element_Count) return Stream_Element_Count;
   pragma Convention (C, Push);

   function Pull
     (Socket : in     Std.Socket_Type;
      Data   : access Stream_Array;
      Length : in     Stream_Element_Count) return Stream_Element_Count;
   pragma Convention (C, Pull);

   package Locking is

      function Init (Item : access Mutex_Access) return Integer;
      pragma Convention (C, Init);

      function Destroy (Item : access Mutex_Access) return Integer;
      pragma Convention (C, Destroy);

      function Lock (Item : access Mutex_Access) return Integer;
      pragma Convention (C, Lock);

      function Unlock (Item : access Mutex_Access) return Integer;
      pragma Convention (C, Unlock);

   end Locking;

   type TS_SSL is record
      ASC       : aliased TSSL.gnutls_anon_server_credentials_t;
      ACC       : aliased TSSL.gnutls_anon_client_credentials_t;
      DH_Params : aliased TSSL.gnutls_dh_params_t;
   end record;

   procedure Initialize
     (Config               : in out TS_SSL;
      Certificate_Filename : in     String;
      Security_Mode        : in     Method  := SSLv23;
      Key_Filename         : in     String  := "";
      Exchange_Certificate : in     Boolean := False);

   procedure Session_Client (Socket : in out Socket_Type);
   procedure Session_Server (Socket : in out Socket_Type);
   --  Bind the SSL socket handle with the socket

   procedure Session_Transport (Socket : in out Socket_Type);

   procedure Finalize (Config : in out TS_SSL);

   Default_Config : aliased TS_SSL;

   protected Default_Config_Synch is
      procedure Create_Default_Config;
   private
      Done : Boolean := False;
   end Default_Config_Synch;

   DH_Bits : constant := 1024;

   procedure Initialize_Default_Config;
   --  Initializes default config. It could be called more then once, because
   --  secondary initialization is ignored.

   procedure Secure
     (Source : in     Net.Socket_Type'Class;
      Target :    out Socket_Type;
      Config : in     SSL.Config);

   -------------------
   -- Accept_Socket --
   -------------------

   procedure Accept_Socket
     (Socket     : in     Net.Socket_Type'Class;
      New_Socket : in out Socket_Type) is
   begin
      Net.Std.Accept_Socket (Socket, NSST (New_Socket));

      if New_Socket.Config = null then
         Initialize_Default_Config;
         New_Socket.Config := Default_Config'Access;
      end if;

      Session_Server (New_Socket);
   end Accept_Socket;

   ----------------------
   -- Check_Error_Code --
   ----------------------

   procedure Check_Error_Code
     (Code : in C.int; Socket : in Socket_Type'Class) is
   begin
      if Code /= 0 then
         declare
            Error : constant String
              := C.Strings.Value (TSSL.gnutls_strerror (Code));
         begin
            Net.Log.Error (Socket, Error);
            Ada.Exceptions.Raise_Exception (Socket_Error'Identity, Error);
         end;
      end if;
   end Check_Error_Code;

   procedure Check_Error_Code (Code : in C.int) is
      Dummy : Socket_Type;
   begin
      Check_Error_Code (Code, Dummy);
   end Check_Error_Code;

   -------------
   -- Connect --
   -------------

   procedure Connect
     (Socket   : in out Socket_Type;
      Host     : in     String;
      Port     : in     Positive;
      Wait     : in     Boolean := True) is
   begin
      Net.Std.Connect (NSST (Socket), Host, Port, Wait);

      if Socket.Config = null then
         Initialize_Default_Config;
         Socket.Config := Default_Config'Access;
      end if;

      Session_Client (Socket);
   end Connect;

   --------------------------
   -- Default_Config_Synch --
   --------------------------

   protected body Default_Config_Synch is

      procedure Create_Default_Config is
         package CNF renames AWS.Config;
         Default : CNF.Object renames CNF.Default_Config;
      begin
         if not Done then
            Initialize
              (Config               => SSL.Default_Config,
               Certificate_Filename => CNF.Certificate (Default),
               Security_Mode        => Method'Value
                                         (CNF.Security_Mode (Default)),
               Key_Filename         => CNF.Key (Default),
               Exchange_Certificate => CNF.Exchange_Certificate (Default));

            Done := True;
         end if;
      end Create_Default_Config;

   end Default_Config_Synch;

   ------------------
   -- Do_Handshake --
   ------------------

   procedure Do_Handshake (Socket : in out Socket_Type) is
   begin
      Check_Error_Code (TSSL.gnutls_handshake (Socket.SSL));
   end Do_Handshake;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Config : in out TS_SSL) is
      use type TSSL.gnutls_anon_client_credentials_t;
      use type TSSL.gnutls_anon_server_credentials_t;
      use type TSSL.gnutls_dh_params_t;
   begin
      if Config.ASC /= null then
         TSSL.gnutls_anon_free_server_credentials (Config.ASC);
         Config.ASC := null;
      end if;

      if Config.ACC /= null then
         TSSL.gnutls_anon_free_client_credentials (Config.ACC);
         Config.ACC := null;
      end if;

      if Config.DH_Params /= null then
         TSSL.gnutls_dh_params_deinit (Config.DH_Params);
         Config.DH_Params := null;
      end if;
   end Finalize;

   ----------
   -- Free --
   ----------

   procedure Free (Socket : in out Socket_Type) is
      function To_Access is new Ada.Unchecked_Conversion
        (TSSL.gnutls_transport_ptr_t, Socket_Access);
      Sock : Socket_Access
        := To_Access (TSSL.gnutls_transport_get_ptr (Socket.SSL));
   begin
      Free (Sock);
      Net.Std.Free (NSST (Socket));

      TSSL.gnutls_deinit (Socket.SSL);
      Socket.SSL := null;
   end Free;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Config               : in out SSL.Config;
      Certificate_Filename : in     String;
      Security_Mode        : in     Method     := SSLv23;
      Key_Filename         : in     String     := "";
      Exchange_Certificate : in     Boolean    := False) is
   begin
      if Config = null then
         Config := new TS_SSL;
      end if;

      Initialize
        (Config.all,
         Certificate_Filename => Certificate_Filename,
         Security_Mode        => Security_Mode,
         Key_Filename         => Key_Filename,
         Exchange_Certificate => Exchange_Certificate);
   end Initialize;

   procedure Initialize
     (Config               : in out TS_SSL;
      Certificate_Filename : in     String;
      Security_Mode        : in     Method     := SSLv23;
      Key_Filename         : in     String     := "";
      Exchange_Certificate : in     Boolean    := False)
   is
      pragma Unreferenced -- ??? temporarily
        (Certificate_Filename, Key_Filename, Exchange_Certificate);

      use type TSSL.gnutls_anon_client_credentials_t;
      use type TSSL.gnutls_anon_server_credentials_t;
      use type TSSL.gnutls_dh_params_t;
   begin
      if (Security_Mode = SSLv2
          or Security_Mode = SSLv23
          or Security_Mode = TLSv1
          or Security_Mode = SSLv3
          or Security_Mode = SSLv2_Server
          or Security_Mode = SSLv23_Server
          or Security_Mode = TLSv1_Server
          or Security_Mode = SSLv3_Server)
        and then Config.ASC = null
      then
         Check_Error_Code
           (TSSL.gnutls_anon_allocate_server_credentials (Config.ASC'Access));

         Check_Error_Code
           (TSSL.gnutls_dh_params_init (Config.DH_Params'Access));
         Check_Error_Code
           (TSSL.gnutls_dh_params_generate2 (Config.DH_Params, DH_Bits));

         TSSL.gnutls_anon_set_server_dh_params (Config.ASC, Config.DH_Params);
      end if;

      if (Security_Mode = SSLv2
          or Security_Mode = SSLv23
          or Security_Mode = TLSv1
          or Security_Mode = SSLv3
          or Security_Mode = SSLv2_Client
          or Security_Mode = SSLv23_Client
          or Security_Mode = TLSv1_Client
          or Security_Mode = SSLv3_Client)
        and then Config.ACC = null
      then
         Check_Error_Code
           (TSSL.gnutls_anon_allocate_client_credentials (Config.ACC'Access));
      end if;
   end Initialize;

   -------------------------------
   -- Initialize_Default_Config --
   -------------------------------

   procedure Initialize_Default_Config is
   begin
      if Default_Config = (null, null, null) then
         Default_Config_Synch.Create_Default_Config;
      end if;
   end Initialize_Default_Config;

   -------------
   -- Locking --
   -------------

   package body Locking is
      use AWS.Utils;

      function Destroy (Item : access Mutex_Access) return Integer is
         procedure Free is
           new Ada.Unchecked_Deallocation (Semaphore, Mutex_Access);
      begin
         Free (Item.all);
         return 0;
      end Destroy;

      function Init (Item : access Mutex_Access) return Integer is
      begin
         Item.all := new Semaphore;
         return 0;
      end Init;

      function Lock (Item : access Mutex_Access) return Integer is
      begin
         Item.all.Seize;
         return 0;
      end Lock;

      function Unlock (Item : access Mutex_Access) return Integer is
      begin
         Item.all.Release;
         return 0;
      end Unlock;

   end Locking;

   -------------
   -- Pending --
   -------------

   function Pending (Socket : in Socket_Type) return Stream_Element_Count is
   begin
      return Stream_Element_Count
               (TSSL.gnutls_record_check_pending (Socket.SSL));
   end Pending;

   ----------
   -- Pull --
   ----------

   function Pull
     (Socket : in     Std.Socket_Type;
      Data   : access Stream_Array;
      Length : in     Stream_Element_Count) return Stream_Element_Count
   is
      Last : Stream_Element_Offset;
   begin
      Std.Receive (Socket, Data (1 .. Length), Last);
      return Last;
   end Pull;

   ----------
   -- Push --
   ----------

   function Push
     (Socket : in Std.Socket_Type;
      Data   : in Stream_Array;
      Length : in Stream_Element_Count) return Stream_Element_Count is
   begin
      Send (Socket, Data (1 .. Length));
      return Length;
   end Push;

   -------------
   -- Receive --
   -------------

   procedure Receive
     (Socket : in     Socket_Type;
      Data   :    out Stream_Element_Array;
      Last   :    out Stream_Element_Offset) is
   begin
      Last := Stream_Element_Offset
                (TSSL.gnutls_record_recv
                   (Socket.SSL, Data'Address, Data'Length));
   end Receive;

   -------------
   -- Release --
   -------------

   procedure Release (Config : in out SSL.Config) is
      procedure Free is new Ada.Unchecked_Deallocation (TS_SSL, SSL.Config);
   begin
      if Config /= null then
         Finalize (Config.all);
         Free (Config);
      end if;
   end Release;

   ------------
   -- Secure --
   ------------

   procedure Secure
     (Source : in     Net.Socket_Type'Class;
      Target :    out Socket_Type;
      Config : in     SSL.Config) is
   begin
      Std.Socket_Type (Target) := Std.Socket_Type (Source);

      if Config = null then
         Initialize_Default_Config;
         Target.Config := Default_Config'Access;
      else
         Target.Config := Config;
      end if;
   end Secure;

   -------------------
   -- Secure_Client --
   -------------------

   function Secure_Client
     (Socket : in Net.Socket_Type'Class;
      Config : in SSL.Config := Null_Config)
      return Socket_Type
   is
      Result : Socket_Type;
   begin
      Secure (Socket, Result, Config);
      Session_Client (Result);
      return Result;
   end Secure_Client;

   -------------------
   -- Secure_Server --
   -------------------

   function Secure_Server
     (Socket : in Net.Socket_Type'Class;
      Config : in SSL.Config := Null_Config)
      return Socket_Type
   is
      Result : Socket_Type;
   begin
      Secure (Socket, Result, Config);
      Session_Server (Result);
      return Result;
   end Secure_Server;

   ----------
   -- Send --
   ----------

   procedure Send
     (Socket : in     Socket_Type;
      Data   : in     Stream_Element_Array;
      Last   :    out Stream_Element_Offset) is
   begin
      Last := Stream_Element_Offset
                (TSSL.gnutls_record_send
                   (Socket.SSL, Data'Address, Data'Length));
   end Send;

   --------------------
   -- Session_Client --
   --------------------

   procedure Session_Client (Socket : in out Socket_Type) is
      Session : aliased TSSL.gnutls_session_t;
   begin
      Check_Error_Code
        (TSSL.gnutls_init (Session'Access, TSSL.GNUTLS_CLIENT), Socket);

      Socket.SSL := Session;

      Check_Error_Code
        (TSSL.gnutls_set_default_priority (Session), Socket);

      Check_Error_Code
        (TSSL.gnutls_credentials_set
           (Session, TSSL.GNUTLS_CRD_ANON, Socket.Config.ACC.all'Address));

      Session_Transport (Socket);
   end Session_Client;

   --------------------
   -- Session_Server --
   --------------------

   procedure Session_Server (Socket : in out Socket_Type) is
      Session : aliased TSSL.gnutls_session_t;
   begin
      Check_Error_Code
        (TSSL.gnutls_init (Session'Access, TSSL.GNUTLS_SERVER), Socket);

      Socket.SSL := Session;

      Check_Error_Code
        (TSSL.gnutls_set_default_priority (Session), Socket);

      Check_Error_Code
        (TSSL.gnutls_credentials_set
           (Session, TSSL.GNUTLS_CRD_ANON, Socket.Config.ASC.all'Address));

      TSSL.gnutls_dh_set_prime_bits (Session, DH_Bits);

      Session_Transport (Socket);
   end Session_Server;

   -----------------------
   -- Session_Transport --
   -----------------------

   procedure Session_Transport (Socket : in out Socket_Type) is
      type Std_Access is access all Net.Std.Socket_Type;
      Sock : constant Std_Access
        := new Std.Socket_Type'(Std.Socket_Type (Socket));
   begin
      TSSL.gnutls_transport_set_ptr
        (Socket.SSL, TSSL.gnutls_transport_ptr_t (Sock.all'Address));
      TSSL.gnutls_transport_set_push_function (Socket.SSL, Push'Address);
      TSSL.gnutls_transport_set_pull_function (Socket.SSL, Pull'Address);
      TSSL.gnutls_transport_set_lowat (Socket.SSL, 0);
   end Session_Transport;

   ----------------
   -- Set_Config --
   ----------------

   procedure Set_Config
     (Socket : in out Socket_Type;
      Config : in     SSL.Config) is
   begin
      Socket.Config := Config;
   end Set_Config;

   --------------
   -- Shutdown --
   --------------

   procedure Shutdown (Socket : in Socket_Type) is
      Code : constant C.int
        := TSSL.gnutls_bye (Socket.SSL, TSSL.GNUTLS_SHUT_RDWR);
   begin
      if Code /= 0 then
         Net.Log.Error (Socket, C.Strings.Value (TSSL.gnutls_strerror (Code)));
      end if;

      Net.Std.Shutdown (NSST (Socket));
   end Shutdown;

   -----------------
   -- Socket_Pair --
   -----------------

   procedure Socket_Pair (S1, S2 : out Socket_Type) is
      ST1, ST2 : Std.Socket_Type;
   begin
      Std.Socket_Pair (ST1, ST2);
      S1 := Secure_Server (ST1);
      S2 := Secure_Client (ST2);
   end Socket_Pair;

begin
   if TSSL.gcry_control
        (Thread_CBS => (Option        => TSSL.GCRY_THREAD_OPTION_USER,
                        Init          => System.Null_Address,
                        Mutex_Init    => Locking.Init'Address,
                        Mutex_Destroy => Locking.Destroy'Address,
                        Mutex_Lock    => Locking.Lock'Address,
                        Mutex_Unlock  => Locking.Unlock'Address,
                        others        => System.Null_Address)) /= 0
   then
      raise Program_Error;
   end if;

   if TSSL.gnutls_global_init /= 0 then
      raise Program_Error;
   end if;
end AWS.Net.SSL;
