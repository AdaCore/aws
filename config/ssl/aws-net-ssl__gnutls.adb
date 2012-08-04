------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2006-2012, AdaCore                     --
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

with Ada.Directories;
with Ada.Unchecked_Deallocation;

with Interfaces.C.Strings;

with AWS.Config;
with AWS.Net.Log;
with AWS.Utils;

package body AWS.Net.SSL is

   use Interfaces;

   use type C.int;
   use type C.unsigned;

   subtype NSST is Net.Std.Socket_Type;

   type Mutex_Access is access all AWS.Utils.Semaphore;

   procedure Check_Error_Code (Code : C.int);
   procedure Check_Error_Code (Code : C.int; Socket : Socket_Type'Class);

   procedure Check_Error_Code (Code : TSSL.gcry_error_t);

   procedure Code_Processing
     (Code : C.int; Socket : Socket_Type'Class; Timeout : Duration);

   procedure Code_Processing (Code : C.int; Socket : Socket_Type'Class);

   procedure Check_Config (Socket : in out Socket_Type);
   pragma Inline (Check_Config);

   procedure Do_Handshake_Internal (Socket : Socket_Type);

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
      CSC       : aliased TSSL.gnutls_certificate_credentials_t;
      CCC       : aliased TSSL.gnutls_certificate_credentials_t;
      DH_Params : aliased TSSL.gnutls_dh_params_t;
      RCC       : Boolean := False; -- Request client certificate
      CREQ      : Boolean := False; -- Certificate is required
      CAfile    : C.Strings.chars_ptr := C.Strings.Null_Ptr;
   end record;

   procedure Initialize
     (Config               : in out TS_SSL;
      Certificate_Filename : String;
      Security_Mode        : Method  := SSLv23;
      Key_Filename         : String  := "";
      Exchange_Certificate : Boolean := False;
      Certificate_Required : Boolean    := False;
      Trusted_CA_Filename  : String     := "";
      Session_Cache_Size   : Positive   := 16#4000#);

   procedure Session_Client (Socket : in out Socket_Type);
   procedure Session_Server (Socket : in out Socket_Type);
   --  Bind the SSL socket handle with the socket

   procedure Session_Transport (Socket : in out Socket_Type);

   function Verify_Callback (Session : TSSL.gnutls_session_t) return C.int;
   pragma Convention (C, Verify_Callback);

   procedure Finalize (Config : in out TS_SSL);

   Default_Config : aliased TS_SSL;

   protected Default_Config_Synch is
      procedure Create_Default_Config;
   private
      Done : Boolean := False;
   end Default_Config_Synch;

   procedure Initialize_Default_Config;
   --  Initializes default config. It could be called more then once, because
   --  secondary initialization is ignored.

   procedure Secure
     (Source : Net.Socket_Type'Class;
      Target : out Socket_Type;
      Config : SSL.Config);

   -------------------
   -- Accept_Socket --
   -------------------

   overriding procedure Accept_Socket
     (Socket : Net.Socket_Type'Class; New_Socket : in out Socket_Type) is
   begin
      loop
         Net.Std.Accept_Socket (Socket, NSST (New_Socket));
         Session_Server (New_Socket);

         begin
            Do_Handshake (New_Socket);
            exit;
         exception
            when Socket_Error =>
               New_Socket.Shutdown;
         end;
      end loop;
   end Accept_Socket;

   ------------------
   -- Check_Config --
   ------------------

   procedure Check_Config (Socket : in out Socket_Type) is
   begin
      if Socket.Config = null then
         Initialize_Default_Config;
         Socket.Config := Default_Config'Access;
      end if;
   end Check_Config;

   ----------------------
   -- Check_Error_Code --
   ----------------------

   procedure Check_Error_Code (Code : C.int; Socket : Socket_Type'Class) is
   begin
      if Code /= 0 then
         declare
            Error : constant String :=
                      C.Strings.Value (TSSL.gnutls_strerror (Code));
         begin
            Net.Log.Error (Socket, Error);
            raise Socket_Error with Error;
         end;
      end if;
   end Check_Error_Code;

   procedure Check_Error_Code (Code : C.int) is
      Dummy : Socket_Type;
   begin
      Check_Error_Code (Code, Dummy);
   end Check_Error_Code;

   procedure Check_Error_Code (Code : TSSL.gpg_error_t) is
   begin
      if Code = 0 then
         return;
      end if;

      raise Program_Error with
        '[' & Code'Img & "] " & C.Strings.Value (TSSL.gcry_strerror (Code))
        & '/' & C.Strings.Value (TSSL.gcry_strsource (Code));
   end Check_Error_Code;

   -------------------------
   -- Clear_Session_Cache --
   -------------------------

   procedure Clear_Session_Cache (Config : SSL.Config := Null_Config) is
   begin
      null;
   end Clear_Session_Cache;

   ---------------------
   -- Code_Processing --
   ---------------------

   procedure Code_Processing
     (Code : C.int; Socket : Socket_Type'Class; Timeout : Duration) is
   begin
      case Code is
         when TSSL.GNUTLS_E_INTERRUPTED | TSSL.GNUTLS_E_AGAIN =>
            case TSSL.gnutls_record_get_direction (Socket.SSL) is
               when 0      => Wait_For (Input, Socket, Timeout);
               when 1      => Wait_For (Output, Socket, Timeout);
               when others => raise Program_Error;
            end case;

         when others =>
            Check_Error_Code (Code, Socket);
      end case;
   end Code_Processing;

   procedure Code_Processing (Code : C.int; Socket : Socket_Type'Class) is
   begin
      Code_Processing (Code, Socket, Net.Socket_Type (Socket).Timeout);
   end Code_Processing;

   -------------
   -- Connect --
   -------------

   overriding procedure Connect
     (Socket : in out Socket_Type;
      Host   : String;
      Port   : Positive;
      Wait   : Boolean     := True;
      Family : Family_Type := Family_Unspec) is
   begin
      Net.Std.Connect (NSST (Socket), Host, Port, Wait, Family);

      Session_Client (Socket);

      if Wait then
         Do_Handshake (Socket);
      end if;
   end Connect;

   --------------------------
   -- Default_Config_Synch --
   --------------------------

   protected body Default_Config_Synch is

      ---------------------------
      -- Create_Default_Config --
      ---------------------------

      procedure Create_Default_Config is
         package CNF renames AWS.Config;
         Default : CNF.Object renames CNF.Default_Config;
      begin
         if not Done then
            Initialize
              (Config               => Default_Config,
               Certificate_Filename => CNF.Certificate (Default),
               Security_Mode        => Method'Value
                                         (CNF.Security_Mode (Default)),
               Key_Filename         => CNF.Key (Default),
               Exchange_Certificate => CNF.Exchange_Certificate (Default),
               Certificate_Required => CNF.Certificate_Required (Default));

            Done := True;
         end if;
      end Create_Default_Config;

   end Default_Config_Synch;

   ------------------
   -- Do_Handshake --
   ------------------

   procedure Do_Handshake (Socket : in out Socket_Type) is
   begin
      Do_Handshake_Internal (Socket);
   end Do_Handshake;

   procedure Do_Handshake_Internal (Socket : Socket_Type) is
      Code : TSSL.ssize_t;
   begin
      loop
         Code := TSSL.gnutls_handshake (Socket.SSL);

         exit when Code = TSSL.GNUTLS_E_SUCCESS;

         Code_Processing (Code, Socket);
      end loop;

      Socket.IO.Handshaken.all := True;
   end Do_Handshake_Internal;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Config : in out TS_SSL) is
      use type TSSL.gnutls_anon_client_credentials_t;
      use type TSSL.gnutls_anon_server_credentials_t;
      use type TSSL.gnutls_certificate_credentials_t;
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

      if Config.CSC /= null then
         TSSL.gnutls_certificate_free_credentials (Config.CSC);
         Config.CSC := null;
      end if;

      if Config.DH_Params /= null then
         TSSL.gnutls_dh_params_deinit (Config.DH_Params);
         Config.DH_Params := null;
      end if;

      C.Strings.Free (Config.CAfile);
   end Finalize;

   ----------
   -- Free --
   ----------

   overriding procedure Free (Socket : in out Socket_Type) is
      use type TSSL.gnutls_session_t;
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Boolean, TSSL.Boolean_Access);
   begin
      if Socket.SSL /= null then
         TSSL.gnutls_deinit (Socket.SSL);
         Socket.SSL := null;
      end if;

      Unchecked_Free (Socket.IO.Handshaken);
      Net.Std.Free (NSST (Socket));
   end Free;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Config               : in out SSL.Config;
      Certificate_Filename : String;
      Security_Mode        : Method     := SSLv23;
      Key_Filename         : String     := "";
      Exchange_Certificate : Boolean    := False;
      Certificate_Required : Boolean    := False;
      Trusted_CA_Filename  : String     := "";
      Session_Cache_Size   : Positive   := 16#4000#) is
   begin
      if Config = null then
         Config := new TS_SSL;
      end if;

      Initialize
        (Config.all,
         Certificate_Filename => Certificate_Filename,
         Security_Mode        => Security_Mode,
         Key_Filename         => Key_Filename,
         Exchange_Certificate => Exchange_Certificate,
         Certificate_Required => Certificate_Required,
         Trusted_CA_Filename  => Trusted_CA_Filename,
         Session_Cache_Size   => Session_Cache_Size);
   end Initialize;

   procedure Initialize
     (Config               : in out TS_SSL;
      Certificate_Filename : String;
      Security_Mode        : Method     := SSLv23;
      Key_Filename         : String     := "";
      Exchange_Certificate : Boolean    := False;
      Certificate_Required : Boolean    := False;
      Trusted_CA_Filename  : String     := "";
      Session_Cache_Size   : Positive   := 16#4000#)
   is
      pragma Unreferenced (Certificate_Required, Trusted_CA_Filename);
      pragma Unreferenced (Session_Cache_Size);

      use type TSSL.gnutls_anon_client_credentials_t;
      use type TSSL.gnutls_anon_server_credentials_t;
      use type TSSL.gnutls_certificate_credentials_t;
      use type TSSL.gnutls_dh_params_t;

      procedure Set_Certificate (CC : TSSL.gnutls_certificate_credentials_t);
      --  Set credentials from Cetificate_Filename and Key_Filename

      ---------------------
      -- Set_Certificate --
      ---------------------

      procedure Set_Certificate (CC : TSSL.gnutls_certificate_credentials_t) is

         procedure Check_File (Prefix, Filename : String);
         --  Check that Filename is present, raise an exception adding
         --  Prefix in front of the message.

         ----------------
         -- Check_File --
         ----------------

         procedure Check_File (Prefix, Filename : String) is
            use type Directories.File_Kind;
         begin
            if Directories.Kind (Filename) /= Directories.Ordinary_File then
               raise Socket_Error
                 with Prefix & " file """ & Filename & """ error.";
            end if;
         end Check_File;

         Code : C.int;

      begin
         Check_File ("Certificate", Certificate_Filename);

         declare
            use C.Strings;
            Cert : aliased C.char_array := C.To_C (Certificate_Filename);
            Key  : aliased C.char_array := C.To_C (Key_Filename);
            CP   : constant chars_ptr := To_Chars_Ptr (Cert'Unchecked_Access);
            KP   : chars_ptr;
         begin
            if Key_Filename = "" then
               KP := CP;
            else
               Check_File ("Key", Key_Filename);
               KP := To_Chars_Ptr (Key'Unchecked_Access);
            end if;

            Code := TSSL.gnutls_certificate_set_x509_key_file
                      (CC, CP, KP, TSSL.GNUTLS_X509_FMT_PEM);

            if Code = TSSL.GNUTLS_E_BASE64_DECODING_ERROR then
               raise Socket_Error with "Certificate/Key file error.";
            else
               Check_Error_Code (Code);
            end if;
         end;
      end Set_Certificate;

   begin
      if (Security_Mode = SSLv23
          or else Security_Mode = TLSv1
          or else Security_Mode = SSLv3
          or else Security_Mode = SSLv23_Server
          or else Security_Mode = TLSv1_Server
          or else Security_Mode = SSLv3_Server)
        and then Config.ASC = null
        and then Config.CSC = null
      then
         Check_Error_Code
           (TSSL.gnutls_dh_params_init (Config.DH_Params'Access));

         if Certificate_Filename = "" then
            Check_Error_Code
              (TSSL.gnutls_anon_allocate_server_credentials
                 (Config.ASC'Access));
            TSSL.gnutls_anon_set_server_dh_params
              (Config.ASC, Config.DH_Params);

         else
            Check_Error_Code
              (TSSL.gnutls_certificate_allocate_credentials
                 (Config.CSC'Access));

            Set_Certificate (Config.CSC);

            TSSL.gnutls_certificate_set_verify_function
              (cred => Config.CSC, func => Verify_Callback'Access);

            TSSL.gnutls_certificate_set_dh_params
              (Config.CSC, Config.DH_Params);
         end if;

         Config.RCC := Exchange_Certificate;
         Config.CREQ := Certificate_Required;

         if Trusted_CA_Filename /= "" then
            Config.CAfile := C.Strings.New_String (Trusted_CA_Filename);
         end if;
      end if;

      if (Security_Mode = SSLv23
          or else Security_Mode = TLSv1
          or else Security_Mode = SSLv3
          or else Security_Mode = SSLv23_Client
          or else Security_Mode = TLSv1_Client
          or else Security_Mode = SSLv3_Client)
        and then Config.ACC = null
        and then Config.CCC = null
      then
         Check_Error_Code
           (TSSL.gnutls_anon_allocate_client_credentials (Config.ACC'Access));

         Check_Error_Code
           (TSSL.gnutls_certificate_allocate_credentials (Config.CCC'Access));
         --  It is a strange, but we have to allocate client certificate
         --  credentials even if we would not assign certificate over there.
         --  Checked in GNUTLS 3.0.3.

         if Certificate_Filename /= "" then
            Set_Certificate (Config.CCC);
         end if;
      end if;
   end Initialize;

   -------------------------------
   -- Initialize_Default_Config --
   -------------------------------

   procedure Initialize_Default_Config is
   begin
      Default_Config_Synch.Create_Default_Config;
   end Initialize_Default_Config;

   -------------
   -- Locking --
   -------------

   package body Locking is
      use AWS.Utils;

      procedure Finalize;

      Working : Boolean := True;

      F : Utils.Finalizer (Finalize'Access);
      pragma Unreferenced (F);

      -------------
      -- Destroy --
      -------------

      function Destroy (Item : access Mutex_Access) return Integer is
         procedure Free is
           new Ada.Unchecked_Deallocation (Semaphore, Mutex_Access);
      begin
         Free (Item.all);
         return 0;
      end Destroy;

      --------------
      -- Finalize --
      --------------

      procedure Finalize is
      begin
         Working := False;
      end Finalize;

      ----------
      -- Init --
      ----------

      function Init (Item : access Mutex_Access) return Integer is
      begin
         if Working then
            Item.all := new Semaphore;
         end if;
         return 0;
      end Init;

      ----------
      -- Lock --
      ----------

      function Lock (Item : access Mutex_Access) return Integer is
      begin
         if Working then
            Item.all.Seize;
         end if;
         return 0;
      end Lock;

      ------------
      -- Unlock --
      ------------

      function Unlock (Item : access Mutex_Access) return Integer is
      begin
         if Working then
            Item.all.Release;
         end if;
         return 0;
      end Unlock;

   end Locking;

   -------------
   -- Pending --
   -------------

   overriding function Pending
     (Socket : Socket_Type) return Stream_Element_Count is
   begin
      return Stream_Element_Count
               (TSSL.gnutls_record_check_pending (Socket.SSL));
   end Pending;

   -------------
   -- Receive --
   -------------

   overriding procedure Receive
     (Socket : Socket_Type;
      Data   : out Stream_Element_Array;
      Last   : out Stream_Element_Offset)
   is
      Code : TSSL.ssize_t;
   begin
      if not Socket.IO.Handshaken.all then
         Do_Handshake_Internal (Socket);
      end if;

      loop
         Code :=
           TSSL.gnutls_record_recv (Socket.SSL, Data'Address, Data'Length);

         if Code = 0 then
            raise Socket_Error with Peer_Closed_Message;
         elsif Code > 0 then
            Last := Data'First + Stream_Element_Offset (Code) - 1;
            exit;
         end if;

         Code_Processing (Code, Socket);
      end loop;
   end Receive;

   -------------
   -- Release --
   -------------

   procedure Release (Config : in out SSL.Config) is
      procedure Free is new Ada.Unchecked_Deallocation (TS_SSL, SSL.Config);
   begin
      if Config /= null and then Config /= Default_Config'Access then
         Finalize (Config.all);
         Free (Config);
      end if;
   end Release;

   ------------
   -- Secure --
   ------------

   procedure Secure
     (Source : Net.Socket_Type'Class;
      Target : out Socket_Type;
      Config : SSL.Config) is
   begin
      Std.Socket_Type (Target) := Std.Socket_Type (Source);
      Target.Config := Config;
      Check_Config (Target);
   end Secure;

   -------------------
   -- Secure_Client --
   -------------------

   function Secure_Client
     (Socket : Net.Socket_Type'Class;
      Config : SSL.Config := Null_Config) return Socket_Type
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
     (Socket : Net.Socket_Type'Class;
      Config : SSL.Config := Null_Config) return Socket_Type
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

   overriding procedure Send
     (Socket : Socket_Type;
      Data   : Stream_Element_Array;
      Last   : out Stream_Element_Offset)
   is
      Code : TSSL.ssize_t;
   begin
      if not Socket.IO.Handshaken.all then
         Do_Handshake_Internal (Socket);
      end if;

      loop
         Code :=
           TSSL.gnutls_record_send (Socket.SSL, Data'Address, Data'Length);

         if Code >= 0 then
            Last := Last_Index (Data'First, Natural (Code));
            exit;
         end if;

         case Code is
            when TSSL.GNUTLS_E_INTERRUPTED =>
               case TSSL.gnutls_record_get_direction (Socket.SSL) is
               when 0 =>
                  if Socket.Pending = 0 then
                     Last := Last_Index (Data'First, 0);
                     exit;
                  end if;

               when 1 =>
                  if not Socket.Check ((Output => True, Input => False))
                    (Output)
                  then
                     Last := Last_Index (Data'First, 0);
                     exit;
                  end if;
               when others =>
                  raise Program_Error;
               end case;

            when TSSL.GNUTLS_E_AGAIN =>
               Last := Last_Index (Data'First, 0);
               exit;

            when others =>
               Check_Error_Code (Code, Socket);
         end case;
      end loop;
   end Send;

   --------------------
   -- Session_Client --
   --------------------

   procedure Session_Client (Socket : in out Socket_Type) is
      use TSSL;
      Session : aliased gnutls_session_t;
   begin
      Check_Config (Socket);

      Check_Error_Code (gnutls_init (Session'Access, GNUTLS_CLIENT), Socket);

      Socket.SSL := Session;

      Check_Error_Code (gnutls_set_default_priority (Session), Socket);

      Check_Error_Code
        (gnutls_credentials_set (Session, cred => Socket.Config.ACC), Socket);

      if Socket.Config.CCC /= null then
         Check_Error_Code
           (gnutls_credentials_set (Session, cred => Socket.Config.CCC),
            Socket);
      end if;

      Session_Transport (Socket);
   end Session_Client;

   --------------------
   -- Session_Server --
   --------------------

   procedure Session_Server (Socket : in out Socket_Type) is
      use TSSL;
      Session : aliased gnutls_session_t;
      Setting : gnutls_certificate_request_t;
   begin
      Check_Config (Socket);

      Check_Error_Code (gnutls_init (Session'Access, GNUTLS_SERVER), Socket);

      Socket.SSL := Session;

      Check_Error_Code (gnutls_set_default_priority (Session), Socket);

      if Socket.Config.CSC = null then
         Check_Error_Code
           (gnutls_credentials_set (Session, cred => Socket.Config.ASC),
            Socket);

      else
         Check_Error_Code
           (gnutls_credentials_set (Session, cred => Socket.Config.CSC),
            Socket);

         if Socket.Config.RCC then
            if Socket.Config.CREQ then
               Setting := GNUTLS_CERT_REQUIRE;
            else
               Setting := GNUTLS_CERT_REQUEST;
            end if;

            gnutls_certificate_server_set_request (Session, Setting);

            if Socket.Config.CAfile /= C.Strings.Null_Ptr then
               TSSL.gnutls_certificate_send_x509_rdn_sequence (Session, 0);

               if TSSL.gnutls_certificate_set_x509_trust_file
                 (Socket.Config.CSC,
                  Socket.Config.CAfile,
                  TSSL.GNUTLS_X509_FMT_PEM) = -1
               then
                  raise Socket_Error with "cannot set CA file " & "...";
               end if;
            end if;

         else
            gnutls_certificate_server_set_request
              (Session, GNUTLS_CERT_IGNORE);
         end if;
      end if;

      Session_Transport (Socket);
   end Session_Server;

   -----------------------
   -- Session_Transport --
   -----------------------

   procedure Session_Transport (Socket : in out Socket_Type) is
   begin
      TSSL.gnutls_transport_set_ptr
        (Socket.SSL, TSSL.gnutls_transport_ptr_t (Socket.Get_FD));
      Socket.IO.Handshaken := new Boolean'(False);
   end Session_Transport;

   ----------------
   -- Set_Config --
   ----------------

   procedure Set_Config (Socket : in out Socket_Type; Config : SSL.Config) is
   begin
      Socket.Config := Config;
   end Set_Config;

   ----------------------------
   -- Set_Session_Cache_Size --
   ----------------------------

   procedure Set_Session_Cache_Size
     (Size : Natural; Config : SSL.Config := Null_Config) is
   begin
      null;
   end Set_Session_Cache_Size;

   -------------------------
   -- Set_Verify_Callback --
   -------------------------

   procedure Set_Verify_Callback
     (Config : in out SSL.Config; Callback : System.Address) is
   begin
      raise Program_Error with "verify callback not implemented on GNU/TLS.";
   end Set_Verify_Callback;

   --------------
   -- Shutdown --
   --------------

   overriding procedure Shutdown
     (Socket : Socket_Type; How : Shutmode_Type := Shut_Read_Write)
   is
      Code : C.int;
      To_C : constant array (Shutmode_Type) of TSSL.gnutls_close_request_t :=
               (Shut_Read_Write => TSSL.GNUTLS_SHUT_RDWR,
                Shut_Read       => TSSL.GNUTLS_SHUT_RDWR, -- Absend, use RDWR
                Shut_Write      => TSSL.GNUTLS_SHUT_WR);
   begin
      loop
         Code := TSSL.gnutls_bye (Socket.SSL, To_C (How));

         exit when Code = TSSL.GNUTLS_E_SUCCESS;

         begin
            Code_Processing
              (Code, Socket,
               Duration'Min (Net.Socket_Type (Socket).Timeout, 0.25));
         exception when E : others =>
            Net.Log.Error (Socket, Ada.Exceptions.Exception_Message (E));
            exit;
         end;
      end loop;

      TSSL.gnutls_transport_set_ptr (Socket.SSL, 0);

      Net.Std.Shutdown (NSST (Socket), How);
   end Shutdown;

   -----------------
   -- Socket_Pair --
   -----------------

   overriding procedure Socket_Pair (S1, S2 : out Socket_Type) is
      ST1, ST2 : Std.Socket_Type;
   begin
      Std.Socket_Pair (ST1, ST2);
      S1 := Secure_Server (ST1);
      S2 := Secure_Client (ST2);
   end Socket_Pair;

   ---------------------
   -- Verify_Callback --
   ---------------------

   function Verify_Callback (Session : TSSL.gnutls_session_t) return C.int is
      pragma Unreferenced (Session);
   begin
      return 0;
   end Verify_Callback;

   -------------
   -- Version --
   -------------

   function Version (Build_Info : Boolean := False) return String is
      use C.Strings;
      pragma Unreferenced (Build_Info);
   begin
      return "GNUTLS " & Value (TSSL.gnutls_check_version (Null_Ptr));
   end Version;

begin
   Check_Error_Code
     (TSSL.aws_gcry_set_thread_cbs
        (Thread_CBS => (Option        => TSSL.GCRY_THREAD_OPTION_USER,
                        Mutex_Init    => Locking.Init'Address,
                        Mutex_Destroy => Locking.Destroy'Address,
                        Mutex_Lock    => Locking.Lock'Address,
                        Mutex_Unlock  => Locking.Unlock'Address,
                        others        => <>)));

   if TSSL.gnutls_global_init /= 0 then
      raise Program_Error;
   end if;
end AWS.Net.SSL;
