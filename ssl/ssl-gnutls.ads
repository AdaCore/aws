--  $Id$

with Interfaces.C.Strings;
with System;

package SSL.GNUTLS is

   use Interfaces;
   package CS renames C.Strings;

   type GCry_Ctl_Cmds is new C.int;

   GCRYCTL_SET_KEY                   : constant GCry_Ctl_Cmds := 1;
   GCRYCTL_SET_IV                    : constant GCry_Ctl_Cmds := 2;
   GCRYCTL_CFB_SYNC                  : constant GCry_Ctl_Cmds := 3;
   GCRYCTL_RESET                     : constant GCry_Ctl_Cmds := 4;
   GCRYCTL_FINALIZE                  : constant GCry_Ctl_Cmds := 5;
   GCRYCTL_GET_KEYLEN                : constant GCry_Ctl_Cmds := 6;
   GCRYCTL_GET_BLKLEN                : constant GCry_Ctl_Cmds := 7;
   GCRYCTL_TEST_ALGO                 : constant GCry_Ctl_Cmds := 8;
   GCRYCTL_IS_SECURE                 : constant GCry_Ctl_Cmds := 9;
   GCRYCTL_GET_ASNOID                : constant GCry_Ctl_Cmds := 10;
   GCRYCTL_ENABLE_ALGO               : constant GCry_Ctl_Cmds := 11;
   GCRYCTL_DISABLE_ALGO              : constant GCry_Ctl_Cmds := 12;
   GCRYCTL_DUMP_RANDOM_STATS         : constant GCry_Ctl_Cmds := 13;
   GCRYCTL_DUMP_SECMEM_STATS         : constant GCry_Ctl_Cmds := 14;
   GCRYCTL_GET_ALGO_NPKEY            : constant GCry_Ctl_Cmds := 15;
   GCRYCTL_GET_ALGO_NSKEY            : constant GCry_Ctl_Cmds := 16;
   GCRYCTL_GET_ALGO_NSIGN            : constant GCry_Ctl_Cmds := 17;
   GCRYCTL_GET_ALGO_NENCR            : constant GCry_Ctl_Cmds := 18;
   GCRYCTL_SET_VERBOSITY             : constant GCry_Ctl_Cmds := 19;
   GCRYCTL_SET_DEBUG_FLAGS           : constant GCry_Ctl_Cmds := 20;
   GCRYCTL_CLEAR_DEBUG_FLAGS         : constant GCry_Ctl_Cmds := 21;
   GCRYCTL_USE_SECURE_RNDPOOL        : constant GCry_Ctl_Cmds := 22;
   GCRYCTL_DUMP_MEMORY_STATS         : constant GCry_Ctl_Cmds := 23;
   GCRYCTL_INIT_SECMEM               : constant GCry_Ctl_Cmds := 24;
   GCRYCTL_TERM_SECMEM               : constant GCry_Ctl_Cmds := 25;
   GCRYCTL_DISABLE_SECMEM_WARN       : constant GCry_Ctl_Cmds := 27;
   GCRYCTL_SUSPEND_SECMEM_WARN       : constant GCry_Ctl_Cmds := 28;
   GCRYCTL_RESUME_SECMEM_WARN        : constant GCry_Ctl_Cmds := 29;
   GCRYCTL_DROP_PRIVS                : constant GCry_Ctl_Cmds := 30;
   GCRYCTL_ENABLE_M_GUARD            : constant GCry_Ctl_Cmds := 31;
   GCRYCTL_START_DUMP                : constant GCry_Ctl_Cmds := 32;
   GCRYCTL_STOP_DUMP                 : constant GCry_Ctl_Cmds := 33;
   GCRYCTL_GET_ALGO_USAGE            : constant GCry_Ctl_Cmds := 34;
   GCRYCTL_IS_ALGO_ENABLED           : constant GCry_Ctl_Cmds := 35;
   GCRYCTL_DISABLE_INTERNAL_LOCKING  : constant GCry_Ctl_Cmds := 36;
   GCRYCTL_DISABLE_SECMEM            : constant GCry_Ctl_Cmds := 37;
   GCRYCTL_INITIALIZATION_FINISHED   : constant GCry_Ctl_Cmds := 38;
   GCRYCTL_INITIALIZATION_FINISHED_P : constant GCry_Ctl_Cmds := 39;
   GCRYCTL_ANY_INITIALIZATION_P      : constant GCry_Ctl_Cmds := 40;
   GCRYCTL_SET_CBC_CTS               : constant GCry_Ctl_Cmds := 41;
   GCRYCTL_SET_CBC_MAC               : constant GCry_Ctl_Cmds := 42;
   GCRYCTL_SET_CTR                   : constant GCry_Ctl_Cmds := 43;
   GCRYCTL_ENABLE_QUICK_RANDOM       : constant GCry_Ctl_Cmds := 44;
   GCRYCTL_SET_RANDOM_SEED_FILE      : constant GCry_Ctl_Cmds := 45;
   GCRYCTL_UPDATE_RANDOM_SEED_FILE   : constant GCry_Ctl_Cmds := 46;
   GCRYCTL_SET_THREAD_CBS            : constant GCry_Ctl_Cmds := 47;

   type GNUTLS_Connection_End is new C.int;

   GNUTLS_SERVER : constant GNUTLS_Connection_End := 1;
   GNUTLS_CLIENT : constant GNUTLS_Connection_End := 2;

   GNUTLS_KX_RSA        : constant := 1;
   GNUTLS_KX_DHE_DSS    : constant := 2;
   GNUTLS_KX_DHE_RSA    : constant := 3;
   GNUTLS_KX_ANON_DH    : constant := 4;
   GNUTLS_KX_SRP        : constant := 5;
   GNUTLS_KX_RSA_EXPORT : constant := 6;
   GNUTLS_KX_SRP_RSA    : constant := 7;
   GNUTLS_KX_SRP_DSS    : constant := 8;

   type GNUTLS_Credentials_Type is new C.int;

   GNUTLS_CRD_CERTIFICATE : constant GNUTLS_Credentials_Type := 1;
   GNUTLS_CRD_ANON        : constant GNUTLS_Credentials_Type := 2;
   GNUTLS_CRD_SRP         : constant GNUTLS_Credentials_Type := 3;

   type GNUTLS_Close_Request is new C.int;

   GNUTLS_SHUT_RDWR : constant GNUTLS_Close_Request := 0;
   GNUTLS_SHUT_WR   : constant GNUTLS_Close_Request := 1;

   type GNUTLS_x509_crt_fmt is new C.int;

   GNUTLS_X509_FMT_DER : constant GNUTLS_x509_crt_fmt := 0;
   GNUTLS_X509_FMT_PEM : constant GNUTLS_x509_crt_fmt := 1;

   subtype GNUTLS_Transport_Ptr is System.Address;

   subtype GPG_Error_T is C.unsigned;
   subtype GCry_Error_T is GPG_Error_T;

   type GCry_Thread_Cbs is record
      Option        : C.int := 1;     -- GCRY_THREAD_OPTION_USER
      Init          : System.Address; -- int (*init) (void);
      Mutex_Init    : System.Address; -- int (*mutex_init) (void **priv);
      Mutex_Destroy : System.Address; -- int (*mutex_destroy) (void **priv);
      Mutex_Lock    : System.Address; -- int (*mutex_lock) (void **priv);
      Mutex_Unlock  : System.Address; -- int (*mutex_unlock) (void **priv);
      Read          : System.Address := System.Null_Address;
      Write         : System.Address := System.Null_Address;
      Select_Socket : System.Address := System.Null_Address;
      WaitPid       : System.Address := System.Null_Address;
      Accept_Socket : System.Address := System.Null_Address;
      Connect       : System.Address := System.Null_Address;
      SendMsg       : System.Address := System.Null_Address;
      RecvMsg       : System.Address := System.Null_Address;
   end record;

   type GNUTLS_Anon_Client_Credentials is new System.Address;
   type GNUTLS_Certificate_Credentials is new System.Address;

   type GNUTLS_Session is new System.Address;
   type Int_Array is array (1 .. Integer'Last) of C.int;

   function GCry_Control
     (Cmd : in GCry_Ctl_Cmds; Cbs : in GCry_Thread_Cbs) return GCry_Error_T;
   pragma Import (C, GCry_Control, "gcry_control");

   procedure GNUTLS_Global_Init;
   pragma Import (C, GNUTLS_Global_Init, "gnutls_global_init");

   function GNUTLS_Anon_Allocate_Client_Credentials
     (SC : access GNUTLS_Anon_Client_Credentials) return C.int;
   pragma Import (C, GNUTLS_Anon_Allocate_Client_Credentials,
                    "gnutls_anon_allocate_client_credentials");

   function GNUTLS_Certificate_Allocate_Credentials
     (SC : access GNUTLS_Certificate_Credentials) return C.int;
   pragma Import (C, GNUTLS_Certificate_Allocate_Credentials,
                    "gnutls_certificate_allocate_credentials");

   function GNUTLS_Certificate_Set_X509_Trust_File
     (Res  : GNUTLS_Certificate_Credentials;
      File : CS.chars_ptr;
      Fmt  : GNUTLS_x509_crt_fmt) return C.int;
   pragma Import (C, GNUTLS_Certificate_Set_X509_Trust_File,
                    "gnutls_certificate_set_x509_trust_file");

   function GNUTLS_Init
     (Session : access GNUTLS_Session;
      Con_End : in     GNUTLS_Connection_End) return C.int;
   pragma Import (C, GNUTLS_Init, "gnutls_init");

   function GNUTLS_Set_Default_Priority
     (Session : in GNUTLS_Session) return C.int;
   pragma Import (C, GNUTLS_Set_Default_Priority,
                    "gnutls_set_default_priority");

   function GNUTLS_KX_Set_Priority
     (Session : in GNUTLS_Session;
      List    : in Int_Array) return C.int;
   pragma Import (C, GNUTLS_KX_Set_Priority, "gnutls_kx_set_priority");

   function GNUTLS_Certificate_Type_Set_Priority
     (Session : in GNUTLS_Session;
      List    : in Int_Array) return C.int;
   pragma Import (C, GNUTLS_Certificate_Type_Set_Priority,
                     "gnutls_certificate_type_set_priority");

   function GNUTLS_Credentials_Set
     (Session : in GNUTLS_Session;
      Kind    : in GNUTLS_Credentials_Type;
      Cred    : in System.Address) return C.int;
   pragma Import (C, GNUTLS_Credentials_Set, "gnutls_credentials_set");

   procedure GNUTLS_Transport_Set_Ptr
     (Session : in GNUTLS_Session;
      Ptr     : in GNUTLS_Transport_Ptr);
   pragma Import (C, GNUTLS_Transport_Set_Ptr, "gnutls_transport_set_ptr");

   procedure GNUTLS_Transport_Set_Push_Function
     (Session   : in GNUTLS_Session;
      Push_Func : in System.Address);
   pragma Import (C, GNUTLS_Transport_Set_Push_Function,
                    "gnutls_transport_set_push_function");

   procedure GNUTLS_Transport_Set_Pull_Function
     (Session   : in GNUTLS_Session;
      Pull_Func : in System.Address);
   pragma Import (C, GNUTLS_Transport_Set_Pull_Function,
                    "gnutls_transport_set_pull_function");

   function GNUTLS_Handshake (Session : in GNUTLS_Session) return C.int;
   pragma Import (C, GNUTLS_Handshake, "gnutls_handshake");

   function GNUTLS_StrError (Error : C.int) return CS.chars_ptr;
   pragma Import (C, GNUTLS_StrError, "gnutls_strerror");

   function GNUTLS_Record_Send
     (Session : in GNUTLS_Session;
      Data    : in System.Address;
      Size_Of : in C.size_t) return C.size_t;
   pragma Import (C, GNUTLS_Record_Send, "gnutls_record_send");

   function GNUTLS_Record_Recv
     (Session : in GNUTLS_Session;
      Data    : in System.Address;
      Size_Of : in C.size_t) return C.size_t;
   pragma Import (C, GNUTLS_Record_Recv, "gnutls_record_recv");

   function GNUTLS_Bye
     (Session : in GNUTLS_Session;
      How     : in GNUTLS_Close_Request) return C.int;
   pragma Import (C, GNUTLS_Bye, "gnutls_bye");

   procedure GNUTLS_Deinit (Session : in GNUTLS_Session);
   pragma Import (C, GNUTLS_Deinit, "gnutls_deinit");

   procedure GNUTLS_Anon_Free_Client_Credentials
     (SC : in GNUTLS_Anon_Client_Credentials);
   pragma Import (C, GNUTLS_Anon_Free_Client_Credentials,
                    "gnutls_anon_free_client_credentials");

   procedure GNUTLS_Certificate_Free_Credentials
     (SC : GNUTLS_Certificate_Credentials);
   pragma Import (C, GNUTLS_Certificate_Free_Credentials,
                    "gnutls_certificate_free_credentials");

   procedure GNUTLS_Global_Deinit;
   pragma Import (C, GNUTLS_Global_Deinit, "gnutls_global_deinit");

end SSL.GNUTLS;
