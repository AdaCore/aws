------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2006-2014, AdaCore                     --
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

with Ada.Calendar;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Ordered_Maps;
with Ada.Directories;
with Ada.Strings.Hash;
with Ada.Text_IO;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

with Interfaces.C.Strings;
with System.Memory;

with AWS.Config;
with AWS.Net.Log;
with AWS.Net.SSL.Certificate.Impl;
with AWS.Net.SSL.RSA_DH_Generators;
with AWS.OS_Lib;
with AWS.Resources;
with AWS.Translator;
with AWS.Utils;

package body AWS.Net.SSL is

   use Interfaces;

   use type C.int;
   use type C.unsigned;

   package CS renames C.Strings;

   subtype NSST is Net.Std.Socket_Type;

   Set_Certificate_Over_Callback : constant Boolean := False;
   --  ??? We have 2 variants to setup certificate now,
   --  over callback installed by gnutls_certificate_set_retrieve_function2
   --  and over gnutls_certificate_set_x509_key_mem, which one to use, we will
   --  decide later, but now we are able to test it both.

   type Datum_Type is record
      Datum : aliased TSSL.gnutls_datum_t;
      Data  : Utils.Stream_Element_Array_Access;
   end record;

   function Load_File (Filename : String) return Datum_Type;

   type PCert_Array is
     array (Positive range <>) of aliased TSSL.gnutls_pcert_st
     with Convention => C;

   DH_Params  : array (0 .. 1) of aliased TSSL.gnutls_dh_params_t
     with Atomic_Components;
   RSA_Params : array (0 .. 1) of aliased TSSL.gnutls_rsa_params_t
     with Atomic_Components;
   --  0 element for current use, 1 element for remain usage after creation new
   --  0 element.

   function Copy (Item : TSSL.gnutls_datum_t) return TSSL.gnutls_datum_t;
   --  Creates gnutls_datum_t copy

   function Lib_Alloc (Size : System.Memory.size_t) return System.Address
     with Convention => C;
   --  Set allocated data to zero to workaroung GNUTLS bug in
   --  gnutls_pcert_list_import_x509_raw when it parse file without
   --  certificate. Should be fixed in 3.3.1, 3.2.12, 3.1.22 GNUTLS versions.

   function Lib_Realloc
     (Ptr  : System.Address;
      Size : System.Memory.size_t) return System.Address
     with Convention => C;
   --  C library could use null pointer as input parameter for realloc, but
   --  gnatmem does not care about it and logging Free of the null pointer
   --  and than claiming for "Releasing deallocated memory".

   procedure Lib_Free (Ptr  : System.Address) with Convention => C;
   --  C library could put null pointer as input parameter for free, but
   --  gnatmem does not care about it and logging Free of the null pointer
   --  and than claiming for "Releasing deallocated memory".

   type PCert_Array_Access is access all PCert_Array;

   procedure Free (Datum : in out Datum_Type) with Inline;

   procedure Check_Error_Code (Code : C.int);
   procedure Check_Error_Code (Code : C.int; Socket : Socket_Type'Class);

   procedure Code_Processing
     (Code : C.int; Socket : Socket_Type'Class; Timeout : Duration);

   procedure Code_Processing (Code : C.int; Socket : Socket_Type'Class);

   procedure Check_Config (Socket : in out Socket_Type) with Inline;

   procedure Do_Handshake_Internal (Socket : Socket_Type) with Inline;
   --  The real handshake is done here

   function To_Config is new Unchecked_Conversion (System.Address, Config);

   function Write_Socket
     (S : C.int; Msg : System.Address; Len : C.int) return C.int
     with Convention => C;
   --  Would be used only on defined MSG_NOSIGNAL platforms to avoid SIGPIPE
   --  signal.

   function DB_Store
     (p1   : System.Address;
      key  : TSSL.gnutls_datum_t;
      data : TSSL.gnutls_datum_t) return C.int with Convention => C;

   function DB_Remove
     (p1 : System.Address; key : TSSL.gnutls_datum_t) return C.int
     with Convention => C;

   function DB_Retrieve
     (p1  : System.Address;
      key : TSSL.gnutls_datum_t) return TSSL.gnutls_datum_t
     with Convention => C;

   procedure SSL_Log (level : C.int; text : CS.chars_ptr)
     with Convention => C;

   procedure SSL_Log_Audit
     (sessn : TSSL.gnutls_session_t; level : C.int; text : CS.chars_ptr)
     with Convention => C;

   procedure SSL_Log_Common
     (Prefix : String; Level : C.int; text : CS.chars_ptr);

   function Equal (Left, Right : TSSL.gnutls_datum_t) return Boolean;

   function Hash (Item : TSSL.gnutls_datum_t) return Ada.Containers.Hash_Type;

   type Session_Element is record
      Datum : TSSL.gnutls_datum_t;
      Birth : Ada.Calendar.Time;
   end record;

   package Session_Container is
     new Ada.Containers.Hashed_Maps
           (Key_Type        => TSSL.gnutls_datum_t,
            Element_Type    => Session_Element,
            Hash            => Hash,
            Equivalent_Keys => Equal);

   package Time_Set is
     new Ada.Containers.Ordered_Maps
           (Key_Type     => Ada.Calendar.Time,
            Element_Type => TSSL.gnutls_datum_t,
            "<"          => Ada.Calendar."<",
            "="          => Equal);

   protected type Session_Cache is

      procedure Set_Size (Size : Natural);
      --  Set the maximum cache size

      function Length return Natural;
      --  Returns number of sessions currently in cache

      procedure Put (Key, Data : TSSL.gnutls_datum_t);

      function Get (Key : TSSL.gnutls_datum_t) return TSSL.gnutls_datum_t;

      procedure Drop (Key : TSSL.gnutls_datum_t);

      procedure Clear;

   private
      Size : Natural := Natural'Last;
      Map  : Session_Container.Map;
      DTI  : Time_Set.Map; --  To remove oldest sessions to fit Size
   end Session_Cache;

   type TS_SSL is record
      ASC            : aliased TSSL.gnutls_anon_server_credentials_t;
      ACC            : aliased TSSL.gnutls_anon_client_credentials_t;
      CSC            : aliased TSSL.gnutls_certificate_credentials_t;
      CCC            : aliased TSSL.gnutls_certificate_credentials_t;
      PCert_List     : PCert_Array_Access;
      TLS_PK         : aliased TSSL.gnutls_privkey_t;
      Priority_Cache : aliased TSSL.gnutls_priority_t;
      Ticket_Support : Boolean;
      Ticket_Key     : aliased TSSL.gnutls_datum_t := (System.Null_Address, 0);
      Sessions       : Session_Cache;
      RCC            : Boolean := False; -- Request client certificate
      CREQ           : Boolean := False; -- Certificate is required
      Verify_CB      : Net.SSL.Certificate.Verify_Callback;
      CRL_File       : C.Strings.chars_ptr := C.Strings.Null_Ptr;
      CRL_Semaphore  : Utils.Semaphore;
      CRL_Time_Stamp : Calendar.Time := Utils.AWS_Epoch;
   end record;

   procedure Initialize
     (Config               : in out TS_SSL;
      Certificate_Filename : String;
      Security_Mode        : Method;
      Priorities           : String;
      Ticket_Support       : Boolean;
      Key_Filename         : String;
      Exchange_Certificate : Boolean;
      Certificate_Required : Boolean;
      Trusted_CA_Filename  : String;
      CRL_Filename         : String;
      Session_Cache_Size   : Positive);

   procedure Session_Client (Socket : in out Socket_Type);
   procedure Session_Server (Socket : in out Socket_Type);
   --  Bind the SSL socket handle with the socket

   procedure Session_Transport (Socket : in out Socket_Type);

   function Verify_Callback (Session : TSSL.gnutls_session_t) return C.int
     with Convention => C;

   procedure Finalize (Config : in out TS_SSL);

   Default_Config : aliased TS_SSL;

   protected Default_Config_Sync is

      procedure Create;
      --  Create default config with default parameters

      procedure Initialize
        (Certificate_Filename : String;
         Security_Mode        : Method;
         Priorities           : String;
         Ticket_Support       : Boolean;
         Key_Filename         : String;
         Exchange_Certificate : Boolean;
         Certificate_Required : Boolean;
         Trusted_CA_Filename  : String;
         CRL_Filename         : String;
         Session_Cache_Size   : Positive);

   private
      Done : Boolean := False;
   end Default_Config_Sync;

   procedure Initialize_Default_Config;
   --  Initializes default config. It could be called more then once, because
   --  secondary initialization is ignored.

   procedure Secure
     (Source : Net.Socket_Type'Class;
      Target : out Socket_Type;
      Config : SSL.Config);
   --  Make Target a secure socket for Source using the given configuration

   function Retrieve_Certificate
     (Session         : TSSL.gnutls_session_t;
      req_ca_rdn      : access constant TSSL.gnutls_datum_t;
      nreqs           : C.int;
      pk_algos        : access constant TSSL.gnutls_pk_algorithm_t;
      pk_algos_length : C.int;
      pcert           : access TSSL.a_gnutls_pcert_st;
      pcert_length    : access C.unsigned;
      privkey         : access TSSL.gnutls_privkey_t) return C.int
      with Convention => C;

   function Params_Callback
     (Sessn  : TSSL.gnutls_session_t;
      Kind   : TSSL.gnutls_params_type_t;
      Params : access TSSL.gnutls_params_st) return C.int
     with Convention => C;
   --  Callback to give Diffie-Hellman and/or RSA parameters

   -------------------------
   -- Abort_DH_Generation --
   -------------------------

   procedure Abort_DH_Generation is
   begin
      Abort_DH_Flag := True;
   end Abort_DH_Generation;

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
         Raise_Socket_Error
           (Socket, C.Strings.Value (TSSL.gnutls_strerror (Code)));
      end if;
   end Check_Error_Code;

   procedure Check_Error_Code (Code : C.int) is
   begin
      if Code /= 0 then
         Raise_Socket_Error
           (Socket_Type'(Std.Socket_Type with others => <>),
            C.Strings.Value (TSSL.gnutls_strerror (Code)));
      end if;
   end Check_Error_Code;

   ------------------------
   -- Cipher_Description --
   ------------------------

   overriding function Cipher_Description (Socket : Socket_Type) return String
   is
      use TSSL;
   begin
      return CS.Value (gnutls_cipher_get_name (gnutls_cipher_get (Socket.SSL)))
        & ' ' & CS.Value
                  (gnutls_protocol_get_name
                     (gnutls_protocol_get_version (Socket.SSL)))
        & ' ' & CS.Value (gnutls_kx_get_name (gnutls_kx_get (Socket.SSL)))
        & ' ' & CS.Value (gnutls_mac_get_name (gnutls_mac_get (Socket.SSL)));
   end Cipher_Description;

   -------------
   -- Ciphers --
   -------------

   procedure Ciphers (Cipher : access procedure (Name : String)) is
      use type CS.chars_ptr;
      Name    : CS.chars_ptr;
      cs_id   : array (0 .. 1) of aliased C.unsigned_char;
      kx      : aliased TSSL.gnutls_kx_algorithm_t;
      ciph    : aliased TSSL.gnutls_cipher_algorithm_t;
      mac     : aliased TSSL.gnutls_mac_algorithm_t;
      min_ver : aliased TSSL.gnutls_protocol_t;
   begin
      for J in 0 .. C.size_t'Last loop
         Name := TSSL.gnutls_cipher_suite_info
                   (J, cs_id (0)'Access, kx'Access, ciph'Access, mac'Access,
                    min_ver'Access);

         exit when Name = CS.Null_Ptr;

         Cipher (Utils.Hex (C.unsigned_char'Pos (cs_id (0)), 2)
            & ' ' & Utils.Hex (C.unsigned_char'Pos (cs_id (1)), 2)
            & ' ' & CS.Value (TSSL.gnutls_protocol_get_name (min_ver))
            & ' ' & CS.Value (TSSL.gnutls_kx_get_name (kx))
            & ' ' & CS.Value (TSSL.gnutls_cipher_get_name (ciph))
            & ' ' & CS.Value (TSSL.gnutls_mac_get_name (mac)));

         --  Could put CS.Value (Name) too, but it would duplicate information
         --  above in different format.
      end loop;
   end Ciphers;

   -------------------------
   -- Clear_Session_Cache --
   -------------------------

   procedure Clear_Session_Cache (Config : SSL.Config := Null_Config) is
   begin
      if Config = Null_Config then
         Default_Config.Sessions.Clear;
      else
         Config.Sessions.Clear;
      end if;
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

   ----------
   -- Copy --
   ----------

   function Copy (Item : TSSL.gnutls_datum_t) return TSSL.gnutls_datum_t is
      Result : TSSL.gnutls_datum_t;

      type Array_Access is access all Stream_Element_Array
                                        (1 .. Stream_Element_Offset
                                                (Item.size));
      function To_Array is
        new Ada.Unchecked_Conversion (TSSL.a_unsigned_char_t, Array_Access);

   begin
      if Item.size = 0 then
         return Item;
      end if;

      Result.data := TSSL.gnutls_malloc (C.size_t (Item.size));
      Result.size := Item.size;
      To_Array (Result.data).all := To_Array (Item.data).all;

      return Result;
   end Copy;

   ---------------
   -- DB_Remove --
   ---------------

   function DB_Remove
     (p1 : System.Address; key : TSSL.gnutls_datum_t) return C.int
   is
      Cfg : constant Config := To_Config (p1);
   begin
      Cfg.Sessions.Drop (key);
      return 0;
   exception
      when E : others =>
         Log_Error (Ada.Exceptions.Exception_Information (E));
         return 1;
   end DB_Remove;

   -----------------
   -- DB_Retrieve --
   -----------------

   function DB_Retrieve
     (p1  : System.Address;
      key : TSSL.gnutls_datum_t) return TSSL.gnutls_datum_t
   is
      Cfg : constant Config := To_Config (p1);
   begin
      return Copy (Cfg.Sessions.Get (key));
   exception
      when E : others =>
         Log_Error (Ada.Exceptions.Exception_Information (E));
         return (System.Null_Address, 0);
   end DB_Retrieve;

   --------------
   -- DB_Store --
   --------------

   function DB_Store
     (p1   : System.Address;
      key  : TSSL.gnutls_datum_t;
      data : TSSL.gnutls_datum_t) return C.int
   is
      Cfg : constant Config := To_Config (p1);
   begin
      Cfg.Sessions.Put (key, data);

      return 0;
   exception
      when E : others =>
         Log_Error (Ada.Exceptions.Exception_Information (E));
         return 1;
   end DB_Store;

   -------------------------
   -- Default_Config_Sync --
   -------------------------

   protected body Default_Config_Sync is

      ------------
      -- Create --
      ------------

      procedure Create is
         package CNF renames AWS.Config;
         Default : CNF.Object renames CNF.Default_Config;
      begin
         if not Done then
            Initialize
              (Config               => Default_Config,
               Certificate_Filename => CNF.Certificate (Default),
               Security_Mode        => Method'Value
                                         (CNF.Security_Mode (Default)),
               Priorities           => CNF.Cipher_Priorities (Default),
               Ticket_Support       => CNF.TLS_Ticket_Support (Default),
               Key_Filename         => CNF.Key (Default),
               Exchange_Certificate => CNF.Exchange_Certificate (Default),
               Certificate_Required => CNF.Certificate_Required (Default),
               Trusted_CA_Filename  => CNF.Trusted_CA (Default),
               CRL_Filename         => CNF.CRL_File (Default),
               Session_Cache_Size   => 16#4000#);
            Done := True;
         end if;
      end Create;

      ----------------
      -- Initialize --
      ----------------

      procedure Initialize
        (Certificate_Filename : String;
         Security_Mode        : Method;
         Priorities           : String;
         Ticket_Support       : Boolean;
         Key_Filename         : String;
         Exchange_Certificate : Boolean;
         Certificate_Required : Boolean;
         Trusted_CA_Filename  : String;
         CRL_Filename         : String;
         Session_Cache_Size   : Positive) is
      begin
         if not Done then
            Initialize
              (Default_Config,
               Certificate_Filename,  Security_Mode, Priorities,
               Ticket_Support, Key_Filename, Exchange_Certificate,
               Certificate_Required, Trusted_CA_Filename, CRL_Filename,
               Session_Cache_Size);
            Done := True;
         end if;
      end Initialize;

   end Default_Config_Sync;

   ------------------
   -- Do_Handshake --
   ------------------

   procedure Do_Handshake (Socket : in out Socket_Type) is
   begin
      Do_Handshake_Internal (Socket);
   end Do_Handshake;

   ---------------------------
   -- Do_Handshake_Internal --
   ---------------------------

   procedure Do_Handshake_Internal (Socket : Socket_Type) is
      Code : C.int;
   begin
      if Socket.IO.Handshaken.all then
         return;
      end if;

      loop
         Code := TSSL.gnutls_handshake (Socket.SSL);

         exit when Code = TSSL.GNUTLS_E_SUCCESS;

         Code_Processing (Code, Socket);
      end loop;

      Socket.IO.Handshaken.all := True;
   end Do_Handshake_Internal;

   -----------
   -- Equal --
   -----------

   function Equal (Left, Right : TSSL.gnutls_datum_t) return Boolean is
      use Ada.Streams;
      type Array_Access is access all Stream_Element_Array
                                        (1 .. Stream_Element_Offset'Last);
      function To_Array is
        new Ada.Unchecked_Conversion (TSSL.a_unsigned_char_t, Array_Access);
   begin
      return Left.size = Right.size
             and then To_Array (Left.data)
                        (1 .. Stream_Element_Offset (Left.size))
                    = To_Array (Right.data)
                        (1 .. Stream_Element_Offset (Right.size));
   end Equal;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Config : in out TS_SSL) is
      use type TSSL.gnutls_anon_client_credentials_t;
      use type TSSL.gnutls_anon_server_credentials_t;
      use type TSSL.gnutls_certificate_credentials_t;
      use type TSSL.gnutls_priority_t;
      use type TSSL.gnutls_privkey_t;

      procedure Unchecked_Free is
        new Ada.Unchecked_Deallocation (PCert_Array, PCert_Array_Access);

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

      if Config.CCC /= null then
         TSSL.gnutls_certificate_free_credentials (Config.CCC);
         Config.CCC := null;
      end if;

      if Config.TLS_PK /= null then
         TSSL.gnutls_privkey_deinit (Config.TLS_PK);
         Config.TLS_PK := null;
      end if;

      if Config.PCert_List /= null then
         Unchecked_Free (Config.PCert_List);
      end if;

      if Config.Priority_Cache /= null then
         TSSL.gnutls_priority_deinit (Config.Priority_Cache);
         Config.Priority_Cache := null;
      end if;

      TSSL.gnutls_free (Config.Ticket_Key.data);
      Config.Ticket_Key.data := System.Null_Address;

      Config.Sessions.Clear;
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

   procedure Free (Datum : in out Datum_Type) is
   begin
      Utils.Unchecked_Free (Datum.Data);
   end Free;

   procedure Free (Session : in out Session_Type) is
      procedure Unchecked_Free is
        new Ada.Unchecked_Deallocation (TSSL.Session_Record, Session_Type);
   begin
      if Session = null then
         return;
      end if;

      TSSL.gnutls_free (Session.Data.data);
      Unchecked_Free (Session);
   end Free;

   procedure Free (Key : in out Private_Key) is
   begin
      if Key /= null then
         TSSL.gnutls_privkey_deinit (TSSL.Private_Key (Key));
         Key := null;
      end if;
   end Free;

   -----------------
   -- Generate_DH --
   -----------------

   procedure Generate_DH is
      use type TSSL.gnutls_dh_params_t;

      New_One : aliased TSSL.gnutls_dh_params_t;
      OK      : Boolean;
      Bits    : C.unsigned;

      function Loaded return Boolean;

      procedure Save;

      ------------
      -- Loaded --
      ------------

      function Loaded return Boolean is
         Filename : constant String :=
                      RSA_DH_Generators.Parameters_Filename
                        ("dh-" & Utils.Image (Integer (Bits)), Exist => True);
         Datum    : Datum_Type;
      begin
         if Filename = "" then
            return False;
         end if;

         Datum := Load_File (Filename);

         Check_Error_Code
           (TSSL.gnutls_dh_params_import_pkcs3
              (New_One, Datum.Datum'Unchecked_Access,
               TSSL.GNUTLS_X509_FMT_PEM));

         Free (Datum);

         DH_Time (DH_Time_Idx + 1) := Resources.File_Timestamp (Filename);
         DH_Time_Idx := DH_Time_Idx + 1;

         return True;
      end Loaded;

      ----------
      -- Save --
      ----------

      procedure Save is
         Filename : constant String :=
                      RSA_DH_Generators.Parameters_Filename
                        ("dh-" & Utils.Image (Integer (Bits)), Exist => False);
         Data     : String (1 .. 4096);
         Last     : aliased C.size_t := Data'Length;
         File     : Text_IO.File_Type;
      begin
         if Filename = "" then
            return;
         end if;

         Check_Error_Code
           (TSSL.gnutls_dh_params_export_pkcs3
              (New_One, TSSL.GNUTLS_X509_FMT_PEM, Data'Address,
               Last'Unchecked_Access));

         Text_IO.Create
           (File, Text_IO.Out_File, Filename, Form => "shared=no");

         Text_IO.Put (File, Data (1 .. Natural (Last)));

         Text_IO.Close (File);
      end Save;

   begin
      DH_Lock.Try_Lock (OK);

      if not OK then
         --  Already in concurrent generation
         return;
      end if;

      Check_Error_Code (TSSL.gnutls_dh_params_init (New_One'Access));

      Bits := TSSL.gnutls_sec_param_to_pk_bits
                (TSSL.GNUTLS_PK_DH, TSSL.GNUTLS_SEC_PARAM_NORMAL);

      if DH_Params (0) /= null or else not Loaded then
         Check_Error_Code (TSSL.gnutls_dh_params_generate2 (New_One, Bits));
         DH_Time (DH_Time_Idx + 1) := Ada.Calendar.Clock;
         DH_Time_Idx := DH_Time_Idx + 1;
         Save;
      end if;

      TSSL.gnutls_dh_params_deinit (DH_Params (1));

      DH_Params (1) := DH_Params (0);
      DH_Params (0) := New_One;

      DH_Lock.Unlock;
   end Generate_DH;

   ------------------
   -- Generate_RSA --
   ------------------

   procedure Generate_RSA is
      use type TSSL.gnutls_rsa_params_t;

      New_One : aliased TSSL.gnutls_rsa_params_t;
      OK      : Boolean;
   begin
      RSA_Lock.Try_Lock (OK);

      if not OK then
         --  Already in concurrent generation
         return;
      end if;

      Check_Error_Code (TSSL.gnutls_rsa_params_init (New_One'Access));
      Check_Error_Code
        (TSSL.gnutls_rsa_params_generate2
           (New_One,
            TSSL.gnutls_sec_param_to_pk_bits
              (TSSL.GNUTLS_PK_RSA, TSSL.GNUTLS_SEC_PARAM_NORMAL)));

      TSSL.gnutls_rsa_params_deinit (RSA_Params (1));

      RSA_Params (1) := RSA_Params (0);
      RSA_Params (0) := New_One;

      RSA_Time (RSA_Time_Idx + 1) := Ada.Calendar.Clock;
      RSA_Time_Idx := RSA_Time_Idx + 1;

      RSA_Lock.Unlock;
   end Generate_RSA;

   ----------
   -- Hash --
   ----------

   function Hash (Item : TSSL.gnutls_datum_t) return Ada.Containers.Hash_Type
   is
      type String_Access is access all String (Positive);
      function To_Access is new
        Ada.Unchecked_Conversion (TSSL.a_unsigned_char_t, String_Access);
   begin
      return Ada.Strings.Hash
               (To_Access (Item.data) (1 .. Natural (Item.size)));
   end Hash;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Config               : in out SSL.Config;
      Certificate_Filename : String;
      Security_Mode        : Method     := SSLv23;
      Priorities           : String     := "";
      Ticket_Support       : Boolean    := False;
      Key_Filename         : String     := "";
      Exchange_Certificate : Boolean    := False;
      Certificate_Required : Boolean    := False;
      Trusted_CA_Filename  : String     := "";
      CRL_Filename         : String     := "";
      Session_Cache_Size   : Positive   := 16#4000#) is
   begin
      if Config = null then
         Config := new TS_SSL;
      end if;

      Initialize
        (Config.all,
         Certificate_Filename => Certificate_Filename,
         Security_Mode        => Security_Mode,
         Priorities           => Priorities,
         Ticket_Support       => Ticket_Support,
         Key_Filename         => Key_Filename,
         Exchange_Certificate => Exchange_Certificate,
         Certificate_Required => Certificate_Required,
         Trusted_CA_Filename  => Trusted_CA_Filename,
         CRL_Filename         => CRL_Filename,
         Session_Cache_Size   => Session_Cache_Size);
   end Initialize;

   procedure Initialize
     (Config               : in out TS_SSL;
      Certificate_Filename : String;
      Security_Mode        : Method;
      Priorities           : String;
      Ticket_Support       : Boolean;
      Key_Filename         : String;
      Exchange_Certificate : Boolean;
      Certificate_Required : Boolean;
      Trusted_CA_Filename  : String;
      CRL_Filename         : String;
      Session_Cache_Size   : Positive)
   is
      use type TSSL.gnutls_anon_client_credentials_t;
      use type TSSL.gnutls_anon_server_credentials_t;
      use type TSSL.gnutls_certificate_credentials_t;
      use type TSSL.gnutls_dh_params_t;

      Cert     : aliased Datum_Type;
      Key      : aliased Datum_Type;
      Trust_CA : aliased Datum_Type;

      procedure Set_Certificate (CC : TSSL.gnutls_certificate_credentials_t);
      --  Set credentials from Cetificate_Filename and Key_Filename

      procedure Check_File (Prefix, Filename : String);
      --  Check that Filename is present, raise an exception adding
      --  Prefix in front of the message.

      procedure Final;

      Drop : Utils.Finalizer (Final'Access) with Unreferenced;

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

      -----------
      -- Final --
      -----------

      procedure Final is
      begin
         Free (Cert);
         Free (Trust_CA);

         if Key_Filename /= "" then
            Free (Key);
         end if;
      end Final;

      ---------------------
      -- Set_Certificate --
      ---------------------

      procedure Set_Certificate (CC : TSSL.gnutls_certificate_credentials_t) is

         function Load_PCert_List (Try_Size : Positive) return PCert_Array;

         Code : C.int;

         ---------------------
         -- Load_PCert_List --
         ---------------------

         function Load_PCert_List (Try_Size : Positive) return PCert_Array is
            Result : PCert_Array (1 .. Try_Size);
            Size   : aliased C.unsigned := C.unsigned (Try_Size);

            RC : constant C.int :=
                   TSSL.gnutls_pcert_list_import_x509_raw
                     (Result (1)'Access,
                      Size'Access,
                      Cert.Datum'Unchecked_Access,
                      TSSL.GNUTLS_X509_FMT_PEM,
                      TSSL.GNUTLS_X509_CRT_LIST_IMPORT_FAIL_IF_EXCEED);
         begin
            if RC = TSSL.GNUTLS_E_SHORT_MEMORY_BUFFER then
               return Load_PCert_List (Positive (Size));
            else
               Check_Error_Code (RC);
            end if;

            return Result (1 .. Positive (Size));
         end Load_PCert_List;

      begin
         if Set_Certificate_Over_Callback then
            Config.PCert_List := new PCert_Array'(Load_PCert_List (4));

            Check_Error_Code (TSSL.gnutls_privkey_init (Config.TLS_PK'Access));
            Check_Error_Code
              (TSSL.gnutls_privkey_import_x509_raw
                 (Config.TLS_PK, Key.Datum'Unchecked_Access,
                  TSSL.GNUTLS_X509_FMT_PEM, CS.Null_Ptr, 0));

            TSSL.gnutls_certificate_set_retrieve_function2
              (CC, Retrieve_Certificate'Access);

         else
            Code := TSSL.gnutls_certificate_set_x509_key_mem
                      (CC,
                       Cert.Datum'Unchecked_Access,
                       Key.Datum'Unchecked_Access,
                       TSSL.GNUTLS_X509_FMT_PEM);

            if Code = TSSL.GNUTLS_E_BASE64_DECODING_ERROR then
               raise Socket_Error with "Certificate/Key file error.";
            else
               Check_Error_Code (Code);
            end if;
         end if;

         if Trusted_CA_Filename /= "" then
            if TSSL.gnutls_certificate_set_x509_trust_mem
                 (CC, Trust_CA.Datum'Unchecked_Access,
                  TSSL.GNUTLS_X509_FMT_PEM) = -1
            then
               raise Socket_Error
                 with "cannot set CA file " & Trusted_CA_Filename;
            end if;
         end if;
      end Set_Certificate;

   begin -- Initialize
      Config.Sessions.Set_Size (Session_Cache_Size);
      Config.Ticket_Support := Ticket_Support;

      if Certificate_Filename /= "" then
         Check_File ("Certificate", Certificate_Filename);

         Cert := Load_File (Certificate_Filename);

         if Key_Filename = "" then
            Key := Cert;
         else
            Check_File ("Key", Key_Filename);
            Key := Load_File (Key_Filename);
         end if;
      end if;

      if Trusted_CA_Filename /= "" then
         Check_File ("CA", Trusted_CA_Filename);
         Trust_CA := Load_File (Trusted_CA_Filename);
      end if;

      if (Security_Mode = SSLv23
          or else Security_Mode = TLSv1
          or else Security_Mode = SSLv3
          or else Security_Mode = SSLv23_Server
          or else Security_Mode = TLSv1_Server
          or else Security_Mode = SSLv3_Server)
        and then Config.ASC = null
        and then Config.CSC = null
      then
         Config.RCC := Exchange_Certificate;
         Config.CREQ := Certificate_Required;

         if Certificate_Filename = "" then
            Check_Error_Code
              (TSSL.gnutls_anon_allocate_server_credentials
                 (Config.ASC'Access));
            TSSL.gnutls_anon_set_params_function
              (Config.ASC, Params_Callback'Access);

         else
            Check_Error_Code
              (TSSL.gnutls_certificate_allocate_credentials
                 (Config.CSC'Access));

            Set_Certificate (Config.CSC);

            TSSL.gnutls_certificate_set_verify_function
              (cred => Config.CSC, func => Verify_Callback'Access);

            TSSL.gnutls_certificate_set_params_function
              (Config.CSC, Params_Callback'Access);
         end if;

         if Ticket_Support then
            Check_Error_Code
              (TSSL.gnutls_session_ticket_key_generate
                 (Config.Ticket_Key'Access));
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

      if CRL_Filename /= "" then
         Config.CRL_File := C.Strings.New_String (CRL_Filename);
      end if;

      declare
         Pr : aliased C.char_array := C.To_C (Priorities);
         Pp : CS.chars_ptr;
         Er : aliased CS.chars_ptr;
         RC : C.int;
      begin
         if Priorities /= "" then
            Pp := CS.To_Chars_Ptr (Pr'Unchecked_Access);
         end if;

         RC := TSSL.gnutls_priority_init
                 (priority_cache => Config.Priority_Cache'Access,
                  priorities     => Pp,
                  err_pos        => Er'Access);

         if RC = TSSL.GNUTLS_E_INVALID_REQUEST then
            Log_Error ("Priority syntax error '" & CS.Value (Er) & ''');
         else
            Check_Error_Code (RC);
         end if;
      end;
   end Initialize;

   -------------------------------
   -- Initialize_Default_Config --
   -------------------------------

   procedure Initialize_Default_Config
     (Certificate_Filename : String;
      Security_Mode        : Method   := SSLv23;
      Priorities           : String   := "";
      Ticket_Support       : Boolean  := False;
      Key_Filename         : String   := "";
      Exchange_Certificate : Boolean  := False;
      Certificate_Required : Boolean  := False;
      Trusted_CA_Filename  : String   := "";
      CRL_Filename         : String   := "";
      Session_Cache_Size   : Positive := 16#4000#) is
   begin
      Default_Config_Sync.Initialize
        (Certificate_Filename, Security_Mode, Priorities, Ticket_Support,
         Key_Filename, Exchange_Certificate, Certificate_Required,
         Trusted_CA_Filename, CRL_Filename, Session_Cache_Size);
   end Initialize_Default_Config;

   procedure Initialize_Default_Config is
   begin
      Default_Config_Sync.Create;
   end Initialize_Default_Config;

   ---------------
   -- Lib_Alloc --
   ---------------

   function Lib_Alloc (Size : System.Memory.size_t) return System.Address is
      Result : constant System.Address := System.Memory.Alloc (Size);
      type Binary_Access is
        access all Stream_Element_Array (1 .. Stream_Element_Offset (Size));
      function To_Access is
        new Ada.Unchecked_Conversion (System.Address, Binary_Access);
   begin
      To_Access (Result).all := (others => 0);
      return Result;
   end Lib_Alloc;

   --------------
   -- Lib_Free --
   --------------

   procedure Lib_Free (Ptr  : System.Address) is
      use System;
   begin
      if Ptr /= Null_Address then
         Memory.Free (Ptr);
      end if;
   end Lib_Free;

   -----------------
   -- Lib_Realloc --
   -----------------

   function Lib_Realloc
     (Ptr  : System.Address;
      Size : System.Memory.size_t) return System.Address
   is
      use System;
   begin
      if Ptr = Null_Address then
         if Set_Certificate_Over_Callback then
            return Lib_Alloc (Size);
         else
            return Memory.Alloc (Size);
         end if;
      else
         return Memory.Realloc (Ptr, Size);
      end if;
   end Lib_Realloc;

   ----------
   -- Load --
   ----------

   function Load (Filename : String) return Private_Key is
      Data : Datum_Type := Load_File (Filename);
      Key  : aliased TSSL.Private_Key;
   begin
      Check_Error_Code (TSSL.gnutls_privkey_init (Key'Access));
      Check_Error_Code
        (TSSL.gnutls_privkey_import_x509_raw
           (Key, Data.Datum'Unchecked_Access, TSSL.GNUTLS_X509_FMT_PEM,
            CS.Null_Ptr, 0));

      Free (Data);

      return Private_Key (Key);
   end Load;

   ---------------
   -- Load_File --
   ---------------

   function Load_File (Filename : String) return Datum_Type is
      use AWS.Resources;

      Result : Datum_Type;
      Last   : Stream_Element_Offset;
      File   : File_Type;
   begin
      Open (File, Name => Filename);

      Result.Data := new Stream_Element_Array
                           (1 .. Stream_Element_Offset (File_Size (Filename)));

      Read (File, Result.Data.all, Last);

      if not End_Of_File (File) then
         Close (File);
         raise Program_Error with "not end of file";
      end if;

      Close (File);

      if Last < Result.Data'Last then
         raise Program_Error with Last'Img & Result.Data'Last'Img;
      end if;

      Result.Datum.size := Result.Data'Length;
      Result.Datum.data := Result.Data.all'Address;

      return Result;
   end Load_File;

   ---------------
   -- Log_Error --
   ---------------

   procedure Log_Error (Text : String) is
   begin
      Log.Error (Socket_Type'(Std.Socket_Type with others => <>), Text);
   end Log_Error;

   ---------------------
   -- Params_Callback --
   ---------------------

   function Params_Callback
     (Sessn  : TSSL.gnutls_session_t;
      Kind   : TSSL.gnutls_params_type_t;
      Params : access TSSL.gnutls_params_st) return C.int
   is
      pragma Unreferenced (Sessn);
   begin
      case Kind is
         when TSSL.GNUTLS_PARAMS_RSA_EXPORT =>
            Params.params.rsa_export := RSA_Params (0);

         when TSSL.GNUTLS_PARAMS_DH =>
            Params.params.dh := DH_Params (0);

         when others =>
            return -1;
      end case;

      Params.kind   := Kind;
      Params.deinit := 0;

      return 0;
   end Params_Callback;

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
      Code  : TSSL.ssize_t;
      First : Stream_Element_Offset := Data'First;
   begin
      Do_Handshake_Internal (Socket);

      loop
         Code := TSSL.gnutls_record_recv
                   (Socket.SSL, Data (First)'Address,
                    C.size_t (Data'Length - First + Data'First));

         if Code > 0 then
            First := First + Stream_Element_Offset (Code);
            Last  := First - 1;

            exit when Last = Data'Last;

         else
            exit when First > Data'First and then NSST (Socket).Pending = 0;

            if Code = 0 then
               raise Socket_Error with Peer_Closed_Message;
            end if;

            Code_Processing (Code, Socket);
         end if;
      end loop;
   end Receive;

   -------------
   -- Release --
   -------------

   procedure Release (Config : in out SSL.Config) is
      procedure Unchecked_Free is
        new Ada.Unchecked_Deallocation (TS_SSL, SSL.Config);
   begin
      if Config /= null and then Config /= Default_Config'Access then
         Finalize (Config.all);
         Unchecked_Free (Config);
      end if;
   end Release;

   --------------------------
   -- Retrieve_Certificate --
   --------------------------

   function Retrieve_Certificate
     (Session         : TSSL.gnutls_session_t;
      req_ca_rdn      : access constant TSSL.gnutls_datum_t;
      nreqs           : C.int;
      pk_algos        : access constant TSSL.gnutls_pk_algorithm_t;
      pk_algos_length : C.int;
      pcert           : access TSSL.a_gnutls_pcert_st;
      pcert_length    : access C.unsigned;
      privkey         : access TSSL.gnutls_privkey_t) return C.int
   is
      pragma Unreferenced (req_ca_rdn, nreqs, pk_algos, pk_algos_length);

      Cfg : constant Config :=
              To_Config (TSSL.gnutls_session_get_ptr (Session));
   begin
      pcert.all        := Cfg.PCert_List (Cfg.PCert_List'First)'Access;
      pcert_length.all := Cfg.PCert_List'Length;
      privkey.all      := Cfg.TLS_PK;

      return 0;
   end Retrieve_Certificate;

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
      Do_Handshake_Internal (Socket);

      if Data'Length = 0 then
         Last := Last_Index (Data'First, 0);
         return;
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

   -------------------
   -- Session_Cache --
   -------------------

   protected body Session_Cache is

      -----------
      -- Clear --
      -----------

      procedure Clear is
      begin
         for J in Map.Iterate loop
            TSSL.gnutls_free (Map (J).Datum.data);
         end loop;

         Map.Clear;

         for J in DTI.Iterate loop
            TSSL.gnutls_free (DTI (J).data);
         end loop;

         DTI.Clear;
      end Clear;

      ----------
      -- Drop --
      ----------

      procedure Drop (Key : TSSL.gnutls_datum_t) is
         Cs : Session_Container.Cursor := Map.Find (Key);
         Dd : constant TSSL.a_unsigned_char_t := Map (Cs).Datum.data;
         Kd : constant TSSL.a_unsigned_char_t :=
                Session_Container.Key (Cs).data;
         Dt : constant Ada.Calendar.Time := Map (Cs).Birth;
      begin
         Map.Delete (Cs);
         DTI.Delete (Dt);
         TSSL.gnutls_free (Kd);
         TSSL.gnutls_free (Dd);
      end Drop;

      ---------
      -- Get --
      ---------

      function Get (Key : TSSL.gnutls_datum_t) return TSSL.gnutls_datum_t is
         Ce : constant Session_Container.Cursor := Map.Find (Key);
      begin
         if Session_Container.Has_Element (Ce) then
            return Map (Key).Datum;
         else
            return (System.Null_Address, 0);
         end if;
      end Get;

      ------------
      -- Length --
      ------------

      function Length return Natural is
      begin
         return Natural (Map.Length);
      end Length;

      ---------
      -- Put --
      ---------

      procedure Put (Key, Data : TSSL.gnutls_datum_t) is
         use Ada.Calendar;
         E  : Session_Element;
         Ce : Time_Set.Cursor;
         Cm : Session_Container.Cursor := Map.Find (Key);
         OK : Boolean;
         K  : TSSL.gnutls_datum_t;
      begin
         if Session_Container.Has_Element (Cm) then
            K := Session_Container.Key (Cm);
            E := Map (Cm);

            Map.Delete (Cm);
            DTI.Delete (E.Birth);
            TSSL.gnutls_free (E.Datum.data);
         else
            K := Copy (Key);
         end if;

         E := (Copy (Data), Clock);

         for J in 1 .. 8 loop
            DTI.Insert (E.Birth, K, Ce, OK);
            exit when OK;
            E.Birth := E.Birth + Duration'Small;
         end loop;

         if not OK then
            raise Program_Error with "Time index entry creation";
         end if;

         Map.Insert (K, E);

         while Natural (Map.Length) > Size loop
            Drop (DTI.First_Element);
         end loop;
      end Put;

      --------------
      -- Set_Size --
      --------------

      procedure Set_Size (Size : Natural) is
      begin
         Session_Cache.Size := Size;
      end Set_Size;

   end Session_Cache;

   --------------------------
   -- Session_Cache_Number --
   --------------------------

   function Session_Cache_Number
     (Config : SSL.Config := Null_Config) return Natural
   is
      Cfg : constant SSL.Config :=
              (if Config = Null_Config then Default_Config'Access else Config);
   begin
      return Cfg.Sessions.Length;
   end Session_Cache_Number;

   --------------------
   -- Session_Client --
   --------------------

   procedure Session_Client (Socket : in out Socket_Type) is
      use TSSL;
   begin
      Check_Config (Socket);

      Check_Error_Code
        (gnutls_init (Socket.SSL'Access, GNUTLS_CLIENT), Socket);

      if Socket.Config.Ticket_Support then
         Check_Error_Code (gnutls_session_ticket_enable_client (Socket.SSL));
      end if;

      if Socket.Sessn /= null then
         Socket.Set_Session_Data (Socket.Sessn);
         Socket.Sessn := null;
      end if;

      Check_Error_Code
        (gnutls_credentials_set (Socket.SSL, cred => Socket.Config.ACC),
         Socket);

      if Socket.Config.CCC /= null then
         Check_Error_Code
           (gnutls_credentials_set (Socket.SSL, cred => Socket.Config.CCC),
            Socket);
      end if;

      Session_Transport (Socket);
   end Session_Client;

   ------------------
   -- Session_Data --
   ------------------

   function Session_Data (Socket : Socket_Type) return Session_Type is
      Result : Session_Type;
      Id  : aliased Stream_Element_Array (1 .. 64);
      Len : aliased C.size_t := Id'Length;
   begin
      Check_Error_Code
        (TSSL.gnutls_session_get_id
           (Socket.SSL, Id'Address, Len'Unchecked_Access));

      Result := new TSSL.Session_Record (Stream_Element_Count (Len));

      Result.Id := Id (1 .. Stream_Element_Count (Len));

      Check_Error_Code
        (TSSL.gnutls_session_get_data2 (Socket.SSL, Result.Data'Access));

      return Result;
   end Session_Data;

   ----------------------
   -- Session_Id_Image --
   ----------------------

   function Session_Id_Image (Session : Session_Type) return String is
   begin
      if Session = null then
         return "";
      else
         return Translator.Base64_Encode (Session.Id);
      end if;
   end Session_Id_Image;

   function Session_Id_Image (Socket : Socket_Type) return String is
      Id  : aliased Stream_Element_Array (1 .. 64);
      Len : aliased C.size_t := Id'Length;
   begin
      Check_Error_Code
        (TSSL.gnutls_session_get_id
           (Socket.SSL, Id'Address, Len'Unchecked_Access));

      return Translator.Base64_Encode (Id (1 .. Stream_Element_Offset (Len)));
   end Session_Id_Image;

   --------------------
   -- Session_Reused --
   --------------------

   function Session_Reused (Socket : Socket_Type) return Boolean is
   begin
      return TSSL.gnutls_session_is_resumed (Socket.SSL) /= 0;
   end Session_Reused;

   --------------------
   -- Session_Server --
   --------------------

   procedure Session_Server (Socket : in out Socket_Type) is
      use TSSL;
      use type C.Strings.chars_ptr;
      use type System.Address;
   begin
      Check_Config (Socket);

      if DH_Params (0) = null and then RSA_Params (0) = null
        and then not RSA_Lock.Locked and then not DH_Lock.Locked
      then
         Start_Parameters_Generation (DH => True);
      end if;

      Check_Error_Code
        (gnutls_init (Socket.SSL'Access, GNUTLS_SERVER), Socket);

      if Socket.Config.Ticket_Support then
         Check_Error_Code
           (gnutls_session_ticket_enable_server
              (Socket.SSL, Socket.Config.Ticket_Key'Access));
      end if;

      gnutls_db_set_ptr (Socket.SSL, Socket.Config.all'Address);
      gnutls_db_set_retrieve_function (Socket.SSL, DB_Retrieve'Access);
      gnutls_db_set_remove_function (Socket.SSL, DB_Remove'Access);
      gnutls_db_set_store_function (Socket.SSL, DB_Store'Access);

      if Socket.Config.CSC = null then
         Check_Error_Code
           (gnutls_credentials_set (Socket.SSL, cred => Socket.Config.ASC),
            Socket);

      else
         Check_Error_Code
           (gnutls_credentials_set (Socket.SSL, cred => Socket.Config.CSC),
            Socket);

         if Socket.Config.RCC then
            gnutls_certificate_server_set_request
              (Socket.SSL,
               (if Socket.Config.CREQ
                then GNUTLS_CERT_REQUIRE else GNUTLS_CERT_REQUEST));

            if Socket.Config.CRL_File /= C.Strings.Null_Ptr then
               declare
                  use type Calendar.Time;

                  TS : constant Calendar.Time :=
                         Utils.File_Time_Stamp
                           (C.Strings.Value (Socket.Config.CRL_File));
                  RC : C.int;
               begin
                  if Socket.Config.CRL_Time_Stamp = Utils.AWS_Epoch
                    or else Socket.Config.CRL_Time_Stamp /= TS
                  then
                     Socket.Config.CRL_Semaphore.Seize;

                     Socket.Config.CRL_Time_Stamp := TS;

                     RC := TSSL.gnutls_certificate_set_x509_crl_file
                             (Socket.Config.CSC,
                              Socket.Config.CRL_File,
                              TSSL.GNUTLS_X509_FMT_PEM);

                     Socket.Config.CRL_Semaphore.Release;

                     if RC = -1 then
                        raise Socket_Error
                          with "cannot set CRL file "
                            & C.Strings.Value (Socket.Config.CRL_File);
                     end if;
                  end if;
               end;
            end if;

         else
            gnutls_certificate_server_set_request
              (Socket.SSL, GNUTLS_CERT_IGNORE);
         end if;
      end if;

      Session_Transport (Socket);
   end Session_Server;

   -----------------------
   -- Session_Transport --
   -----------------------

   procedure Session_Transport (Socket : in out Socket_Type) is
   begin
      Check_Error_Code
        (TSSL.gnutls_priority_set (Socket.SSL, Socket.Config.Priority_Cache),
         Socket);

      TSSL.gnutls_transport_set_ptr
        (Socket.SSL, TSSL.gnutls_transport_ptr_t (Socket.Get_FD));

      pragma Warnings (Off, "*condition is always *");

      if OS_Lib.MSG_NOSIGNAL /= -1 then
         TSSL.gnutls_transport_set_push_function
           (Socket.SSL, push_func => Write_Socket'Address);
      end if;

      pragma Warnings (On, "*condition is always *");

      Socket.IO.Handshaken := new Boolean'(False);

      --  Record the SSL config to use in Verify_Callback for server and for
      --  Retrieve_Certificate for client.

      TSSL.gnutls_session_set_ptr (Socket.SSL, Socket.Config.all'Address);
   end Session_Transport;

   ----------------
   -- Set_Config --
   ----------------

   procedure Set_Config (Socket : in out Socket_Type; Config : SSL.Config) is
   begin
      Socket.Config := Config;
   end Set_Config;

   ---------------
   -- Set_Debug --
   ---------------

   procedure Set_Debug (Level : Natural) is
   begin
      TSSL.gnutls_global_set_log_function (SSL_Log'Access);
      TSSL.gnutls_global_set_audit_log_function (SSL_Log_Audit'Access);
      TSSL.gnutls_global_set_log_level (C.int (Level));
   end Set_Debug;

   ----------------------------
   -- Set_Session_Cache_Size --
   ----------------------------

   procedure Set_Session_Cache_Size
     (Size : Natural; Config : SSL.Config := Null_Config) is
   begin
      if Config = Null_Config then
         Initialize_Default_Config;
         Default_Config.Sessions.Set_Size (Size);
      else
         Config.Sessions.Set_Size (Size);
      end if;
   end Set_Session_Cache_Size;

   ----------------------
   -- Set_Session_Data --
   ----------------------

   procedure Set_Session_Data
     (Socket : in out Socket_Type; Data : Session_Type)
   is
      use type TSSL.gnutls_session_t;
   begin
      if Socket.SSL = null or else Socket.Get_FD = No_Socket then
         Socket.Sessn := Data;
      else
         Check_Error_Code
           (TSSL.gnutls_session_set_data
              (Socket.SSL, Data.Data.data, C.size_t (Data.Data.size)));
      end if;
   end Set_Session_Data;

   -------------------------
   -- Set_Verify_Callback --
   -------------------------

   procedure Set_Verify_Callback
     (Config : in out SSL.Config; Callback : System.Address)
   is
      function To_Callback is new Unchecked_Conversion
        (System.Address, Net.SSL.Certificate.Verify_Callback);
   begin
      Config.Verify_CB := To_Callback (Callback);
   end Set_Verify_Callback;

   --------------
   -- Shutdown --
   --------------

   overriding procedure Shutdown
     (Socket : Socket_Type; How : Shutmode_Type := Shut_Read_Write)
   is
      use type TSSL.Boolean_Access;
      use type TSSL.gnutls_session_t;

      Code : C.int;
      To_C : constant array (Shutmode_Type) of TSSL.gnutls_close_request_t :=
               (Shut_Read_Write => TSSL.GNUTLS_SHUT_RDWR,
                Shut_Read       => TSSL.GNUTLS_SHUT_RDWR, -- Absent, use RDWR
                Shut_Write      => TSSL.GNUTLS_SHUT_WR);
   begin
      if Socket.IO.Handshaken /= null and then Socket.IO.Handshaken.all then
         --  Must be done only after successful handshake

         loop
            Code := TSSL.gnutls_bye (Socket.SSL, To_C (How));

            exit when Code = TSSL.GNUTLS_E_SUCCESS;

            begin
               Code_Processing
                 (Code, Socket,
                  Duration'Min
                    (Net.Socket_Type (Socket).Timeout, Shutdown_Read_Timeout));
            exception
               when E : others =>
                  Net.Log.Error (Socket, Ada.Exceptions.Exception_Message (E));
                  exit;
            end;
         end loop;
      end if;

      if Socket.SSL /= null then
         TSSL.gnutls_transport_set_ptr (Socket.SSL, 0);
      end if;

      Net.Std.Shutdown (NSST (Socket), How);
   end Shutdown;

   ---------------
   -- Signature --
   ---------------

   function Signature
     (Ptr  : System.Address;
      Size : Interfaces.C.size_t;
      Key  : Private_Key;
      Hash : Hash_Method) return Stream_Element_Array
   is
      To_C : constant array (Hash_Method) of TSSL.gnutls_mac_algorithm_t
               := (MD5    => TSSL.GNUTLS_MAC_MD5,
                   SHA1   => TSSL.GNUTLS_MAC_SHA1,
                   SHA224 => TSSL.GNUTLS_MAC_SHA224,
                   SHA256 => TSSL.GNUTLS_MAC_SHA256,
                   SHA384 => TSSL.GNUTLS_MAC_SHA384,
                   SHA512 => TSSL.GNUTLS_MAC_SHA512);
      Dat : aliased TSSL.gnutls_datum_t := (Ptr, C.unsigned (Size));
      Sig : aliased TSSL.gnutls_datum_t;
   begin
      Check_Error_Code
        (TSSL.gnutls_privkey_sign_data
           (signer    => TSSL.Private_Key (Key),
            hash      => TSSL.gnutls_digest_algorithm_t (To_C (Hash)),
            flags     => 0,
            data      => Dat'Unchecked_Access,
            signature => Sig'Access));

      declare
         type Array_Access is access all
            Stream_Element_Array (1 .. Stream_Element_Offset (Sig.size));

         function To_Result is
           new Ada.Unchecked_Conversion (TSSL.a_unsigned_char_t, Array_Access);

         Result : constant Stream_Element_Array := To_Result (Sig.data).all;
      begin
         TSSL.gnutls_free (Sig.data);
         return Result;
      end;
   end Signature;

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

   -------------
   -- SSL_Log --
   -------------

   procedure SSL_Log (level : C.int; text : CS.chars_ptr) is
   begin
      SSL_Log_Common ("", level, text);
   end SSL_Log;

   -------------------
   -- SSL_Log_Audit --
   -------------------

   procedure SSL_Log_Audit
     (sessn : TSSL.gnutls_session_t; level : C.int; text : CS.chars_ptr)
   is
      pragma Unreferenced (sessn);
   begin
      SSL_Log_Common ("@", level, text);
   end SSL_Log_Audit;

   --------------------
   -- SSL_Log_Common --
   --------------------

   procedure SSL_Log_Common
     (Prefix : String; Level : C.int; text : CS.chars_ptr)
   is
      use Ada.Text_IO;

      Lev : constant String  := Level'Img;
      Fst : Positive := Lev'First;
      Adt : constant String := CS.Value (text);
   begin
      if Lev (Fst) = ' ' then
         Fst := Fst + 1;
      end if;

      Put (Current_Error, "|<" & Prefix & Lev (Fst .. Lev'Last) & ">| " & Adt);
   end SSL_Log_Common;

   ---------------------------------
   -- Start_Parameters_Generation --
   ---------------------------------

   procedure Start_Parameters_Generation
     (DH : Boolean; Logging : access procedure (Text : String) := null) is
   begin
      RSA_DH_Generators.Start_Parameters_Generation (DH, Logging);
   end Start_Parameters_Generation;

   ---------------------
   -- Verify_Callback --
   ---------------------

   function Verify_Callback (Session : TSSL.gnutls_session_t) return C.int is
      use type Net.SSL.Certificate.Verify_Callback;
      use type TSSL.a_gnutls_datum_t;

      type Datum_List is
        array (1 .. C.unsigned'Last) of aliased TSSL.gnutls_datum_t;
      pragma Convention (C, Datum_List);

      type Datum_List_Access is access all Datum_List;

      function To_Array_Access is
        new Ada.Unchecked_Conversion
              (TSSL.a_gnutls_datum_t, Datum_List_Access);

      Status        : aliased C.unsigned;
      Cfg           : Config;
      Cert_List     : TSSL.a_gnutls_datum_t;
      Cert_List_Len : aliased C.unsigned;
      Cert          : aliased TSSL.gnutls_x509_crt_t;
      Error_Code    : C.int;

      function Is_Error (Code : C.int) return Boolean;

      --------------
      -- Is_Error --
      --------------

      function Is_Error (Code : C.int) return Boolean is
      begin
         if Code < 0 then
            Log_Error (C.Strings.Value (TSSL.gnutls_strerror (Code)));
            return True;
         end if;

         return False;
      end Is_Error;

   begin
      --  Get the session config

      Cfg := To_Config (TSSL.gnutls_session_get_ptr (Session));

      Error_Code := TSSL.gnutls_certificate_verify_peers2
                      (Session, Status'Access);

      if Error_Code = TSSL.GNUTLS_E_NO_CERTIFICATE_FOUND and then not Cfg.CREQ
      then
         return 0;
      elsif Is_Error (Error_Code) then
         return TSSL.GNUTLS_E_CERTIFICATE_ERROR;
      end if;

      --  Get the peer certificate

      Cert_List := TSSL.gnutls_certificate_get_peers
                     (Session, Cert_List_Len'Access);

      if Cert_List = null then
         Log_Error ("gnutls_certificate_get_peers null result");
         return TSSL.GNUTLS_E_CERTIFICATE_ERROR;
      end if;

      if Cfg.Verify_CB /= null then
         for J in reverse 1 .. Cert_List_Len loop
            if Is_Error (TSSL.gnutls_x509_crt_init (Cert'Access)) then
               return TSSL.GNUTLS_E_CERTIFICATE_ERROR;
            end if;

            if Is_Error
                 (TSSL.gnutls_x509_crt_import
                    (Cert, To_Array_Access (Cert_List) (J)'Unchecked_Access,
                     TSSL.GNUTLS_X509_FMT_DER))
            then
               return TSSL.GNUTLS_E_CERTIFICATE_ERROR;
            end if;

            if not Cfg.Verify_CB (Net.SSL.Certificate.Impl.Read (Status, Cert))
              and then Status = 0
            then
               Status := 1;
            end if;

            TSSL.gnutls_x509_crt_deinit (Cert);
         end loop;
      end if;

      if Status = 0 or else not Cfg.CREQ then
         return 0;
      else
         return TSSL.GNUTLS_E_CERTIFICATE_ERROR;
      end if;

   exception
      when E : others =>
         Log_Error (Ada.Exceptions.Exception_Message (E));
         return TSSL.GNUTLS_E_CERTIFICATE_ERROR;
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

   ------------------
   -- Write_Socket --
   ------------------

   function Write_Socket
     (S : C.int; Msg : System.Address; Len : C.int) return C.int
   is
      function C_Send
        (S     : C.int;
         Msg   : System.Address;
         Len   : C.int;
         Flags : C.int) return C.int
      with Import, Convention => Stdcall, External_Name => "send";

   begin
      return C_Send (S, Msg, Len, OS_Lib.MSG_NOSIGNAL);
   end Write_Socket;

begin
   TSSL.gnutls_global_set_mem_functions
     (alloc_func        => (if Set_Certificate_Over_Callback
                            then Lib_Alloc'Address
                            else System.Memory.Alloc'Address),
      secure_alloc_func => System.Memory.Alloc'Address,
      is_secure_func    => null,
      realloc_func      => Lib_Realloc'Address,
      free_func         => Lib_Free'Access);

   if TSSL.gnutls_global_init /= 0 then
      raise Program_Error;
   end if;
end AWS.Net.SSL;
