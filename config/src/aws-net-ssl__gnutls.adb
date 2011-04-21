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
with Ada.Exceptions;
with Ada.Streams.Stream_IO;
with Ada.Strings.Fixed;
with Ada.Unchecked_Deallocation;

with Interfaces.C.Strings;
with System;

with AWS.Config;
with AWS.Net.Log;
with AWS.Utils;

package body AWS.Net.SSL is

   use Interfaces;

   use type C.unsigned;
   use type C.int;

   subtype NSST is Net.Std.Socket_Type;

   type Mutex_Access is access all AWS.Utils.Semaphore;

   procedure Check_Error_Code (Code : C.int);
   procedure Check_Error_Code (Code : C.int; Socket : Socket_Type'Class);

   procedure Check_Error_Code (Code : TSSL.gcry_error_t);

   procedure Code_Processing (Code : C.int; Socket : Socket_Type'Class);

   procedure Check_Config (Socket : in out Socket_Type);
   pragma Inline (Check_Config);

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
   end record;

   procedure Initialize
     (Config               : in out TS_SSL;
      Certificate_Filename : String;
      Security_Mode        : Method  := SSLv23;
      Key_Filename         : String  := "";
      Exchange_Certificate : Boolean := False;
      Session_Cache_Size   : Positive   := 16#4000#);

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
     (Source : Net.Socket_Type'Class;
      Target : out Socket_Type;
      Config : SSL.Config);

   -------------------
   -- Accept_Socket --
   -------------------

   overriding procedure Accept_Socket
     (Socket     : Net.Socket_Type'Class;
      New_Socket : in out Socket_Type) is
   begin
      Net.Std.Accept_Socket (Socket, NSST (New_Socket));
      Session_Server (New_Socket);
      Do_Handshake (New_Socket);
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

   procedure Check_Error_Code
     (Code : C.int; Socket : Socket_Type'Class) is
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

   procedure Code_Processing (Code : C.int; Socket : Socket_Type'Class) is
   begin
      case Code is
      when TSSL.GNUTLS_E_INTERRUPTED | TSSL.GNUTLS_E_AGAIN =>
         case TSSL.gnutls_record_get_direction (Socket.SSL) is
         when 0 => Wait_For (Input, Socket);
         when 1 => Wait_For (Output, Socket);
         when others => raise Program_Error;
         end case;
      when others => Check_Error_Code (Code, Socket);
      end case;
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
               Exchange_Certificate => CNF.Exchange_Certificate (Default));

            Done := True;
         end if;
      end Create_Default_Config;

   end Default_Config_Synch;

   ------------------
   -- Do_Handshake --
   ------------------

   procedure Do_Handshake (Socket : in out Socket_Type) is
      Code : TSSL.ssize_t;
   begin
      loop
         Code := TSSL.gnutls_handshake (Socket.SSL);

         exit when Code = TSSL.GNUTLS_E_SUCCESS;

         Code_Processing (Code, Socket);
      end loop;
   end Do_Handshake;

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
   end Finalize;

   ----------
   -- Free --
   ----------

   overriding procedure Free (Socket : in out Socket_Type) is
      use type TSSL.gnutls_session_t;
   begin
      if Socket.SSL /= null then
         TSSL.gnutls_deinit (Socket.SSL);
         Socket.SSL := null;
      end if;

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
         Session_Cache_Size   => Session_Cache_Size);
   end Initialize;

   procedure Initialize
     (Config               : in out TS_SSL;
      Certificate_Filename : String;
      Security_Mode        : Method     := SSLv23;
      Key_Filename         : String     := "";
      Exchange_Certificate : Boolean    := False;
      Session_Cache_Size   : Positive   := 16#4000#)
   is
      pragma Unreferenced (Session_Cache_Size);
      use type TSSL.gnutls_anon_client_credentials_t;
      use type TSSL.gnutls_anon_server_credentials_t;
      use type TSSL.gnutls_certificate_credentials_t;
      use type TSSL.gnutls_dh_params_t;

      procedure Set_Certificate
        (CC : TSSL.gnutls_certificate_credentials_t);
      --  Set credentials from Cetificate_Filename and Key_Filename

      ---------------------
      -- Set_Certificate --
      ---------------------

      procedure Set_Certificate
        (CC : TSSL.gnutls_certificate_credentials_t)
      is

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
               Raise_Exception
                 (Socket_Error'Identity,
                  Prefix & " file """ & Filename & """ error.");
            end if;
         end Check_File;

         Code : C.int;

      begin
         if Key_Filename = "" then
            --  Load certificates and private key from Certificate_File

            Check_File ("Certificate", Certificate_Filename);

            declare
               use Ada.Strings;

               function Get_File_Data return String;
               --  Returns certificate file data

               Prefix : constant String := "-----BEGIN ";
               Suffix : constant String := "-----END ";
               Cert_C : constant String := "CERTIFICATE-----";
               First  : Natural := 1;
               Last   : Natural;
               Cert   : aliased TSSL.gnutls_datum_t
                 := (System.Null_Address, 0);
               Key    : aliased TSSL.gnutls_datum_t
                 := (System.Null_Address, 0);

               -------------------
               -- Get_File_Data --
               -------------------

               function Get_File_Data return String is
                  use Ada.Streams.Stream_IO;
                  File : File_Type;
               begin
                  Open
                    (File, In_File, Certificate_Filename,
                     Form => "shared=no");

                  declare
                     Result : aliased String (1 .. Natural (Size (File)));
                  begin
                     String'Read (Stream (File), Result);
                     Close (File);
                     return Result;
                  end;
               end Get_File_Data;

               Data : aliased constant String := Get_File_Data;

            begin
               loop
                  First := Fixed.Index (Data (First .. Data'Last), Prefix);
                  exit when First = 0;

                  Last := Fixed.Index (Data (First .. Data'Last), Suffix);

                  if Last = 0 then
                     Last := Data'Last;
                  else
                     Last := Fixed.Index
                       (Data (Last .. Data'Last), "" & ASCII.LF);
                     if Last = 0 then
                        Last := Data'Last;
                     end if;
                  end if;

                  if Data (First + Prefix'Length
                           .. First + Prefix'Length + Cert_C'Length - 1)
                    = Cert_C
                  then
                     if Cert.size = 0 then
                        Cert.data := Data (First)'Address;

                        if Key.size = 0 then
                           --  Store first certificate position temporary
                           --  in size field and wait for private key.

                           Cert.size := C.unsigned (First);

                        else
                           --  If key already gotten then all other data
                           --  is certificates list.

                           Cert.size := C.unsigned (Data'Last - First);

                           exit;
                        end if;
                     end if;

                  else
                     Key.data := Data (First)'Address;
                     Key.size := C.unsigned (Last - First);

                     if Cert.size > 0 then
                        --  If key gotten after certificate, calculate
                        --  size of certificates list.

                        Cert.size := C.unsigned (First) - Cert.size;
                        exit;
                     end if;
                  end if;

                  exit when Last = Data'Last;
                  First := Last;
               end loop;

               Code := TSSL.gnutls_certificate_set_x509_key_mem
                 (CC,
                  cert => Cert'Unchecked_Access,
                  key  => Key'Unchecked_Access,
                  p4   => TSSL.GNUTLS_X509_FMT_PEM);

               if Code = TSSL.GNUTLS_E_BASE64_DECODING_ERROR then
                  Raise_Exception
                    (Socket_Error'Identity,
                     "Certificate file """
                     & Certificate_Filename & """ error.");
               else
                  Check_Error_Code (Code);
               end if;
            end;

         else
            Check_File ("Certificate", Certificate_Filename);
            Check_File ("Key", Key_Filename);

            declare
               Cert : aliased C.char_array := C.To_C (Certificate_Filename);
               Key  : aliased C.char_array := C.To_C (Key_Filename);
            begin
               Code := TSSL.gnutls_certificate_set_x509_key_file
                 (CC,
                  C.Strings.To_Chars_Ptr (Cert'Unchecked_Access),
                  C.Strings.To_Chars_Ptr (Key'Unchecked_Access),
                  TSSL.GNUTLS_X509_FMT_PEM);

               if Code = TSSL.GNUTLS_E_BASE64_DECODING_ERROR then
                  Raise_Exception
                    (Socket_Error'Identity,
                     "Certificate/Key file error.");
               else
                  Check_Error_Code (Code);
               end if;
            end;
         end if;
      end Set_Certificate;

   begin
      if (Security_Mode = SSLv2
          or else Security_Mode = SSLv23
          or else Security_Mode = TLSv1
          or else Security_Mode = SSLv3
          or else Security_Mode = SSLv2_Server
          or else Security_Mode = SSLv23_Server
          or else Security_Mode = TLSv1_Server
          or else Security_Mode = SSLv3_Server)
        and then Config.ASC = null
        and then Config.CSC = null
      then
         Check_Error_Code
           (TSSL.gnutls_dh_params_init (Config.DH_Params'Access));
         Check_Error_Code
           (TSSL.gnutls_dh_params_generate2 (Config.DH_Params, DH_Bits));

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

            TSSL.gnutls_certificate_set_dh_params
              (Config.CSC, Config.DH_Params);
         end if;

         Config.RCC := Exchange_Certificate;
      end if;

      if (Security_Mode = SSLv2
          or else Security_Mode = SSLv23
          or else Security_Mode = TLSv1
          or else Security_Mode = SSLv3
          or else Security_Mode = SSLv2_Client
          or else Security_Mode = SSLv23_Client
          or else Security_Mode = TLSv1_Client
          or else Security_Mode = SSLv3_Client)
        and then Config.CCC = null
      then
         Check_Error_Code
           (TSSL.gnutls_anon_allocate_client_credentials (Config.ACC'Access));
         Check_Error_Code
           (TSSL.gnutls_certificate_allocate_credentials (Config.CCC'Access));
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
      Config : SSL.Config := Null_Config)
      return Socket_Type
   is
      Result : Socket_Type;
   begin
      Secure (Socket, Result, Config);
      Session_Client (Result);
      Do_Handshake (Result);
      return Result;
   end Secure_Client;

   -------------------
   -- Secure_Server --
   -------------------

   function Secure_Server
     (Socket : Net.Socket_Type'Class;
      Config : SSL.Config := Null_Config)
      return Socket_Type
   is
      Result : Socket_Type;
   begin
      Secure (Socket, Result, Config);
      Session_Server (Result);
      Do_Handshake (Result);
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
      loop
         Code :=
           TSSL.gnutls_record_send (Socket.SSL, Data'Address, Data'Length);

         if Code >= 0 then
            Last := Last_Index (Data'First, Natural (Code));
            exit;
         end if;

         Code_Processing (Code, Socket);
      end loop;
   end Send;

   --------------------
   -- Session_Client --
   --------------------

   procedure Session_Client (Socket : in out Socket_Type) is
      use TSSL;
      Session : aliased gnutls_session_t;

      type Priority_List is array (0 .. 4) of gnutls_kx_algorithm_t;
      pragma Convention (C, Priority_List);

      kx_prio : constant Priority_List :=
                  (GNUTLS_KX_RSA, GNUTLS_KX_RSA_EXPORT,
                   GNUTLS_KX_DHE_RSA, GNUTLS_KX_DHE_DSS,
                   GNUTLS_0);

   begin
      Check_Config (Socket);

      Check_Error_Code (gnutls_init (Session'Access, GNUTLS_CLIENT), Socket);

      Socket.SSL := Session;

      Check_Error_Code (gnutls_set_default_priority (Session), Socket);

      Check_Error_Code
        (gnutls_kx_set_priority (Session, kx_prio'Address), Socket);
      Check_Error_Code
        (gnutls_credentials_set (Session, cred => Socket.Config.ACC), Socket);

      Check_Error_Code
        (gnutls_credentials_set (Session, cred => Socket.Config.CCC), Socket);

      gnutls_dh_set_prime_bits (Session, DH_Bits);

      Session_Transport (Socket);
   end Session_Client;

   --------------------
   -- Session_Server --
   --------------------

   procedure Session_Server (Socket : in out Socket_Type) is
      use TSSL;
      Session : aliased gnutls_session_t;

      type Priority_List is array (0 .. 4) of gnutls_kx_algorithm_t;
      pragma Convention (C, Priority_List);
      kx_prio : constant Priority_List :=
                  (GNUTLS_KX_RSA, GNUTLS_KX_RSA_EXPORT,
                   GNUTLS_KX_DHE_RSA, GNUTLS_KX_DHE_DSS,
                   GNUTLS_0);

   begin
      Check_Config (Socket);

      Check_Error_Code (gnutls_init (Session'Access, GNUTLS_SERVER), Socket);

      Check_Error_Code (gnutls_set_default_priority (Session), Socket);

      Check_Error_Code
        (gnutls_kx_set_priority (Session, kx_prio'Address), Socket);

      if Socket.Config.CSC = null then
         Check_Error_Code
           (gnutls_credentials_set (Session, cred => Socket.Config.ASC),
            Socket);

      else
         Check_Error_Code
           (gnutls_credentials_set
              (Session, GNUTLS_CRD_CERTIFICATE, Socket.Config.CSC),
            Socket);

         if Socket.Config.RCC then
            gnutls_certificate_server_set_request
              (Session, GNUTLS_CERT_REQUEST);
         else
            gnutls_certificate_server_set_request
              (Session, GNUTLS_CERT_IGNORE);
         end if;
      end if;

      gnutls_dh_set_prime_bits (Session, DH_Bits);

      Socket.SSL := Session;

      Session_Transport (Socket);
   end Session_Server;

   -----------------------
   -- Session_Transport --
   -----------------------

   procedure Session_Transport (Socket : in out Socket_Type) is
   begin
      TSSL.gnutls_transport_set_ptr
        (Socket.SSL, TSSL.gnutls_transport_ptr_t (Socket.Get_FD));

      --  www.gnu.org/software/gnutls/manual/html_node/The-transport-layer.html
      --
      --  For non blocking sockets or other custom made pull/push functions
      --  the gnutls_transport_set_lowat must be called, with a zero low water
      --  mark value.

      TSSL.gnutls_transport_set_lowat (Socket.SSL, 0);
   end Session_Transport;

   ----------------
   -- Set_Config --
   ----------------

   procedure Set_Config
     (Socket : in out Socket_Type;
      Config : SSL.Config) is
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

   --------------
   -- Shutdown --
   --------------

   overriding procedure Shutdown
     (Socket : Socket_Type; How : Shutmode_Type := Shut_Read_Write)
   is
      use System;
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
            Code_Processing (Code, Socket);
         exception when E : others =>
            Net.Log.Error (Socket, Ada.Exceptions.Exception_Message (E));
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
