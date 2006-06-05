------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                            Copyright (C) 2006                            --
--                                  AdaCore                                 --
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

with Ada.Exceptions;
with Ada.Streams.Stream_IO;
with Ada.Strings.Fixed;
with Ada.Task_Attributes;
with Ada.Unchecked_Conversion;
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

   package Skip_Exceptions is new Ada.Task_Attributes
     (Ada.Exceptions.Exception_Occurrence_Access, null);

   subtype NSST is Net.Std.Socket_Type;

   type Mutex_Access is access all AWS.Utils.Semaphore;

   subtype Stream_Array is
     Stream_Element_Array (1 .. Stream_Element_Offset'Last);

   procedure Check_Error_Code (Code : in C.int);
   procedure Check_Error_Code (Code : in C.int; Socket : in Socket_Type'Class);

   procedure Check_Config (Socket : in out Socket_Type);
   pragma Inline (Check_Config);

   procedure Save_Exception (E : in Ada.Exceptions.Exception_Occurrence);

   function Push
     (Socket : in Std.Socket_Type;
      Data   : in Stream_Array;
      Length : in Stream_Element_Count) return Stream_Element_Offset;
   pragma Convention (C, Push);

   function Pull
     (Socket : in     Std.Socket_Type;
      Data   : access Stream_Array;
      Length : in     Stream_Element_Count) return Stream_Element_Offset;
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
      CSC       : aliased TSSL.gnutls_certificate_credentials_t;
      CCC       : aliased TSSL.gnutls_certificate_credentials_t;
      DH_Params : aliased TSSL.gnutls_dh_params_t;
      RCC       : Boolean := False; -- Request client certificate
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
     (Code : in C.int; Socket : in Socket_Type'Class) is
   begin
      if Code = TSSL.GNUTLS_E_PULL_ERROR
        or else Code = TSSL.GNUTLS_E_PUSH_ERROR
      then
         Ada.Exceptions.Reraise_Occurrence (Skip_Exceptions.Value.all);

      elsif Code /= 0 then
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
     (Socket : in out Socket_Type;
      Host   : in     String;
      Port   : in     Positive;
      Wait   : in     Boolean := True) is
   begin
      Net.Std.Connect (NSST (Socket), Host, Port, Wait);

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
   begin
      Check_Error_Code (TSSL.gnutls_handshake (Socket.SSL), Socket);
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

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Socket : in out Socket_Type) is
      use System;
      use type TSSL.gnutls_session_t;

      function To_Access is new Ada.Unchecked_Conversion
        (TSSL.gnutls_transport_ptr_t, Socket_Access);

      Sock : Socket_Access;
   begin
      if Socket.SSL /= null
        and then Net.Socket_Type (Socket).C.Ref_Count.Value = 2
      then
         --  Free one more reference from gnutls_transport_ptr_t

         Sock := To_Access (TSSL.gnutls_transport_get_ptr (Socket.SSL));

         --  Unregister the push/pull callbacks to avoid access violation in
         --  case the socket was not shutdown properly. on the

         TSSL.gnutls_transport_set_push_function (Socket.SSL, Null_Address);
         TSSL.gnutls_transport_set_pull_function (Socket.SSL, Null_Address);

         TSSL.gnutls_transport_set_ptr
           (Socket.SSL, TSSL.gnutls_transport_ptr_t (System.Null_Address));

         Free (Sock);
      end if;

      Std.Finalize (Std.Socket_Type (Socket));
   end Finalize;

   ----------
   -- Free --
   ----------

   procedure Free (Socket : in out Socket_Type) is
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
      use type TSSL.gnutls_anon_client_credentials_t;
      use type TSSL.gnutls_anon_server_credentials_t;
      use type TSSL.gnutls_certificate_credentials_t;
      use type TSSL.gnutls_dh_params_t;

      procedure Set_Certificate
        (CC : in out TSSL.gnutls_certificate_credentials_t);
      --  Set credentials from Cetificate_Filename and Key_Filename

      ---------------------
      -- Set_Certificate --
      ---------------------

      procedure Set_Certificate
        (CC : in out TSSL.gnutls_certificate_credentials_t) is
      begin
         if Key_Filename = "" then
            --  Load certificates and private key from Certificate_File

            declare
               use Ada.Strings;
               use type C.unsigned;

               function Get_File_Data return String;

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

               exception
                  when Name_Error =>
                     Ada.Exceptions.Raise_Exception
                       (Socket_Error'Identity,
                        "file """ & Certificate_Filename & """ error.");
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

               Check_Error_Code
                 (TSSL.gnutls_certificate_set_x509_key_mem
                    (CC,
                     Cert => Cert'Unchecked_Access,
                     Key  => Key'Unchecked_Access,
                     P4   => TSSL.GNUTLS_X509_FMT_PEM));
            end;

         else
            declare
               Cert : aliased C.char_array := C.To_C (Certificate_Filename);
               Key  : aliased C.char_array := C.To_C (Key_Filename);
            begin
               Check_Error_Code
                 (TSSL.gnutls_certificate_set_x509_key_file
                    (CC,
                     C.Strings.To_Chars_Ptr (Cert'Unchecked_Access),
                     C.Strings.To_Chars_Ptr (Key'Unchecked_Access),
                     TSSL.GNUTLS_X509_FMT_PEM));
            end;
         end if;
      end Set_Certificate;

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
          or Security_Mode = SSLv23
          or Security_Mode = TLSv1
          or Security_Mode = SSLv3
          or Security_Mode = SSLv2_Client
          or Security_Mode = SSLv23_Client
          or Security_Mode = TLSv1_Client
          or Security_Mode = SSLv3_Client)
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

      ----------
      -- Init --
      ----------

      function Init (Item : access Mutex_Access) return Integer is
      begin
         Item.all := new Semaphore;
         return 0;
      end Init;

      ----------
      -- Lock --
      ----------

      function Lock (Item : access Mutex_Access) return Integer is
      begin
         Item.all.Seize;
         return 0;
      end Lock;

      ------------
      -- Unlock --
      ------------

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
      Length : in     Stream_Element_Count) return Stream_Element_Offset
   is
      Last : Stream_Element_Offset;
   begin
      Std.Receive (Socket, Data (1 .. Length), Last);
      return Last;
   exception
      when E : others =>
         Save_Exception (E);
         return -1;
   end Pull;

   ----------
   -- Push --
   ----------

   function Push
     (Socket : in Std.Socket_Type;
      Data   : in Stream_Array;
      Length : in Stream_Element_Count) return Stream_Element_Offset
   is
      Last : Stream_Element_Count;
   begin
      Std.Send (Socket, Data (1 .. Length), Last);
      return Last;
   exception
      when E : others =>
         Save_Exception (E);
         return -1;
   end Push;

   -------------
   -- Receive --
   -------------

   procedure Receive
     (Socket : in     Socket_Type;
      Data   :    out Stream_Element_Array;
      Last   :    out Stream_Element_Offset)
   is
      use type TSSL.ssize_t;
      Code : constant TSSL.ssize_t
        := TSSL.gnutls_record_recv (Socket.SSL, Data'Address, Data'Length);
   begin
      if Code < 0 then
         Check_Error_Code (TSSL.GNUTLS_E_PULL_ERROR, Socket);
      end if;

      Last := Data'First + Stream_Element_Offset (Code) - 1;
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

   --------------------
   -- Save_Exception --
   --------------------

   procedure Save_Exception (E : in Ada.Exceptions.Exception_Occurrence) is
      TA : constant Skip_Exceptions.Attribute_Handle
        := Skip_Exceptions.Reference;
   begin
      if TA.all = null then
         TA.all := Ada.Exceptions.Save_Occurrence (E);
      else
         Ada.Exceptions.Save_Occurrence (TA.all.all, E);
      end if;
   end Save_Exception;

   ------------
   -- Secure --
   ------------

   procedure Secure
     (Source : in     Net.Socket_Type'Class;
      Target :    out Socket_Type;
      Config : in     SSL.Config) is
   begin
      Std.Socket_Type (Target) := Std.Socket_Type (Source);
      Target.Config := Config;
      Check_Config (Target);
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
      Do_Handshake (Result);
      return Result;
   end Secure_Server;

   ----------
   -- Send --
   ----------

   procedure Send
     (Socket : in     Socket_Type;
      Data   : in     Stream_Element_Array;
      Last   :    out Stream_Element_Offset)
   is
      use type TSSL.ssize_t;
      Code : constant TSSL.ssize_t
        := TSSL.gnutls_record_send (Socket.SSL, Data'Address, Data'Length);
   begin
      if Code < 0 then
         Check_Error_Code (TSSL.GNUTLS_E_PUSH_ERROR, Socket);
      end if;

      if Data'First = Stream_Element_Offset'First and Code = 0 then
         Last := Data'Last + 1;
      else
         Last := Data'First + Stream_Element_Offset (Code) - 1;
      end if;
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
      Sock : constant Socket_Access
        := new Std.Socket_Type'(Std.Socket_Type (Socket));
   begin
      --  Note : We make a copy of socket into the GNU/TLS transport ptr.
      --  Finalize have to Free Socket when reference counter is 2 and it must
      --  free the internal copy.

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
      use System;
      Code : C.int;
   begin
      --  Unregister the push/pull callback to avoid locking on the
      --  gnutls_bye() call.

      TSSL.gnutls_transport_set_push_function (Socket.SSL, Null_Address);
      TSSL.gnutls_transport_set_pull_function (Socket.SSL, Null_Address);

      Code := TSSL.gnutls_bye (Socket.SSL, TSSL.GNUTLS_SHUT_RDWR);

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
     (CMD        => TSSL.GCRYCTL_SET_THREAD_CBS,
      Thread_CBS => (Option        => TSSL.GCRY_THREAD_OPTION_USER,
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
