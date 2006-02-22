------------------------------------------------------------------------------
--                            Secure Sockets Layer                          --
--                         Binding to OpenSSL library                       --
--                                                                          --
--                         Copyright (C) 2000-2006                          --
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

with Interfaces.C.Strings;
with System;

package SSL.Thin is

   use Interfaces.C;

   package Cstr renames Interfaces.C.Strings;

   type Rand_Meth_St is record
      Seed, Bytes, Cleanup, Add, Pseudorand, Status : System.Address;
   end record;
   pragma Convention (C, Rand_Meth_St);

   subtype Pointer is System.Address;
   Null_Pointer : constant Pointer := System.Null_Address;

   subtype SSL_Method is Pointer;
   subtype SSL_CTX    is Pointer;
   subtype SSL_Handle is Pointer;
   subtype RSA        is Pointer;
   subtype X509       is Pointer;
   subtype X509_Name  is Pointer;

   Null_CTX    : SSL_CTX    renames Null_Pointer;
   Null_Handle : SSL_Handle renames Null_Pointer;

   subtype Error_Code is unsigned_long;

   SSL_FILETYPE_PEM                  : constant := 1;
   SSL_CTRL_NEED_TMP_RSA             : constant := 1;
   SSL_CTRL_SET_TMP_RSA              : constant := 2;
   SSL_CTRL_SET_TMP_DH               : constant := 3;
   SSL_CTRL_SET_TMP_RSA_CB           : constant := 4;
   SSL_CTRL_SET_TMP_DH_CB            : constant := 5;
   SSL_CTRL_GET_SESSION_REUSED       : constant := 6;
   SSL_CTRL_GET_CLIENT_CERT_REQUEST  : constant := 7;
   SSL_CTRL_GET_NUM_RENEGOTIATIONS   : constant := 8;
   SSL_CTRL_CLEAR_NUM_RENEGOTIATIONS : constant := 9;
   SSL_CTRL_GET_TOTAL_RENEGOTIATIONS : constant := 10;
   SSL_CTRL_GET_FLAGS                : constant := 11;
   SSL_CTRL_EXTRA_CHAIN_CERT         : constant := 12;
   SSL_CTRL_SESS_NUMBER              : constant := 20;
   SSL_CTRL_SESS_CONNECT             : constant := 21;
   SSL_CTRL_SESS_CONNECT_GOOD        : constant := 22;
   SSL_CTRL_SESS_CONNECT_RENEGOTIATE : constant := 23;
   SSL_CTRL_SESS_ACCEPT              : constant := 24;
   SSL_CTRL_SESS_ACCEPT_GOOD         : constant := 25;
   SSL_CTRL_SESS_ACCEPT_RENEGOTIATE  : constant := 26;
   SSL_CTRL_SESS_HIT                 : constant := 27;
   SSL_CTRL_SESS_CB_HIT              : constant := 28;
   SSL_CTRL_SESS_MISSES              : constant := 29;
   SSL_CTRL_SESS_TIMEOUTS            : constant := 30;
   SSL_CTRL_SESS_CACHE_FULL          : constant := 31;
   SSL_CTRL_OPTIONS                  : constant := 32;
   SSL_CTRL_MODE                     : constant := 33;
   SSL_CTRL_GET_READ_AHEAD           : constant := 40;
   SSL_CTRL_SET_READ_AHEAD           : constant := 41;
   SSL_CTRL_SET_SESS_CACHE_SIZE      : constant := 42;
   SSL_CTRL_GET_SESS_CACHE_SIZE      : constant := 43;
   SSL_CTRL_SET_SESS_CACHE_MODE      : constant := 44;
   SSL_CTRL_GET_SESS_CACHE_MODE      : constant := 45;
   SSL_SENT_SHUTDOWN                 : constant := 1;
   SSL_RECEIVED_SHUTDOWN             : constant := 2;

   SSL_VERIFY_NONE                   : constant := 0;
   SSL_VERIFY_PEER                   : constant := 1;
   SSL_VERIFY_FAIL_IF_NO_PEER_CERT   : constant := 2;
   SSL_VERIFY_CLIENT_ONCE            : constant := 4;
   SSL_F_SSL_VERIFY_CERT_CHAIN       : constant := 207;

   SSL_ERROR_NONE             : constant := 0;
   SSL_ERROR_SSL              : constant := 1;
   SSL_ERROR_WANT_READ        : constant := 2;
   SSL_ERROR_WANT_WRITE       : constant := 3;
   SSL_ERROR_WANT_X509_LOOKUP : constant := 4;
   SSL_ERROR_SYSCALL          : constant := 5;
   SSL_ERROR_ZERO_RETURN      : constant := 6;
   SSL_ERROR_WANT_CONNECT     : constant := 7;
   SSL_ERROR_WANT_ACCEPT      : constant := 8;

   RSA_3  : constant := 3;
   RSA_F4 : constant := 16#10001#;

   --------------------------
   --  SSL mode constants. --
   --------------------------

   SSL_MODE_ENABLE_PARTIAL_WRITE : constant := 1;
   --  Allow SSL_write(..., n) to return r with 0 < r < n (i.e. report success
   --  when just a single record has been written).

   SSL_MODE_ACCEPT_MOVING_WRITE_BUFFER : constant := 2;
   --  Make it possible to retry SSL_write() with changed buffer location
   --  (buffer contents must stay the same!); this is not the default to avoid
   --  the misconception that non-blocking SSL_write() behaves like
   --  non-blocking write().

   SSL_MODE_AUTO_RETRY : constant := 4;
   --  Never bother the application with retries if the transport
   --  is blocking.

   SSL_MODE_NO_AUTO_CHAIN : constant := 8;
   --  Don't attempt to automatically build certificate chain

   -----------------------------------------------
   -- Multithread data access locking constants --
   -----------------------------------------------

   CRYPTO_LOCK   : constant := 1;
   CRYPTO_UNLOCK : constant := 2;
   CRYPTO_READ   : constant := 4;
   CRYPTO_WRITE  : constant := 8;

   ----------------------------------------------------
   -- Multithread data access setup locking routines --
   ----------------------------------------------------

   function  CRYPTO_num_locks return Natural;

   procedure CRYPTO_set_id_callback (Id_Function : Pointer);
   procedure CRYPTO_set_locking_callback (Locking_Function : Pointer);
   function  CRYPTO_get_locking_callback return Pointer;

   procedure CRYPTO_set_dynlock_create_callback  (Create_Function : Pointer);
   procedure CRYPTO_set_dynlock_lock_callback    (Lock_Function : Pointer);
   procedure CRYPTO_set_dynlock_destroy_callback (Destroy_Function : Pointer);

   function CRYPTO_get_dynlock_create_callback  return Pointer;
   function CRYPTO_get_dynlock_lock_callback    return Pointer;
   function CRYPTO_get_dynlock_destroy_callback return Pointer;

   -----------------------------------------------------------
   -- End of multithread data access setup locking routines --
   -----------------------------------------------------------

   function SSLeay return long;
   --  Returns OpenSSL numeric release version identifier

   function SSLv3_method         return SSL_Method;
   function SSLv3_server_method  return SSL_Method;
   function SSLv3_client_method  return SSL_Method;
   function SSLv2_method         return SSL_Method;
   function SSLv2_server_method  return SSL_Method;
   function SSLv2_client_method  return SSL_Method;
   function SSLv23_method        return SSL_Method;
   function SSLv23_server_method return SSL_Method;
   function SSLv23_client_method return SSL_Method;
   function TLSv1_method         return SSL_Method;
   function TLSv1_server_method  return SSL_Method;
   function TLSv1_client_method  return SSL_Method;

   function CRYPTO_set_mem_functions
     (M : in System.Address;
      R : in System.Address;
      F : in System.Address)
      return int;

   function SSL_CTX_new (Meth : in SSL_Method) return SSL_CTX;

   procedure SSL_CTX_free (P1 : in SSL_CTX);

   procedure SSL_CTX_set_quiet_shutdown (Ctx : in SSL_CTX; Mode : in int);

   function SSL_CTX_ctrl
     (Ctx  : in SSL_CTX;
      Cmd  : in int;
      Larg : in int;
      Parg : in Pointer)
      return int;

   procedure SSL_library_init;

   procedure SSL_load_error_strings;

   procedure ERR_load_crypto_strings;

   procedure ERR_load_ssl_strings;

   function ERR_get_error return Error_Code;

   function ERR_error_string
     (Code   : in Error_Code;
      Buffer : in Cstr.chars_ptr)
      return Cstr.chars_ptr;

   procedure ERR_error_string_n
     (Code   : in Error_Code;
      Buffer : in Cstr.chars_ptr;
      Len    : in size_t);

   procedure ERR_Remove_State (pid : in int := 0);

   function SSL_new (Ctx : in SSL_CTX) return SSL_Handle;

   procedure SSL_free (SSL : in SSL_Handle);

   function SSL_clear (SSL : in SSL_Handle) return int;

   function SSL_set_fd
     (S  : in SSL_Handle;
      Fd : in int)
      return int;

   procedure SSL_set_read_ahead
     (S   : in SSL_Handle;
      Yes : in int);

   function SSL_connect (SSL : in SSL_Handle) return int;

   function SSL_accept (SSL : in SSL_Handle) return int;

   procedure SSL_set_connect_state (SSL : in SSL_Handle);

   procedure SSL_set_accept_state (SSL : in SSL_Handle);

   function SSL_renegotiate (SSL : in SSL_Handle) return int;

   function SSL_do_handshake (SSL : in SSL_Handle) return int;

   function SSL_want (S : in SSL_Handle) return int;

   function SSL_read
     (SSL : in SSL_Handle;
      Buf : in Pointer;
      Num : in int)
      return int;

   function SSL_peek
     (SSL : in SSL_Handle;
      Buf : in Pointer;
      Num : in int)
      return int;

   function SSL_write
     (SSL : in SSL_Handle;
      Buf : in Pointer;
      Num : in int)
      return int;

   function SSL_pending (S : in SSL_Handle) return int;

   function SSL_get_error (SSL : in SSL_Handle; ret : in int) return int;

   type Generate_Key_Callback is access
     procedure (I1, I2 : in Integer; Param : in Pointer);
   pragma Convention (C, Generate_Key_Callback);

   function RSA_generate_key
     (Bits     : in int;
      E        : in unsigned;
      Callback : in Generate_Key_Callback;
      Cb_Arg   : in Pointer)
      return RSA;

   function SSL_use_RSAPrivateKey
     (SSL         : in SSL_Handle;
      Private_Key : in RSA)
      return int;

   function SSL_shutdown (SSL : in SSL_Handle) return int;

   procedure SSL_set_shutdown (SSL : in SSL_Handle; Mode : in int);

   function SSL_CTX_use_PrivateKey_file
     (Ctx    : in SSL_CTX;
      File   : in char_array;
      C_Type : in int)
      return int;

   function SSL_use_PrivateKey_file
     (SSL    : in SSL_Handle;
      File   : in char_array;
      C_Type : in int)
      return int;

   function SSL_CTX_use_certificate_file
     (Ctx    : in SSL_CTX;
      File   : in char_array;
      C_Type : in int)
      return int;

   function SSL_CTX_use_certificate_chain_file
     (Ctx    : in SSL_CTX;
      File   : in char_array)
      return int;

   function SSL_use_certificate_file
     (SSL    : in SSL_Handle;
      File   : in char_array;
      C_Type : in int)
      return int;

   function SSL_CTX_check_private_key (Ctx : in SSL_CTX) return int;

   procedure SSL_CTX_set_verify
     (Ctx      : in SSL_CTX;
      Mode     : in int;
      Callback : in Pointer);

   procedure RAND_seed (Buf : in Pointer; Num : in Integer);

   function RAND_status return Integer;

   procedure RAND_set_rand_method (Method : access Rand_Meth_St);

   --  Certificate

   function SSL_get_peer_certificate (SSL : in SSL_Handle) return X509;

   procedure X509_free (X509 : in Thin.X509);

   function X509_NAME_oneline
     (Name : in X509_Name;
      Buf  : in Pointer;
      Size : in int)
      return Cstr.chars_ptr;

   function X509_get_subject_name (X509 : in Thin.X509) return X509_Name;
   function X509_get_issuer_name (X509 : in Thin.X509) return X509_Name;

   procedure SSL_CTX_set_default_verify_paths (Ctx : in SSL_CTX);

private

   pragma Import (C, CRYPTO_num_locks, "CRYPTO_num_locks");
   pragma Import (C, CRYPTO_set_id_callback, "CRYPTO_set_id_callback");
   pragma Import (C, CRYPTO_set_locking_callback,
                    "CRYPTO_set_locking_callback");
   pragma Import (C, CRYPTO_get_locking_callback,
                    "CRYPTO_get_locking_callback");
   pragma Import (C, CRYPTO_set_dynlock_create_callback,
                    "CRYPTO_set_dynlock_create_callback");
   pragma Import (C, CRYPTO_set_dynlock_lock_callback,
                    "CRYPTO_set_dynlock_lock_callback");
   pragma Import (C, CRYPTO_set_dynlock_destroy_callback,
                    "CRYPTO_set_dynlock_destroy_callback");
   pragma Import (C, CRYPTO_get_dynlock_create_callback,
                    "CRYPTO_get_dynlock_create_callback");
   pragma Import (C, CRYPTO_get_dynlock_lock_callback,
                    "CRYPTO_get_dynlock_lock_callback");
   pragma Import (C, CRYPTO_get_dynlock_destroy_callback,
                    "CRYPTO_get_dynlock_destroy_callback");
   pragma Import (C, SSLeay, "SSLeay");
   pragma Import (C, RAND_seed, "RAND_seed");
   pragma Import (C, RAND_status, "RAND_status");
   pragma Import (C, RAND_set_rand_method, "RAND_set_rand_method");
   pragma Import (C, SSL_set_fd, "SSL_set_fd");
   pragma Import (C, SSL_accept, "SSL_accept");
   pragma Import (C, ERR_Remove_State, "ERR_remove_state");

   pragma Import (C, SSL_set_connect_state, "SSL_set_connect_state");
   pragma Import (C, SSL_set_accept_state, "SSL_set_accept_state");

   pragma Import (C, SSL_CTX_use_certificate_file,
                    "SSL_CTX_use_certificate_file");

   pragma Import (C, SSL_CTX_use_certificate_chain_file,
                    "SSL_CTX_use_certificate_chain_file");

   pragma Import (C, SSL_CTX_use_PrivateKey_file,
                    "SSL_CTX_use_PrivateKey_file");
   pragma Import (C, SSL_use_certificate_file, "SSL_use_certificate_file");
   pragma Import (C, SSL_use_PrivateKey_file, "SSL_use_PrivateKey_file");
   pragma Import (C, SSL_CTX_check_private_key, "SSL_CTX_check_private_key");
   pragma Import (C, SSL_read, "SSL_read");
   pragma Import (C, SSL_write, "SSL_write");
   pragma Import (C, SSL_peek, "SSL_peek");
   pragma Import (C, SSL_connect, "SSL_connect");
   pragma Import (C, SSL_CTX_set_quiet_shutdown, "SSL_CTX_set_quiet_shutdown");
   pragma Import (C, SSL_CTX_ctrl, "SSL_CTX_ctrl");
   pragma Import (C, SSL_pending, "SSL_pending");
   pragma Import (C, SSL_set_shutdown, "SSL_set_shutdown");
   pragma Import (C, SSL_shutdown, "SSL_shutdown");
   pragma Import (C, SSL_do_handshake, "SSL_do_handshake");
   pragma Import (C, SSL_renegotiate, "SSL_renegotiate");
   pragma Import (C, SSL_want, "SSL_want");
   pragma Import (C, SSL_set_read_ahead, "SSL_set_read_ahead");

   pragma Import (C, RSA_generate_key, "RSA_generate_key");
   pragma Import (C, SSL_use_RSAPrivateKey, "SSL_use_RSAPrivateKey");

   pragma Import (C, SSL_library_init, "SSL_library_init");

   pragma Import (C, SSL_load_error_strings, "SSL_load_error_strings");

   pragma Import (C, ERR_load_crypto_strings, "ERR_load_crypto_strings");
   pragma Import (C, ERR_load_ssl_strings, "ERR_load_SSL_strings");

   pragma Import (C, ERR_get_error, "ERR_get_error");
   pragma Import (C, ERR_error_string, "ERR_error_string");
   pragma Import (C, ERR_error_string_n, "ERR_error_string_n");

   pragma Import (C, CRYPTO_set_mem_functions, "CRYPTO_set_mem_functions");

   pragma Import (C, SSL_CTX_new, "SSL_CTX_new");
   pragma Import (C, SSL_CTX_free, "SSL_CTX_free");

   pragma Import (C, SSLv3_method, "SSLv3_method");
   pragma Import (C, SSLv3_server_method, "SSLv3_server_method");
   pragma Import (C, SSLv3_client_method, "SSLv3_client_method");
   pragma Import (C, SSLv2_method, "SSLv2_method");
   pragma Import (C, SSLv2_server_method, "SSLv2_server_method");
   pragma Import (C, SSLv2_client_method, "SSLv2_client_method");
   pragma Import (C, SSLv23_method, "SSLv23_method");
   pragma Import (C, SSLv23_server_method, "SSLv23_server_method");
   pragma Import (C, SSLv23_client_method, "SSLv23_client_method");
   pragma Import (C, TLSv1_method, "TLSv1_method");
   pragma Import (C, TLSv1_server_method, "TLSv1_server_method");
   pragma Import (C, TLSv1_client_method, "TLSv1_client_method");

   pragma Import (C, SSL_new, "SSL_new");
   pragma Import (C, SSL_free, "SSL_free");
   pragma Import (C, SSL_clear, "SSL_clear");
   pragma Import (C, SSL_get_error, "SSL_get_error");

   pragma Import (C, SSL_get_peer_certificate, "SSL_get_peer_certificate");
   pragma Import (C, X509_free, "X509_free");
   pragma Import (C, X509_get_subject_name, "X509_get_subject_name");
   pragma Import (C, X509_get_issuer_name, "X509_get_issuer_name");
   pragma Import (C, X509_NAME_oneline, "X509_NAME_oneline");
   pragma Import (C, SSL_CTX_set_verify, "SSL_CTX_set_verify");
   pragma Import (C, SSL_CTX_set_default_verify_paths,
                  "SSL_CTX_set_default_verify_paths");

end SSL.Thin;
