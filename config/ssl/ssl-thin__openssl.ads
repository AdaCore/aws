------------------------------------------------------------------------------
--                            Secure Sockets Layer                          --
--                                                                          --
--                     Copyright (C) 2000-2012, AdaCore                     --
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

with Interfaces.C.Strings;
with System;

package SSL.Thin is

   use Interfaces.C;

   package Cstr renames Interfaces.C.Strings;

   subtype Pointer is System.Address;
   Null_Pointer : constant Pointer := System.Null_Address;

   type Rand_Meth_St is record
      Seed, Bytes, Cleanup, Add, Pseudorand, Status : Pointer;
   end record;
   pragma Convention (C, Rand_Meth_St);

   type BIO_Method_St is record
      Kind : Integer;
      Name : Cstr.chars_ptr;
      Bwrite,
      Bread,
      Bputs,
      Bgets,
      Ctrl,
      Create,
      Destroy,
      Callback_Ctrl : Pointer;
   end record;
   pragma Convention (C, BIO_Method_St);

   type BIO_Method_Access is access all BIO_Method_St;

   type BIO_St;
   type BIO_Access is access all BIO_St;

   type BIO_St is record
      Method       : BIO_Method_Access;
      Callback     : Pointer;
      CB_Arg       : Cstr.chars_ptr; -- first argument for the callback
      Init         : int;
      Shutdown     : int;
      Flags        : int;  -- extra Storage
      Retry_Reason : int;
      Num          : int;
      Ptr          : Pointer;
      Next_BIO     : BIO_Access; -- used by filter BIOs
      Prev_BIO     : BIO_Access; -- used by filter BIOs
      References   : int;
      Num_Read     : unsigned_long;
      Num_Write    : unsigned_long;
      --  CRYPTO_EX_DATA ex_data; -- Do not need to translete it
   end record;

   subtype SSL_Method     is Pointer;
   subtype SSL_CTX        is Pointer;
   subtype SSL_Handle     is Pointer;
   subtype RSA            is Pointer;
   subtype X509           is Pointer;
   subtype X509_Name      is Pointer;
   subtype X509_STORE_CTX is Pointer;

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

   BIO_C_SET_CONNECT                 : constant := 100;
   BIO_C_DO_STATE_MACHINE            : constant := 101;
   BIO_C_SET_NBIO                    : constant := 102;
   BIO_C_SET_PROXY_PARAM             : constant := 103;
   BIO_C_SET_FD                      : constant := 104;
   BIO_C_GET_FD                      : constant := 105;
   BIO_C_SET_FILE_PTR                : constant := 106;
   BIO_C_GET_FILE_PTR                : constant := 107;
   BIO_C_SET_FILENAME                : constant := 108;
   BIO_C_SET_SSL                     : constant := 109;
   BIO_C_GET_SSL                     : constant := 110;
   BIO_C_SET_MD                      : constant := 111;
   BIO_C_GET_MD                      : constant := 112;
   BIO_C_GET_CIPHER_STATUS           : constant := 113;
   BIO_C_SET_BUF_MEM                 : constant := 114;
   BIO_C_GET_BUF_MEM_PTR             : constant := 115;
   BIO_C_GET_BUFF_NUM_LINES          : constant := 116;
   BIO_C_SET_BUFF_SIZE               : constant := 117;
   BIO_C_SET_ACCEPT                  : constant := 118;
   BIO_C_SSL_MODE                    : constant := 119;
   BIO_C_GET_MD_CTX                  : constant := 120;
   BIO_C_GET_PROXY_PARAM             : constant := 121;
   BIO_C_SET_BUFF_READ_DATA          : constant := 122; -- data to read first
   BIO_C_GET_CONNECT                 : constant := 123;
   BIO_C_GET_ACCEPT                  : constant := 124;
   BIO_C_SET_SSL_RENEGOTIATE_BYTES   : constant := 125;
   BIO_C_GET_SSL_NUM_RENEGOTIATES    : constant := 126;
   BIO_C_SET_SSL_RENEGOTIATE_TIMEOUT : constant := 127;
   BIO_C_FILE_SEEK                   : constant := 128;
   BIO_C_GET_CIPHER_CTX              : constant := 129;
   BIO_C_SET_BUF_MEM_EOF_RETURN      : constant := 130;
   BIO_C_SET_BIND_MODE               : constant := 131;
   BIO_C_GET_BIND_MODE               : constant := 132;
   BIO_C_FILE_TELL                   : constant := 133;
   BIO_C_GET_SOCKS                   : constant := 134;
   BIO_C_SET_SOCKS                   : constant := 135;

   BIO_C_SET_WRITE_BUF_SIZE  : constant := 136; -- for BIO_s_bio
   BIO_C_GET_WRITE_BUF_SIZE  : constant := 137;
   BIO_C_MAKE_BIO_PAIR       : constant := 138;
   BIO_C_DESTROY_BIO_PAIR    : constant := 139;
   BIO_C_GET_WRITE_GUARANTEE : constant := 140;
   BIO_C_GET_READ_REQUEST    : constant := 141;
   BIO_C_SHUTDOWN_WR         : constant := 142;
   BIO_C_NREAD0              : constant := 143;
   BIO_C_NREAD               : constant := 144;
   BIO_C_NWRITE0             : constant := 145;
   BIO_C_NWRITE              : constant := 146;
   BIO_C_RESET_READ_REQUEST  : constant := 147;

   -------------------------------------------------------------------------
   -- These are used in the following macros and are passed to BIO_ctrl() --
   -------------------------------------------------------------------------

   BIO_CTRL_RESET        : constant := 1; -- opt - rewind/zero etc
   BIO_CTRL_EOF          : constant := 2; -- opt - are we at the eof
   BIO_CTRL_INFO         : constant := 3; -- opt - extra tit-bits
   BIO_CTRL_SET          : constant := 4; -- man - set the 'IO' type
   BIO_CTRL_GET          : constant := 5; -- man - get the 'IO' type
   BIO_CTRL_PUSH         : constant := 6; -- opt intern, used to signify change
   BIO_CTRL_POP          : constant := 7; -- opt intern, used to signify change
   BIO_CTRL_GET_CLOSE    : constant := 8; -- man - set the 'close' on free
   BIO_CTRL_SET_CLOSE    : constant := 9; -- man - set the 'close' on free
   BIO_CTRL_PENDING      : constant := 10; -- opt - is their more data buffered
   BIO_CTRL_FLUSH        : constant := 11; -- opt - 'flush' buffered output
   BIO_CTRL_DUP          : constant := 12; -- man - extra stuff for 'duped' BIO
   BIO_CTRL_WPENDING     : constant := 13; -- opt numb of bytes still to write
   BIO_CTRL_SET_CALLBACK : constant := 14; -- opt - set callback function
   BIO_CTRL_GET_CALLBACK : constant := 15; -- opt - set callback function
   BIO_CTRL_SET_FILENAME : constant := 30; -- BIO_s_file special

   BIO_NOCLOSE : constant := 0;
   BIO_CLOSE   : constant := 1;

   BIO_FLAGS_READ       : constant := 1;
   BIO_FLAGS_WRITE      : constant := 2;
   BIO_FLAGS_IO_SPECIAL : constant := 4;
   BIO_FLAGS_RWS        : constant
     := BIO_FLAGS_READ + BIO_FLAGS_WRITE + BIO_FLAGS_IO_SPECIAL;
   BIO_FLAGS_SHOULD_RETRY  : constant := 8;

   RSA_3  : constant := 3;
   RSA_F4 : constant := 16#10001#;

   ASN1_OBJECT_FLAG_DYNAMIC         : constant := 16#01#;
   ASN1_OBJECT_FLAG_CRITICAL        : constant := 16#02#;
   ASN1_OBJECT_FLAG_DYNAMIC_STRINGS : constant := 16#04#;
   ASN1_OBJECT_FLAG_DYNAMIC_DATA    : constant := 16#08#;
   ASN1_STRING_FLAG_BITS_LEFT       : constant := 16#08#;
   ASN1_STRING_FLAG_NDEF            : constant := 16#010#;
   ASN1_STRING_FLAG_CONT            : constant := 16#020#;
   ASN1_STRING_FLAG_MSTRING         : constant := 16#040#;
   ASN1_LONG_UNDEF                  : constant := 16#7fffffff#;
   ASN1_STRFLGS_ESC_2253            : constant := 1;
   ASN1_STRFLGS_ESC_CTRL            : constant := 2;
   ASN1_STRFLGS_ESC_MSB             : constant := 4;
   ASN1_STRFLGS_ESC_QUOTE           : constant := 8;
   CHARTYPE_PRINTABLESTRING         : constant := 16#10#;
   CHARTYPE_FIRST_ESC_2253          : constant := 16#20#;
   CHARTYPE_LAST_ESC_2253           : constant := 16#40#;
   ASN1_STRFLGS_UTF8_CONVERT        : constant := 16#10#;
   ASN1_STRFLGS_IGNORE_TYPE         : constant := 16#20#;
   ASN1_STRFLGS_SHOW_TYPE           : constant := 16#40#;
   ASN1_STRFLGS_DUMP_ALL            : constant := 16#80#;
   ASN1_STRFLGS_DUMP_UNKNOWN        : constant := 16#100#;
   ASN1_STRFLGS_DUMP_DER            : constant := 16#200#;

   ASN1_STRFLGS_RFC2253 : constant := ASN1_STRFLGS_ESC_2253
                                    + ASN1_STRFLGS_ESC_CTRL
                                    + ASN1_STRFLGS_ESC_MSB
                                    + ASN1_STRFLGS_UTF8_CONVERT
                                    + ASN1_STRFLGS_DUMP_UNKNOWN
                                    + ASN1_STRFLGS_DUMP_DER;

   XN_FLAG_SEP_MASK            : constant := 16#f# * 2 ** 16;
   XN_FLAG_COMPAT              : constant := 0;
   XN_FLAG_SEP_COMMA_PLUS      : constant := 1 * 2 ** 16;
   XN_FLAG_SEP_CPLUS_SPC       : constant := 2 * 2 ** 16;
   XN_FLAG_SEP_SPLUS_SPC       : constant := 3 * 2 ** 16;
   XN_FLAG_SEP_MULTILINE       : constant := 4 * 2 ** 16;
   XN_FLAG_DN_REV              : constant := 2 ** 20;
   XN_FLAG_FN_MASK             : constant := 3 * 2 ** 21;
   XN_FLAG_FN_SN               : constant := 0;
   XN_FLAG_FN_LN               : constant := 1 * 2 ** 21;
   XN_FLAG_FN_OID              : constant := 2 * 2 ** 21;
   XN_FLAG_FN_NONE             : constant := 3 * 2 ** 21;
   XN_FLAG_SPC_EQ              : constant := 2 ** 23;
   XN_FLAG_DUMP_UNKNOWN_FIELDS : constant := 2 ** 24;
   XN_FLAG_FN_ALIGN            : constant := 2 ** 25;

   XN_FLAG_RFC2253 : constant := ASN1_STRFLGS_RFC2253
                               + XN_FLAG_SEP_COMMA_PLUS
                               + XN_FLAG_DN_REV
                               + XN_FLAG_FN_SN
                               + XN_FLAG_DUMP_UNKNOWN_FIELDS;

   XN_FLAG_RFC2253_DIRECT : constant := ASN1_STRFLGS_RFC2253
                                      + XN_FLAG_SEP_COMMA_PLUS
                                      + XN_FLAG_FN_SN
                                      + XN_FLAG_DUMP_UNKNOWN_FIELDS;
   --  XN_FLAG_RFC2253 macros has a XN_FLAG_DN_REV flag (reverse attributes),
   --  but GNUTLS has a direct attributes order and I do not see such reverse
   --  in RFC2253.

   type ASN1_STRING is record
      length : int;
      stype  : int;
      data   : Interfaces.C.Strings.chars_ptr;
   end record;
   pragma Convention (C, ASN1_STRING);

   subtype ASN1_UTCTIME is ASN1_STRING;

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

   procedure CRYPTO_set_dynlock_create_callback (Create_Function : Pointer);
   procedure CRYPTO_set_dynlock_lock_callback (Lock_Function : Pointer);
   procedure CRYPTO_set_dynlock_destroy_callback
     (Destroy_Function : Pointer);

   function CRYPTO_get_dynlock_create_callback  return Pointer;
   function CRYPTO_get_dynlock_lock_callback    return Pointer;
   function CRYPTO_get_dynlock_destroy_callback return Pointer;

   ------------------------------------------
   -- OpenSSL version information routines --
   ------------------------------------------

   SSLEAY_VERSION  : constant := 0;
   SSLEAY_CFLAGS   : constant := 2;
   SSLEAY_BUILT_ON : constant := 3;
   SSLEAY_PLATFORM : constant := 4;
   SSLEAY_DIR      : constant := 5;

   function SSLeay return long;
   --  Returns OpenSSL numeric release version identifier

   function SSLeay_version_info (T : int) return Cstr.chars_ptr;
   --  Returns version information line

   -------------------------------
   -- Context control routines  --
   -------------------------------

   function SSLv3_method         return SSL_Method;
   function SSLv3_server_method  return SSL_Method;
   function SSLv3_client_method  return SSL_Method;
   function SSLv23_method        return SSL_Method;
   function SSLv23_server_method return SSL_Method;
   function SSLv23_client_method return SSL_Method;
   function TLSv1_method         return SSL_Method;
   function TLSv1_server_method  return SSL_Method;
   function TLSv1_client_method  return SSL_Method;

   function SSL_CTX_new (Meth : SSL_Method) return SSL_CTX;

   procedure SSL_CTX_free (P1 : SSL_CTX);

   procedure SSL_CTX_set_quiet_shutdown (Ctx : SSL_CTX; Mode : int);

   function SSL_CTX_ctrl
     (Ctx  : SSL_CTX;
      Cmd  : int;
      Larg : int;
      Parg : Pointer) return int;

   -------------------------------------
   -- Library initialization routines --
   -------------------------------------

   function CRYPTO_set_mem_functions
     (M : System.Address;
      R : System.Address;
      F : System.Address) return int;

   procedure SSL_library_init;

   procedure SSL_load_error_strings;

   procedure ERR_load_crypto_strings;

   procedure ERR_load_ssl_strings;

   --------------------------------
   -- Error information routines --
   --------------------------------

   function ERR_get_error return Error_Code;

   function ERR_error_string
     (Code : Error_Code; Buffer : Cstr.chars_ptr) return Cstr.chars_ptr;

   procedure ERR_error_string_n
     (Code : Error_Code; Buffer : Cstr.chars_ptr; Len : size_t);

   procedure ERR_remove_state (pid : int := 0);

   -----------------------------------------
   -- Connection handler control routines --
   -----------------------------------------

   function SSL_new (Ctx : SSL_CTX) return SSL_Handle;

   procedure SSL_free (SSL : SSL_Handle);

   function SSL_clear (SSL : SSL_Handle) return int;

   function SSL_set_fd (S : SSL_Handle; Fd : int) return int;

   procedure SSL_set_bio (SSL : SSL_Handle; RBIO, WBIO : BIO_Access);

   function SSL_get_rbio (SSL : SSL_Handle) return BIO_Access;
   function SSL_get_wbio (SSL : SSL_Handle) return BIO_Access;

   procedure SSL_set_read_ahead (S : SSL_Handle; Yes : int);

   function SSL_connect (SSL : SSL_Handle) return int;

   function SSL_accept (SSL : SSL_Handle) return int;

   procedure SSL_set_connect_state (SSL : SSL_Handle);

   procedure SSL_set_accept_state (SSL : SSL_Handle);

   function SSL_renegotiate (SSL : SSL_Handle) return int;

   function SSL_do_handshake (SSL : SSL_Handle) return int;

   function SSL_want (S : SSL_Handle) return int;

   function SSL_read
     (SSL : SSL_Handle; Buf : Pointer; Num : int) return int;

   function SSL_peek
     (SSL : SSL_Handle; Buf : Pointer; Num : int) return int;

   function SSL_write
     (SSL : SSL_Handle; Buf : Pointer; Num : int) return int;

   function SSL_pending (S : SSL_Handle) return int;

   function SSL_get_error (SSL : SSL_Handle; ret : int) return int;

   function SSL_shutdown (SSL : SSL_Handle) return int;

   procedure SSL_set_shutdown (SSL : SSL_Handle; Mode : int);

   function SSL_get_SSL_CTX (SSL : SSL_Handle) return SSL_CTX;

   ----------------------
   --  Crypto routines --
   ----------------------

   type Generate_Key_Callback is access
     procedure (I1, I2 : Integer; Param : Pointer);
   pragma Convention (C, Generate_Key_Callback);

   function RSA_generate_key
     (Bits     : int;
      E        : unsigned;
      Callback : Generate_Key_Callback;
      Cb_Arg   : Pointer) return RSA;

   function SSL_use_RSAPrivateKey
     (SSL : SSL_Handle; Private_Key : RSA) return int;

   function SSL_CTX_use_PrivateKey_file
     (Ctx : SSL_CTX; File : char_array; C_Type : int) return int;

   function SSL_use_PrivateKey_file
     (SSL : SSL_Handle; File : char_array; C_Type : int) return int;

   function SSL_CTX_use_certificate_file
     (Ctx : SSL_CTX; File : char_array; C_Type : int) return int;

   function SSL_CTX_use_certificate_chain_file
     (Ctx : SSL_CTX; File : char_array) return int;

   function SSL_use_certificate_file
     (SSL : SSL_Handle; File : char_array; C_Type : int) return int;

   function SSL_CTX_check_private_key (Ctx : SSL_CTX) return int;

   procedure SSL_CTX_set_verify
     (Ctx : SSL_CTX; Mode : int; Callback : Pointer);

   function SSL_CTX_get_verify_mode (Ctx : SSL_CTX) return unsigned;

   procedure RAND_seed (Buf : Pointer; Num : Integer);

   function RAND_status return Integer;

   procedure RAND_set_rand_method (Method : access Rand_Meth_St);

   --  Connection data

   function SSL_CTX_get_ex_new_index
     (Args                          : long;
      Argp                          : Pointer;
      New_Func, Dup_Func, Free_Func : Pointer) return int;

   function SSL_CTX_set_ex_data
     (SSL : SSL_CTX; Idx : int; Arg : Pointer) return int;

   function SSL_CTX_get_ex_data (SSL : SSL_CTX; Idx : int) return Pointer;

   --  Certificate

   function SSL_get_peer_certificate (SSL : SSL_Handle) return X509;

   procedure X509_free (X509 : Thin.X509);

   function X509_NAME_oneline
     (Name : X509_Name;
      Buf  : Pointer;
      Size : int) return Cstr.chars_ptr;

   function X509_NAME_print_ex
     (Output : BIO_Access;
      Name   : X509_Name;
      Indent : int;
      flags  : unsigned_long) return int;

   function X509_get_subject_name (X509 : Thin.X509) return X509_Name;
   function X509_get_issuer_name (X509 : Thin.X509) return X509_Name;
   function X509_get_notAfter
     (X509 : Thin.X509) return access constant ASN1_STRING;
   function X509_get_notBefore
     (X509 : Thin.X509) return access constant ASN1_STRING;

   procedure SSL_CTX_set_default_verify_paths (Ctx : SSL_CTX);

   function SSL_get_ex_data_X509_STORE_CTX_idx return int;

   function X509_STORE_CTX_get_ex_data
     (Ctx : X509_STORE_CTX; Idx : int) return SSL_Handle;

   function X509_STORE_CTX_get_current_cert (Ctx : X509_STORE_CTX) return X509;

   -------------------
   --  BIO routines --
   -------------------

   function BIO_s_socket return BIO_Method_Access;
   function BIO_s_mem return BIO_Method_Access;

   function BIO_new (Method : BIO_Method_St) return BIO_Access;
   function BIO_new (Method : BIO_Method_Access) return BIO_Access;

   function BIO_int_ctrl
     (Bp   : BIO_Access;
      Cmd  : int;
      Larg : long;
      Iarg : int) return long;

   procedure BIO_int_ctrl
     (Bp : BIO_Access; Cmd : int; Larg : long; Iarg : int);
   --  BIO_set_fd(b,fd,c) BIO_int_ctrl(b,BIO_C_SET_FD,c,fd)

   function BIO_ctrl
     (Bp   : BIO_Access;
      Cmd  : int;
      Larg : long    := 0;
      Parg : Pointer := Null_Pointer) return long;
   --  BIO_pending(b) = (int)BIO_ctrl(b,BIO_CTRL_PENDING,0,NULL)

   procedure BIO_ctrl
     (Bp : BIO_Access; Cmd : int; Larg : long; Parg : Pointer);
   --  BIO_get_fd(b,c) = BIO_ctrl(b,BIO_C_GET_FD,0,(char *)c)
   --  BIO_set_mem_eof_return(b,v)
   --  = BIO_ctrl(b,BIO_C_SET_BUF_MEM_EOF_RETURN,v,NULL)

   function BIO_read
     (BIO : BIO_Access; Data : Pointer; Len : int) return int;

   function BIO_write
     (BIO : BIO_Access; Data : Pointer; Len : int) return int;

   function BIO_new_bio_pair
     (bio1 : access BIO_Access; writebuf1 : size_t;
      bio2 : access BIO_Access; writebuf2 : size_t) return int;

   function BIO_nread0 (BIO : BIO_Access; Buf : Pointer) return int;
   function BIO_nread
     (BIO : BIO_Access; Buf : Pointer; Num : int) return int;
   --  Buf is the address of the buffer address

   function BIO_nwrite0 (BIO : BIO_Access; Buf : Pointer) return int;
   function BIO_nwrite
     (BIO : BIO_Access; Buf : Pointer; Num : int) return int;
   --  Buf is the address of the buffer address

   function BIO_free (BIO : BIO_Access) return int;
   procedure BIO_free (BIO : BIO_Access);

   SSL_SESS_CACHE_OFF                : constant := 0;
   SSL_SESS_CACHE_CLIENT             : constant := 1;
   SSL_SESS_CACHE_SERVER             : constant := 2;
   SSL_SESS_CACHE_BOTH               : constant :=
     SSL_SESS_CACHE_CLIENT + SSL_SESS_CACHE_SERVER;
   SSL_SESS_CACHE_NO_AUTO_CLEAR      : constant := 16#0080#;
   SSL_SESS_CACHE_NO_INTERNAL_LOOKUP : constant := 16#0100#;
   SSL_SESS_CACHE_NO_INTERNAL_STORE  : constant := 16#0200#;
   SSL_SESS_CACHE_NO_INTERNAL        : constant :=
     SSL_SESS_CACHE_NO_INTERNAL_LOOKUP + SSL_SESS_CACHE_NO_INTERNAL_STORE;

   function SSL_CTX_set_session_cache_mode
     (Ctx : SSL_CTX; Mode : long) return long;

   procedure SSL_CTX_flush_sessions (Ctx : SSL_CTX; Tm : long);

   type SSL_Session is new Pointer;

   function SSL_set_session
     (SSL : SSL_Handle; session : SSL_Session) return int;

   function SSL_get_session (SSL : SSL_Handle) return SSL_Session;
   function SSL_get1_session (SSL : SSL_Handle) return SSL_Session;

private

   pragma Import (C, BIO_s_socket, "BIO_s_socket");
   pragma Import (C, BIO_s_mem, "BIO_s_mem");
   pragma Import (C, BIO_new, "BIO_new");
   pragma Import (C, BIO_read, "BIO_read");
   pragma Import (C, BIO_write, "BIO_write");
   pragma Import (C, BIO_int_ctrl, "BIO_int_ctrl");
   pragma Import (C, BIO_ctrl, "BIO_ctrl");
   pragma Import (C, BIO_new_bio_pair, "BIO_new_bio_pair");
   pragma Import (C, BIO_free, "BIO_free");
   pragma Import (C, BIO_nread0, "BIO_nread0");
   pragma Import (C, BIO_nread, "BIO_nread");
   pragma Import (C, BIO_nwrite0, "BIO_nwrite0");
   pragma Import (C, BIO_nwrite, "BIO_nwrite");
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
   pragma Import (C, SSLeay_version_info, "SSLeay_version");
   pragma Import (C, RAND_seed, "RAND_seed");
   pragma Import (C, RAND_status, "RAND_status");
   pragma Import (C, RAND_set_rand_method, "RAND_set_rand_method");
   pragma Import (C, SSL_set_fd, "SSL_set_fd");
   pragma Import (C, SSL_set_bio, "SSL_set_bio");
   pragma Import (C, SSL_get_rbio, "SSL_get_rbio");
   pragma Import (C, SSL_get_wbio, "SSL_get_wbio");
   pragma Import (C, SSL_accept, "SSL_accept");
   pragma Import (C, ERR_remove_state, "ERR_remove_state");

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
   pragma Import (C, SSL_CTX_set_session_cache_mode,
                    "SSL_CTX_set_session_cache_mode");
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
   pragma Import (C, X509_get_notAfter, "__aws_X509_get_notAfter");
   pragma Import (C, X509_get_notBefore, "__aws_X509_get_notBefore");
   pragma Import (C, X509_NAME_oneline, "X509_NAME_oneline");
   pragma Import (C, X509_NAME_print_ex, "X509_NAME_print_ex");
   pragma Import (C, SSL_get_ex_data_X509_STORE_CTX_idx,
                  "SSL_get_ex_data_X509_STORE_CTX_idx");
   pragma Import (C, X509_STORE_CTX_get_ex_data, "X509_STORE_CTX_get_ex_data");
   pragma Import (C, X509_STORE_CTX_get_current_cert,
                  "X509_STORE_CTX_get_current_cert");
   pragma Import (C, SSL_CTX_set_verify, "SSL_CTX_set_verify");
   pragma Import (C, SSL_CTX_get_verify_mode, "SSL_CTX_get_verify_mode");
   pragma Import (C, SSL_CTX_set_default_verify_paths,
                  "SSL_CTX_set_default_verify_paths");
   pragma Import (C, SSL_set_session, "SSL_set_session");
   pragma Import (C, SSL_get_session, "SSL_get_session");
   pragma Import (C, SSL_get1_session, "SSL_get1_session");
   pragma Import (C, SSL_CTX_flush_sessions, "SSL_CTX_flush_sessions");
   pragma Import (C, SSL_CTX_get_ex_new_index, "SSL_CTX_get_ex_new_index");
   pragma Import (C, SSL_CTX_set_ex_data, "SSL_CTX_set_ex_data");
   pragma Import (C, SSL_CTX_get_ex_data, "SSL_CTX_get_ex_data");
   pragma Import (C, SSL_get_SSL_CTX, "SSL_get_SSL_CTX");

end SSL.Thin;
