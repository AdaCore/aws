------------------------------------------------------------------------------
--                            Secure Sockets Layer                          --
--                                                                          --
--                     Copyright (C) 2000-2013, AdaCore                     --
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

with Interfaces.C.Strings;
with System;

package SSL.Thin is

   use Interfaces.C;

   package Cstr renames Interfaces.C.Strings;

   subtype Pointer is System.Address;
   Null_Pointer : constant Pointer := System.Null_Address;

   type Rand_Meth_St is record
      Seed, Bytes, Cleanup, Add, Pseudorand, Status : Pointer;
   end record with Convention => C;

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
   end record with Convention => C;

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
   subtype X509_STORE     is Pointer;

   type X509_LOOKUP        is new Pointer;
   type X509_LOOKUP_METHOD is new Pointer;
   type X509_VERIFY_PARAM  is new Pointer;

   type BIGNUM             is new Pointer;

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

   X509_V_FLAG_CB_ISSUER_CHECK      : constant := 16#1#;
   X509_V_FLAG_USE_CHECK_TIME       : constant := 16#2#;
   X509_V_FLAG_CRL_CHECK            : constant := 16#4#;
   X509_V_FLAG_CRL_CHECK_ALL        : constant := 16#8#;
   X509_V_FLAG_IGNORE_CRITICAL      : constant := 16#10#;
   X509_V_FLAG_X509_STRICT          : constant := 16#20#;
   X509_V_FLAG_ALLOW_PROXY_CERTS    : constant := 16#40#;
   X509_V_FLAG_POLICY_CHECK         : constant := 16#80#;
   X509_V_FLAG_EXPLICIT_POLICY      : constant := 16#100#;
   X509_V_FLAG_INHIBIT_ANY          : constant := 16#200#;
   X509_V_FLAG_INHIBIT_MAP          : constant := 16#400#;
   X509_V_FLAG_NOTIFY_POLICY        : constant := 16#800#;
   X509_V_FLAG_EXTENDED_CRL_SUPPORT : constant := 16#1000#;
   X509_V_FLAG_USE_DELTAS           : constant := 16#2000#;
   X509_V_FLAG_CHECK_SS_SIGNATURE   : constant := 16#4000#;

   X509_FILETYPE_PEM     : constant := 1;
   X509_FILETYPE_ASN1    : constant := 2;
   X509_FILETYPE_DEFAULT : constant := 3;

   X509_V_OK                                     : constant := 0;
   X509_V_ERR_UNABLE_TO_GET_ISSUER_CERT          : constant := 2;
   X509_V_ERR_UNABLE_TO_GET_CRL                  : constant := 3;
   X509_V_ERR_UNABLE_TO_DECRYPT_CERT_SIGNATURE   : constant := 4;
   X509_V_ERR_UNABLE_TO_DECRYPT_CRL_SIGNATURE    : constant := 5;
   X509_V_ERR_UNABLE_TO_DECODE_ISSUER_PUBLIC_KEY : constant := 6;
   X509_V_ERR_CERT_SIGNATURE_FAILURE             : constant := 7;
   X509_V_ERR_CRL_SIGNATURE_FAILURE              : constant := 8;
   X509_V_ERR_CERT_NOT_YET_VALID                 : constant := 9;
   X509_V_ERR_CERT_HAS_EXPIRED                   : constant := 10;
   X509_V_ERR_CRL_NOT_YET_VALID                  : constant := 11;
   X509_V_ERR_CRL_HAS_EXPIRED                    : constant := 12;
   X509_V_ERR_ERROR_IN_CERT_NOT_BEFORE_FIELD     : constant := 13;
   X509_V_ERR_ERROR_IN_CERT_NOT_AFTER_FIELD      : constant := 14;
   X509_V_ERR_ERROR_IN_CRL_LAST_UPDATE_FIELD     : constant := 15;
   X509_V_ERR_ERROR_IN_CRL_NEXT_UPDATE_FIELD     : constant := 16;
   X509_V_ERR_OUT_OF_MEM                         : constant := 17;
   X509_V_ERR_DEPTH_ZERO_SELF_SIGNED_CERT        : constant := 18;
   X509_V_ERR_SELF_SIGNED_CERT_IN_CHAIN          : constant := 19;
   X509_V_ERR_UNABLE_TO_GET_ISSUER_CERT_LOCALLY  : constant := 20;
   X509_V_ERR_UNABLE_TO_VERIFY_LEAF_SIGNATURE    : constant := 21;
   X509_V_ERR_CERT_CHAIN_TOO_LONG                : constant := 22;
   X509_V_ERR_CERT_REVOKED                       : constant := 23;
   X509_V_ERR_INVALID_CA                         : constant := 24;
   X509_V_ERR_PATH_LENGTH_EXCEEDED               : constant := 25;
   X509_V_ERR_INVALID_PURPOSE                    : constant := 26;
   X509_V_ERR_CERT_UNTRUSTED                     : constant := 27;
   X509_V_ERR_CERT_REJECTED                      : constant := 28;
   X509_V_ERR_SUBJECT_ISSUER_MISMATCH            : constant := 29;
   X509_V_ERR_AKID_SKID_MISMATCH                 : constant := 30;
   X509_V_ERR_AKID_ISSUER_SERIAL_MISMATCH        : constant := 31;
   X509_V_ERR_KEYUSAGE_NO_CERTSIGN               : constant := 32;
   X509_V_ERR_APPLICATION_VERIFICATION           : constant := 50;

   type ASN1_STRING is record
      length : int;
      stype  : int;
      data   : Interfaces.C.Strings.chars_ptr;
   end record with Convention => C;

   subtype ASN1_UTCTIME is ASN1_STRING;
   subtype ASN1_INTEGER is ASN1_STRING;

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

   function  CRYPTO_num_locks return Natural
     with Import, Convention => C, Link_Name => "CRYPTO_num_locks";

   procedure CRYPTO_set_id_callback (Id_Function : Pointer)
     with Import, Convention => C, Link_Name => "CRYPTO_set_id_callback";

   procedure CRYPTO_set_locking_callback (Locking_Function : Pointer)
     with Import, Convention => C, Link_Name => "CRYPTO_set_locking_callback";

   function CRYPTO_get_locking_callback return Pointer
     with Import, Convention => C, Link_Name => "CRYPTO_get_locking_callback";

   procedure CRYPTO_set_dynlock_create_callback (Create_Function : Pointer)
     with Import, Convention => C,
          Link_Name => "CRYPTO_set_dynlock_create_callback";

   procedure CRYPTO_set_dynlock_lock_callback (Lock_Function : Pointer)
     with Import, Convention => C,
          Link_Name => "CRYPTO_set_dynlock_lock_callback";

   procedure CRYPTO_set_dynlock_destroy_callback
     (Destroy_Function : Pointer)
     with Import, Convention => C,
          Link_Name => "CRYPTO_set_dynlock_destroy_callback";

   function CRYPTO_get_dynlock_create_callback  return Pointer
     with Import, Convention => C,
          Link_Name => "CRYPTO_get_dynlock_create_callback";

   function CRYPTO_get_dynlock_lock_callback    return Pointer
     with Import, Convention => C,
          Link_Name => "CRYPTO_get_dynlock_lock_callback";

   function CRYPTO_get_dynlock_destroy_callback return Pointer
     with Import, Convention => C,
          Link_Name => "CRYPTO_get_dynlock_destroy_callback";

   ------------------------------------------
   -- OpenSSL version information routines --
   ------------------------------------------

   SSLEAY_VERSION  : constant := 0;
   SSLEAY_CFLAGS   : constant := 2;
   SSLEAY_BUILT_ON : constant := 3;
   SSLEAY_PLATFORM : constant := 4;
   SSLEAY_DIR      : constant := 5;

   function SSLeay return long
     with Import, Convention => C, Link_Name => "SSLeay";
   --  Returns OpenSSL numeric release version identifier

   function SSLeay_version_info (T : int) return Cstr.chars_ptr
     with Import, Convention => C, Link_Name => "SSLeay_version";
   --  Returns version information line

   -------------------------------
   -- Context control routines  --
   -------------------------------

   function SSLv3_method         return SSL_Method
     with Import, Convention => C, Link_Name => "SSLv3_method";

   function SSLv3_server_method  return SSL_Method
     with Import, Convention => C, Link_Name => "SSLv3_server_method";

   function SSLv3_client_method  return SSL_Method
     with Import, Convention => C, Link_Name => "SSLv3_client_method";

   function SSLv23_method        return SSL_Method
     with Import, Convention => C, Link_Name => "SSLv23_method";

   function SSLv23_server_method return SSL_Method
     with Import, Convention => C, Link_Name => "SSLv23_server_method";

   function SSLv23_client_method return SSL_Method
     with Import, Convention => C, Link_Name => "SSLv23_client_method";

   function TLSv1_method         return SSL_Method
     with Import, Convention => C, Link_Name => "TLSv1_method";

   function TLSv1_server_method  return SSL_Method
     with Import, Convention => C, Link_Name => "TLSv1_server_method";

   function TLSv1_client_method  return SSL_Method
     with Import, Convention => C, Link_Name => "TLSv1_client_method";

   function SSL_CTX_new (Meth : SSL_Method) return SSL_CTX
     with Import, Convention => C, Link_Name => "SSL_CTX_new";

   procedure SSL_CTX_free (P1 : SSL_CTX)
     with Import, Convention => C, Link_Name => "SSL_CTX_free";

   procedure SSL_CTX_set_quiet_shutdown (Ctx : SSL_CTX; Mode : int)
     with Import, Convention => C, Link_Name => "SSL_CTX_set_quiet_shutdown";

   function SSL_CTX_ctrl
     (Ctx  : SSL_CTX; Cmd : int; Larg : int; Parg : Pointer) return int
     with Import, Convention => C, Link_Name => "SSL_CTX_ctrl";

   -------------------------------------
   -- Library initialization routines --
   -------------------------------------

   function CRYPTO_set_mem_functions (M, R, F : System.Address) return int
     with Import, Convention => C, Link_Name => "CRYPTO_set_mem_functions";

   procedure SSL_library_init
     with Import, Convention => C, Link_Name => "SSL_library_init";

   procedure SSL_load_error_strings
     with Import, Convention => C, Link_Name => "SSL_load_error_strings";

   procedure ERR_load_crypto_strings
     with Import, Convention => C, Link_Name => "ERR_load_crypto_strings";

   procedure ERR_load_ssl_strings
     with Import, Convention => C, Link_Name => "ERR_load_ssl_strings";

   --------------------------------
   -- Error information routines --
   --------------------------------

   function ERR_get_error return Error_Code
     with Import, Convention => C, Link_Name => "ERR_get_error";

   function ERR_error_string
     (Code : Error_Code; Buffer : Cstr.chars_ptr) return Cstr.chars_ptr
     with Import, Convention => C, Link_Name => "ERR_error_string";

   procedure ERR_error_string_n
     (Code : Error_Code; Buffer : Cstr.chars_ptr; Len : size_t)
     with Import, Convention => C, Link_Name => "ERR_error_string_n";

   procedure ERR_remove_state (pid : int := 0)
     with Import, Convention => C, Link_Name => "ERR_remove_state";

   -----------------------------------------
   -- Connection handler control routines --
   -----------------------------------------

   function SSL_new (Ctx : SSL_CTX) return SSL_Handle
     with Import, Convention => C, Link_Name => "SSL_new";

   procedure SSL_free (SSL : SSL_Handle)
     with Import, Convention => C, Link_Name => "SSL_free";

   function SSL_clear (SSL : SSL_Handle) return int
     with Import, Convention => C, Link_Name => "SSL_clear";

   function SSL_set_fd (S : SSL_Handle; Fd : int) return int
     with Import, Convention => C, Link_Name => "SSL_set_fd";

   procedure SSL_set_bio (SSL : SSL_Handle; RBIO, WBIO : BIO_Access)
     with Import, Convention => C, Link_Name => "SSL_set_bio";

   function SSL_get_rbio (SSL : SSL_Handle) return BIO_Access
     with Import, Convention => C, Link_Name => "SSL_get_rbio";

   function SSL_get_wbio (SSL : SSL_Handle) return BIO_Access
     with Import, Convention => C, Link_Name => "SSL_get_wbio";

   procedure SSL_set_read_ahead (S : SSL_Handle; Yes : int)
     with Import, Convention => C, Link_Name => "SSL_set_read_ahead";

   function SSL_connect (SSL : SSL_Handle) return int
     with Import, Convention => C, Link_Name => "SSL_connect";

   function SSL_accept (SSL : SSL_Handle) return int
     with Import, Convention => C, Link_Name => "SSL_accept";

   procedure SSL_set_connect_state (SSL : SSL_Handle)
     with Import, Convention => C, Link_Name => "SSL_set_connect_state";

   procedure SSL_set_accept_state (SSL : SSL_Handle)
     with Import, Convention => C, Link_Name => "SSL_set_accept_state";

   function SSL_renegotiate (SSL : SSL_Handle) return int
     with Import, Convention => C, Link_Name => "SSL_renegotiate";

   function SSL_do_handshake (SSL : SSL_Handle) return int
     with Import, Convention => C, Link_Name => "SSL_do_handshake";

   function SSL_want (S : SSL_Handle) return int
     with Import, Convention => C, Link_Name => "SSL_want";

   function SSL_read (SSL : SSL_Handle; Buf : Pointer; Num : int) return int
     with Import, Convention => C, Link_Name => "SSL_read";

   function SSL_peek (SSL : SSL_Handle; Buf : Pointer; Num : int) return int
     with Import, Convention => C, Link_Name => "SSL_peek";

   function SSL_write (SSL : SSL_Handle; Buf : Pointer; Num : int) return int
     with Import, Convention => C, Link_Name => "SSL_write";

   function SSL_pending (S : SSL_Handle) return int
     with Import, Convention => C, Link_Name => "SSL_pending";

   function SSL_get_error (SSL : SSL_Handle; Ret : int) return int
     with Import, Convention => C, Link_Name => "SSL_get_error";

   function SSL_shutdown (SSL : SSL_Handle) return int
     with Import, Convention => C, Link_Name => "SSL_shutdown";

   procedure SSL_set_shutdown (SSL : SSL_Handle; Mode : int)
     with Import, Convention => C, Link_Name => "SSL_set_shutdown";

   function SSL_get_SSL_CTX (SSL : SSL_Handle) return SSL_CTX
     with Import, Convention => C, Link_Name => "SSL_get_SSL_CTX";

   ----------------------
   --  Crypto routines --
   ----------------------

   type Generate_Key_Callback is access
     procedure (I1, I2 : Integer; Param : Pointer)
   with Convention => C;

   function RSA_generate_key
     (Bits     : int;
      E        : unsigned;
      Callback : Generate_Key_Callback;
      Cb_Arg   : Pointer) return RSA
     with Import, Convention => C, Link_Name => "RSA_generate_key";

   function SSL_use_RSAPrivateKey
     (SSL : SSL_Handle; Private_Key : RSA) return int
     with Import, Convention => C, Link_Name => "SSL_use_RSAPrivateKey";

   function SSL_CTX_use_PrivateKey_file
     (Ctx : SSL_CTX; File : char_array; C_Type : int) return int
     with Import, Convention => C, Link_Name => "SSL_CTX_use_PrivateKey_file";

   function SSL_use_PrivateKey_file
     (SSL : SSL_Handle; File : char_array; C_Type : int) return int
     with Import, Convention => C, Link_Name => "SSL_use_PrivateKey_file";

   function SSL_CTX_use_certificate_file
     (Ctx : SSL_CTX; File : char_array; C_Type : int) return int
     with Import, Convention => C, Link_Name => "SSL_CTX_use_certificate_file";

   function SSL_CTX_use_certificate_chain_file
     (Ctx : SSL_CTX; File : char_array) return int
     with Import, Convention => C,
          Link_Name => "SSL_CTX_use_certificate_chain_file";

   function SSL_use_certificate_file
     (SSL : SSL_Handle; File : char_array; C_Type : int) return int
     with Import, Convention => C, Link_Name => "SSL_use_certificate_file";

   function SSL_get_verify_result (SSL : SSL_Handle) return long
     with Import, Convention => C, Link_Name => "SSL_get_verify_result";

   function SSL_CTX_check_private_key (Ctx : SSL_CTX) return int
     with Import, Convention => C, Link_Name => "SSL_CTX_check_private_key";

   procedure SSL_CTX_set_verify
     (Ctx : SSL_CTX; Mode : int; Callback : Pointer)
     with Import, Convention => C, Link_Name => "SSL_CTX_set_verify";

   function SSL_CTX_get_verify_mode (Ctx : SSL_CTX) return unsigned
     with Import, Convention => C, Link_Name => "SSL_CTX_set_verify";

   function SSL_CTX_load_verify_locations
     (Cts : SSL_CTX; CAfile, CApath : Cstr.chars_ptr) return int
     with Import, Convention => C,
          Link_Name => "SSL_CTX_load_verify_locations";

   function SSL_CTX_set1_param
     (Ctx : SSL_CTX; Param : X509_VERIFY_PARAM) return int
     with Import, Convention => C, Link_Name => "SSL_CTX_set1_param";

   procedure RAND_seed (Buf : Pointer; Num : Integer)
     with Import, Convention => C, Link_Name => "RAND_seed";

   function RAND_status return Integer
     with Import, Convention => C, Link_Name => "RAND_status";

   procedure RAND_set_rand_method (Method : access Rand_Meth_St)
     with Import, Convention => C, Link_Name => "RAND_set_rand_method";

   --  Connection data

   function SSL_CTX_get_ex_new_index
     (Args                          : long;
      Argp                          : Pointer;
      New_Func, Dup_Func, Free_Func : Pointer) return int
     with Import, Convention => C, Link_Name => "SSL_CTX_get_ex_new_index";

   function SSL_CTX_set_ex_data
     (SSL : SSL_CTX; Idx : int; Arg : Pointer) return int
     with Import, Convention => C, Link_Name => "SSL_CTX_set_ex_data";

   function SSL_CTX_get_ex_data (SSL : SSL_CTX; Idx : int) return Pointer
     with Import, Convention => C, Link_Name => "SSL_CTX_get_ex_data";

   --  Certificate

   function SSL_get_peer_certificate (SSL : SSL_Handle) return X509
     with Import, Convention => C, Link_Name => "SSL_get_peer_certificate";

   procedure X509_free (X509 : Thin.X509)
     with Import, Convention => C, Link_Name => "X509_free";

   function X509_NAME_oneline
     (Name : X509_Name; Buf : Pointer; Size : int) return Cstr.chars_ptr
     with Import, Convention => C, Link_Name => "X509_NAME_oneline";

   function X509_NAME_print_ex
     (Output : BIO_Access;
      Name   : X509_Name;
      Indent : int;
      flags  : unsigned_long) return int
     with Import, Convention => C, Link_Name => "X509_NAME_print_ex";

   function X509_get_subject_name (X509 : Thin.X509) return X509_Name
     with Import, Convention => C, Link_Name => "X509_get_subject_name";

   function X509_get_issuer_name (X509 : Thin.X509) return X509_Name
     with Import, Convention => C, Link_Name => "X509_get_issuer_name";

   function X509_get_notAfter
     (X509 : Thin.X509) return access constant ASN1_STRING
     with Import, Convention => C, Link_Name => "__aws_X509_get_notAfter";

   function X509_get_notBefore
     (X509 : Thin.X509) return access constant ASN1_STRING
     with Import, Convention => C, Link_Name => "__aws_X509_get_notBefore";

   function X509_get_serialNumber
     (X509 : Thin.X509) return access constant ASN1_INTEGER
     with Import, Convention => C, Link_Name => "X509_get_serialNumber";

   procedure SSL_CTX_set_default_verify_paths (Ctx : SSL_CTX)
     with Import, Convention => C,
          Link_Name => "SSL_CTX_set_default_verify_paths";

   function SSL_get_ex_data_X509_STORE_CTX_idx return int
     with Import, Convention => C,
          Link_Name => "SSL_get_ex_data_X509_STORE_CTX_idx";

   function X509_STORE_CTX_get_ex_data
     (Ctx : X509_STORE_CTX; Idx : int) return SSL_Handle
     with Import, Convention => C, Link_Name => "X509_STORE_CTX_get_ex_data";

   function X509_STORE_CTX_get_current_cert (Ctx : X509_STORE_CTX) return X509
     with Import, Convention => C,
          Link_Name => "X509_STORE_CTX_get_current_cert";

   function X509_STORE_CTX_get_error (Ctx : SSL_CTX) return int
     with Import, Convention => C, Link_Name => "X509_STORE_CTX_get_error";

   function X509_verify_cert_error_string (Error : long) return Cstr.chars_ptr
     with Import, Convention => C,
          Link_Name => "X509_verify_cert_error_string";

   function SSL_CTX_get_cert_store (Ctx : SSL_CTX) return X509_STORE
     with Import, Convention => C, Link_Name => "SSL_CTX_get_cert_store";

   function X509_LOOKUP_file return X509_LOOKUP_METHOD
     with Import, Convention => C, Link_Name => "X509_LOOKUP_file";

   function X509_STORE_add_lookup
     (Store : X509_STORE; Method : X509_LOOKUP_METHOD) return X509_LOOKUP
     with Import, Convention => C, Link_Name => "X509_STORE_add_lookup";

   function X509_load_crl_file
     (Context : X509_LOOKUP; File : Strings.chars_ptr; Typ : int) return int
     with Import, Convention => C, Link_Name => "X509_load_crl_file";

   function X509_STORE_set1_param
     (Store : X509_STORE; Param : X509_VERIFY_PARAM) return int
     with Import, Convention => C, Link_Name => "X509_STORE_set1_param";

   --  Verify param

   function X509_VERIFY_PARAM_new return X509_VERIFY_PARAM
     with Import, Convention => C, Link_Name => "X509_VERIFY_PARAM_new";

   function X509_VERIFY_PARAM_set_flags
     (Param : X509_VERIFY_PARAM; Flags : unsigned_long) return int
     with Import, Convention => C, Link_Name => "X509_VERIFY_PARAM_set_flags";

   procedure X509_VERIFY_PARAM_free (Param : X509_VERIFY_PARAM)
     with Import, Convention => C, Link_Name => "X509_VERIFY_PARAM_free";

   -------------------
   -- ASN1 routines --
   -------------------

   function i2c_ASN1_INTEGER
     (A  : access constant ASN1_INTEGER;
      Pp : access Strings.chars_ptr) return int
     with Import, Convention => C, Link_Name => "i2c_ASN1_INTEGER";

   function ASN1_INTEGER_get (A : access constant ASN1_INTEGER) return long
     with Import, Convention => C, Link_Name => "ASN1_INTEGER_get";

   function ASN1_INTEGER_to_BN
     (A  : access constant ASN1_INTEGER;
      Pp : access Strings.chars_ptr) return BIGNUM
     with Import, Convention => C, Link_Name => "ASN1_INTEGER_to_BN";

   function BN_bn2bin (BN : BIGNUM; Output : Strings.chars_ptr) return int
     with Import, Convention => C, Link_Name => "BN_bn2bin";

   function BN_bn2hex (BN : BIGNUM) return Strings.chars_ptr
     with Import, Convention => C, Link_Name => "BN_bn2hex";

   -------------------
   --  BIO routines --
   -------------------

   function BIO_s_socket return BIO_Method_Access
     with Import, Convention => C, Link_Name => "BIO_s_socket";

   function BIO_s_mem return BIO_Method_Access
     with Import, Convention => C, Link_Name => "BIO_s_mem";

   function BIO_new (Method : BIO_Method_St) return BIO_Access
     with Import, Convention => C, Link_Name => "BIO_new";

   function BIO_new (Method : BIO_Method_Access) return BIO_Access
     with Import, Convention => C, Link_Name => "BIO_new";

   function BIO_int_ctrl
     (Bp   : BIO_Access;
      Cmd  : int;
      Larg : long;
      Iarg : int) return long
     with Import, Convention => C, Link_Name => "BIO_int_ctrl";

   procedure BIO_int_ctrl
     (Bp : BIO_Access; Cmd : int; Larg : long; Iarg : int)
     with Import, Convention => C, Link_Name => "BIO_int_ctrl";
   --  BIO_set_fd(b,fd,c) BIO_int_ctrl(b,BIO_C_SET_FD,c,fd)

   function BIO_ctrl
     (Bp   : BIO_Access;
      Cmd  : int;
      Larg : long    := 0;
      Parg : Pointer := Null_Pointer) return long
     with Import, Convention => C, Link_Name => "BIO_ctrl";
   --  BIO_pending(b) = (int)BIO_ctrl(b,BIO_CTRL_PENDING,0,NULL)

   procedure BIO_ctrl
     (Bp : BIO_Access; Cmd : int; Larg : long; Parg : Pointer)
     with Import, Convention => C, Link_Name => "BIO_ctrl";
   --  BIO_get_fd(b,c) = BIO_ctrl(b,BIO_C_GET_FD,0,(char *)c)
   --  BIO_set_mem_eof_return(b,v)
   --  = BIO_ctrl(b,BIO_C_SET_BUF_MEM_EOF_RETURN,v,NULL)

   function BIO_read
     (BIO : BIO_Access; Data : Pointer; Len : int) return int
     with Import, Convention => C, Link_Name => "BIO_read";

   function BIO_write
     (BIO : BIO_Access; Data : Pointer; Len : int) return int
     with Import, Convention => C, Link_Name => "BIO_write";

   function BIO_new_bio_pair
     (bio1 : access BIO_Access; writebuf1 : size_t;
      bio2 : access BIO_Access; writebuf2 : size_t) return int
     with Import, Convention => C, Link_Name => "BIO_new_bio_pair";

   function BIO_nread0 (BIO : BIO_Access; Buf : Pointer) return int
     with Import, Convention => C, Link_Name => "BIO_nread0";

   function BIO_nread
     (BIO : BIO_Access; Buf : Pointer; Num : int) return int
     with Import, Convention => C, Link_Name => "BIO_nread";
   --  Buf is the address of the buffer address

   function BIO_nwrite0 (BIO : BIO_Access; Buf : Pointer) return int
     with Import, Convention => C, Link_Name => "BIO_nwrite0";

   function BIO_nwrite
     (BIO : BIO_Access; Buf : Pointer; Num : int) return int
     with Import, Convention => C, Link_Name => "BIO_nwrite";
   --  Buf is the address of the buffer address

   function BIO_free (BIO : BIO_Access) return int
     with Import, Convention => C, Link_Name => "BIO_free";

   procedure BIO_free (BIO : BIO_Access)
     with Import, Convention => C, Link_Name => "BIO_free";

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
     (Ctx : SSL_CTX; Mode : long) return long
     with Import, Convention => C,
          Link_Name => "SSL_CTX_set_session_cache_mode";

   procedure SSL_CTX_flush_sessions (Ctx : SSL_CTX; Tm : long)
     with Import, Convention => C, Link_Name => "SSL_CTX_flush_sessions";

   type SSL_Session is new Pointer;

   function SSL_set_session
     (SSL : SSL_Handle; session : SSL_Session) return int
     with Import, Convention => C, Link_Name => "SSL_set_session";

   function SSL_get_session (SSL : SSL_Handle) return SSL_Session
     with Import, Convention => C, Link_Name => "SSL_get_session";

   function SSL_get1_session (SSL : SSL_Handle) return SSL_Session
     with Import, Convention => C, Link_Name => "SSL_get1_session";

end SSL.Thin;
