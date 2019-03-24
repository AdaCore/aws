------------------------------------------------------------------------------
--                            Secure Sockets Layer                          --
--                                                                          --
--                     Copyright (C) 2000-2016, AdaCore                     --
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
   end record with Convention => C;

   type ssl_cipher_st is record
      valid : int;
      name : Cstr.chars_ptr; -- text name
      id   : unsigned_long;  -- id, 4 bytes, first is version

      --  changed in 0.9.9: these four used to be portions of a single value
      --  'algorithms'
      algorithm_mkey : unsigned_long; -- key exchange algorithm
      algorithm_auth : unsigned_long; -- server authentication
      algorithm_enc  : unsigned_long; -- symmetric encryption
      algorithm_mac  : unsigned_long; -- symmetric authentication
      algorithm_ssl  : unsigned_long; -- (major) protocol version

      algo_strength  : unsigned_long; -- strength and export flags
      algorithm2     : unsigned_long; -- Extra flags
      strength_bits  : int; -- Number of bits really used
      alg_bits       : int; -- Number of bits for algorithm
   end record with Convention => C;

   subtype SSL_CIPHER is ssl_cipher_st;

   type bn_gencb_st;

   type gen_cb_function is access function
     (a, b : int; cb : access bn_gencb_st) return int with Convention => C;

   type bn_gencb_st is record
      var : unsigned := 2;
      arg : Pointer;
      cb  : gen_cb_function;
   end record with Convention => C;

   subtype BN_GENCB is bn_gencb_st;

   type X509_NAME_ENTRY is null record with Convention => C;

   type BIO_callback is access function
     (BIO  : BIO_Access;
      cmd  : int;
      argp : Cstr.chars_ptr;
      argi : int;
      argl : long;
      ret  : long) return long
     with Convention => C;

   subtype SSL_Method     is Pointer;
   subtype RSA            is Pointer;
   subtype DH             is Pointer;

   type ENGINE is new Pointer;
   type EVP_PKEY_CTX is new Pointer;
   type EVP_PKEY is new Pointer;

   subtype Private_Key is EVP_PKEY;

   type SSL_CTX            is new Pointer;
   type SSL_Handle         is new Pointer;
   type X509               is new Pointer;
   type X509_NAME          is new Pointer;
   type X509_STORE         is new Pointer;
   type X509_STORE_CTX     is new Pointer;
   type X509_LOOKUP        is new Pointer;
   type X509_LOOKUP_METHOD is new Pointer;
   type X509_VERIFY_PARAM  is new Pointer;
   type STACK_OF_X509_NAME is new Pointer;

   type BIGNUM             is new Pointer;

   Null_CTX    : constant SSL_CTX    := SSL_CTX (Null_Pointer);
   Null_Handle : constant SSL_Handle := SSL_Handle (Null_Pointer);
   Null_X509   : constant X509       := X509 (Null_Pointer);

   Null_STACK_OF_X509_NAME : constant STACK_OF_X509_NAME :=
     STACK_OF_X509_NAME (Null_Pointer);

   Null_Private_Key : Private_Key := Private_Key (Null_Pointer);

   subtype Error_Code is unsigned_long;

   SSL_FILETYPE_PEM                  : constant := 1;

   SSL_CTRL_NEED_TMP_RSA             : constant := 1;
   SSL_CTRL_SET_TMP_RSA              : constant := 2;
   SSL_CTRL_SET_TMP_DH               : constant := 3;
   SSL_CTRL_SET_TMP_ECDH             : constant := 4;
   SSL_CTRL_SET_TMP_RSA_CB           : constant := 5;
   SSL_CTRL_SET_TMP_DH_CB            : constant := 6;
   SSL_CTRL_SET_TMP_ECDH_CB          : constant := 7;

   SSL_CTRL_GET_SESSION_REUSED       : constant := 8;
   SSL_CTRL_GET_CLIENT_CERT_REQUEST  : constant := 9;
   SSL_CTRL_GET_NUM_RENEGOTIATIONS   : constant := 10;
   SSL_CTRL_CLEAR_NUM_RENEGOTIATIONS : constant := 11;
   SSL_CTRL_GET_TOTAL_RENEGOTIATIONS : constant := 12;
   SSL_CTRL_GET_FLAGS                : constant := 13;
   SSL_CTRL_EXTRA_CHAIN_CERT         : constant := 14;

   SSL_CTRL_SET_MSG_CALLBACK         : constant := 15;
   SSL_CTRL_SET_MSG_CALLBACK_ARG     : constant := 16;

   SSL_CTRL_SET_MTU                  : constant := 17;

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
   SSL_CTRL_MODE                     : constant := 33;
   SSL_CTRL_GET_READ_AHEAD           : constant := 40;
   SSL_CTRL_SET_READ_AHEAD           : constant := 41;
   SSL_CTRL_SET_SESS_CACHE_SIZE      : constant := 42;
   SSL_CTRL_GET_SESS_CACHE_SIZE      : constant := 43;
   SSL_CTRL_SET_SESS_CACHE_MODE      : constant := 44;
   SSL_CTRL_GET_SESS_CACHE_MODE      : constant := 45;

   SSL_CTRL_GET_MAX_CERT_LIST        : constant := 50;
   SSL_CTRL_SET_MAX_CERT_LIST        : constant := 51;

   SSL_CTRL_SET_MAX_SEND_FRAGMENT    : constant := 52;

   SSL_CTRL_SET_TLSEXT_SERVERNAME_CB           : constant := 53;
   SSL_CTRL_SET_TLSEXT_SERVERNAME_ARG          : constant := 54;
   SSL_CTRL_SET_TLSEXT_HOSTNAME                : constant := 55;
   SSL_CTRL_SET_TLSEXT_DEBUG_CB                : constant := 56;
   SSL_CTRL_SET_TLSEXT_DEBUG_ARG               : constant := 57;
   SSL_CTRL_GET_TLSEXT_TICKET_KEYS             : constant := 58;
   SSL_CTRL_SET_TLSEXT_TICKET_KEYS             : constant := 59;
   SSL_CTRL_SET_TLSEXT_OPAQUE_PRF_INPUT        : constant := 60;
   SSL_CTRL_SET_TLSEXT_OPAQUE_PRF_INPUT_CB     : constant := 61;
   SSL_CTRL_SET_TLSEXT_OPAQUE_PRF_INPUT_CB_ARG : constant := 62;
   SSL_CTRL_SET_TLSEXT_STATUS_REQ_CB           : constant := 63;
   SSL_CTRL_SET_TLSEXT_STATUS_REQ_CB_ARG       : constant := 64;
   SSL_CTRL_SET_TLSEXT_STATUS_REQ_TYPE         : constant := 65;
   SSL_CTRL_GET_TLSEXT_STATUS_REQ_EXTS         : constant := 66;
   SSL_CTRL_SET_TLSEXT_STATUS_REQ_EXTS         : constant := 67;
   SSL_CTRL_GET_TLSEXT_STATUS_REQ_IDS          : constant := 68;
   SSL_CTRL_SET_TLSEXT_STATUS_REQ_IDS          : constant := 69;
   SSL_CTRL_GET_TLSEXT_STATUS_REQ_OCSP_RESP    : constant := 70;
   SSL_CTRL_SET_TLSEXT_STATUS_REQ_OCSP_RESP    : constant := 71;

   SSL_CTRL_SET_TLSEXT_TICKET_KEY_CB    : constant := 72;

   SSL_CTRL_SET_TLS_EXT_SRP_USERNAME_CB : constant := 75;
   SSL_CTRL_SET_SRP_VERIFY_PARAM_CB     : constant := 76;
   SSL_CTRL_SET_SRP_GIVE_CLIENT_PWD_CB  : constant := 77;

   SSL_CTRL_SET_SRP_ARG              : constant := 78;
   SSL_CTRL_SET_TLS_EXT_SRP_USERNAME : constant := 79;
   SSL_CTRL_SET_TLS_EXT_SRP_STRENGTH : constant := 80;
   SSL_CTRL_SET_TLS_EXT_SRP_PASSWORD : constant := 81;

   SSL_CTRL_TLS_EXT_SEND_HEARTBEAT            : constant := 85;
   SSL_CTRL_GET_TLS_EXT_HEARTBEAT_PENDING     : constant := 86;
   SSL_CTRL_SET_TLS_EXT_HEARTBEAT_NO_REQUESTS : constant := 87;

   TLSEXT_NAMETYPE_host_name         : constant := 0;

   DTLS_CTRL_GET_TIMEOUT             : constant := 73;
   DTLS_CTRL_HANDLE_TIMEOUT          : constant := 74;
   DTLS_CTRL_LISTEN                  : constant := 75;

   SSL_CTRL_GET_RI_SUPPORT           : constant := 76;
   SSL_CTRL_CLEAR_OPTIONS            : constant := 77;
   SSL_CTRL_CLEAR_MODE               : constant := 78;

   SSL_CTRL_GET_EXTRA_CHAIN_CERTS    : constant := 82;
   SSL_CTRL_CLEAR_EXTRA_CHAIN_CERTS  : constant := 83;

   SSL_SENT_SHUTDOWN                 : constant := 1;
   SSL_RECEIVED_SHUTDOWN             : constant := 2;

   SSL_OP_MICROSOFT_SESS_ID_BUG                  : constant := 16#00000001#;
   SSL_OP_NETSCAPE_CHALLENGE_BUG                 : constant := 16#00000002#;
   SSL_OP_LEGACY_SERVER_CONNECT                  : constant := 16#00000004#;
   SSL_OP_NETSCAPE_REUSE_CIPHER_CHANGE_BUG       : constant := 16#00000008#;
   SSL_OP_SSLREF2_REUSE_CERT_TYPE_BUG            : constant := 16#00000010#;
   SSL_OP_MICROSOFT_BIG_SSLV3_BUFFER             : constant := 16#00000020#;
   SSL_OP_MSIE_SSLV2_RSA_PADDING                 : constant := 16#00000040#;
   SSL_OP_SSLEAY_080_CLIENT_DH_BUG               : constant := 16#00000080#;
   SSL_OP_TLS_D5_BUG                             : constant := 16#00000100#;
   SSL_OP_TLS_BLOCK_PADDING_BUG                  : constant := 16#00000200#;
   SSL_OP_DONT_INSERT_EMPTY_FRAGMENTS            : constant := 16#00000800#;
   SSL_OP_ALL                                    : constant := 16#80000BFF#;
   SSL_OP_NO_QUERY_MTU                           : constant := 16#00001000#;
   SSL_OP_COOKIE_EXCHANGE                        : constant := 16#00002000#;
   SSL_OP_NO_TICKET                              : constant := 16#00004000#;
   SSL_OP_CISCO_ANYCONNECT                       : constant := 16#00008000#;
   SSL_OP_NO_SESSION_RESUMPTION_ON_RENEGOTIATION : constant := 16#00010000#;
   SSL_OP_NO_COMPRESSION                         : constant := 16#00020000#;
   SSL_OP_ALLOW_UNSAFE_LEGACY_RENEGOTIATION      : constant := 16#00040000#;
   SSL_OP_SINGLE_ECDH_USE                        : constant := 16#00080000#;
   SSL_OP_SINGLE_DH_USE                          : constant := 16#00100000#;
   SSL_OP_EPHEMERAL_RSA                          : constant := 16#00200000#;
   SSL_OP_CIPHER_SERVER_PREFERENCE               : constant := 16#00400000#;
   SSL_OP_TLS_ROLLBACK_BUG                       : constant := 16#00800000#;
   SSL_OP_NO_SSLv2                               : constant := 16#01000000#;
   SSL_OP_NO_SSLv3                               : constant := 16#02000000#;
   SSL_OP_NO_TLSv1                               : constant := 16#04000000#;
   SSL_OP_NO_TLSv1_2                             : constant := 16#08000000#;
   SSL_OP_NO_TLSv1_1                             : constant := 16#10000000#;
   SSL_OP_PKCS1_CHECK_1                          : constant := 16#0#;
   SSL_OP_PKCS1_CHECK_2                          : constant := 16#0#;
   SSL_OP_NETSCAPE_CA_DN_BUG                     : constant := 16#20000000#;
   SSL_OP_NETSCAPE_DEMO_CIPHER_CHANGE_BUG        : constant := 16#40000000#;
   SSL_OP_CRYPTOPRO_TLSEXT_BUG                   : constant := 16#80000000#;

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

   SSL_NOTHING     : constant := 1;
   SSL_WRITING     : constant := 2;
   SSL_READING     : constant := 3;
   SSL_X509_LOOKUP : constant := 4;

   SSL_MAX_KRB5_PRINCIPAL_LENGTH       : constant := 256;
   SSL_MAX_SSL_SESSION_ID_LENGTH       : constant := 32;
   SSL_MAX_SID_CTX_LENGTH              : constant := 32;
   SSL_MIN_RSA_MODULUS_LENGTH_IN_BYTES : constant := 512 / 8;
   SSL_MAX_KEY_ARG_LENGTH              : constant := 8;
   SSL_MAX_MASTER_KEY_LENGTH           : constant := 48;

   SSL_TLSEXT_ERR_OK            : constant := 0;
   SSL_TLSEXT_ERR_ALERT_WARNING : constant := 1;
   SSL_TLSEXT_ERR_ALERT_FATAL   : constant := 2;
   SSL_TLSEXT_ERR_NOACK         : constant := 3;

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

   BIO_FP_READ   : constant := 2;
   BIO_FP_WRITE  : constant := 4;
   BIO_FP_APPEND : constant := 8;
   BIO_FP_TEXT   : constant := 16;

   BIO_FLAGS_READ       : constant := 1;
   BIO_FLAGS_WRITE      : constant := 2;
   BIO_FLAGS_IO_SPECIAL : constant := 4;
   BIO_FLAGS_RWS        : constant
     := BIO_FLAGS_READ + BIO_FLAGS_WRITE + BIO_FLAGS_IO_SPECIAL;
   BIO_FLAGS_SHOULD_RETRY  : constant := 8;

   RSA_3  : constant := 3;
   RSA_F4 : constant := 16#10001#;

   RSA_PKCS1_PADDING      : constant := 1;
   RSA_SSLV23_PADDING     : constant := 2;
   RSA_NO_PADDING         : constant := 3;
   RSA_PKCS1_OAEP_PADDING : constant := 4;
   RSA_X931_PADDING       : constant := 5;
   RSA_PKCS1_PSS_PADDING  : constant := 6;
   RSA_PKCS1_PADDING_SIZE : constant := 11;

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

   NID_md5       : constant := 4;
   NID_sha1      : constant := 64;
   NID_sha224    : constant := 675;
   NID_sha256    : constant := 672;
   NID_sha384    : constant := 673;
   NID_sha512    : constant := 674;
   NID_ripemd160 : constant := 117;

   NID_sxnet                        : constant := 143;
   NID_X500                         : constant := 11;
   NID_X509                         : constant := 12;
   NID_commonName                   : constant := 13;
   NID_surname                      : constant := 100;
   NID_serialNumber                 : constant := 105;
   NID_countryName                  : constant := 14;
   NID_localityName                 : constant := 15;
   NID_stateOrProvinceName          : constant := 16;
   NID_streetAddress                : constant := 660;
   NID_organizationName             : constant := 17;
   NID_organizationalUnitName       : constant := 18;
   NID_title                        : constant := 106;
   NID_description                  : constant := 107;
   NID_searchGuide                  : constant := 859;
   NID_businessCategory             : constant := 860;
   NID_postalAddress                : constant := 861;
   NID_postalCode                   : constant := 661;
   NID_postOfficeBox                : constant := 862;
   NID_physicalDeliveryOfficeName   : constant := 863;
   NID_telephoneNumber              : constant := 864;
   NID_telexNumber                  : constant := 865;
   NID_teletexTerminalIdentifier    : constant := 866;
   NID_facsimileTelephoneNumber     : constant := 867;
   NID_x121Address                  : constant := 868;
   NID_internationaliSDNNumber      : constant := 869;
   NID_registeredAddress            : constant := 870;
   NID_destinationIndicator         : constant := 871;
   NID_preferredDeliveryMethod      : constant := 872;
   NID_presentationAddress          : constant := 873;
   NID_supportedApplicationContext  : constant := 874;
   NID_member                       : constant := 875;
   NID_owner                        : constant := 876;
   NID_roleOccupant                 : constant := 877;
   NID_seeAlso                      : constant := 878;
   NID_userPassword                 : constant := 879;
   NID_userCertificate              : constant := 880;
   NID_cACertificate                : constant := 881;
   NID_authorityRevocationList      : constant := 882;
   NID_certificateRevocationList    : constant := 883;
   NID_crossCertificatePair         : constant := 884;
   NID_name                         : constant := 173;
   NID_givenName                    : constant := 99;
   NID_initials                     : constant := 101;
   NID_generationQualifier          : constant := 509;
   NID_x500UniqueIdentifier         : constant := 503;
   NID_dnQualifier                  : constant := 174;
   NID_enhancedSearchGuide          : constant := 885;
   NID_protocolInformation          : constant := 886;
   NID_distinguishedName            : constant := 887;
   NID_uniqueMember                 : constant := 888;
   NID_houseIdentifier              : constant := 889;
   NID_supportedAlgorithms          : constant := 890;
   NID_deltaRevocationList          : constant := 891;
   NID_dmdName                      : constant := 892;
   NID_pseudonym                    : constant := 510;
   NID_role                         : constant := 400;
   NID_X500algorithms               : constant := 378;
   NID_rsa                          : constant := 19;
   NID_mdc2WithRSA                  : constant := 96;
   NID_mdc2                         : constant := 95;
   NID_id_ce                        : constant := 81;
   NID_subject_directory_attributes : constant := 769;
   NID_subject_key_identifier       : constant := 82;
   NID_key_usage                    : constant := 83;
   NID_private_key_usage_period     : constant := 84;
   NID_subject_alt_name             : constant := 85;
   NID_issuer_alt_name              : constant := 86;
   NID_basic_constraints            : constant := 87;
   NID_crl_number                   : constant := 88;
   NID_crl_reason                   : constant := 141;
   NID_invalidity_date              : constant := 142;
   NID_delta_crl                    : constant := 140;
   NID_issuing_distribution_point   : constant := 770;
   NID_certificate_issuer           : constant := 771;
   NID_name_constraints             : constant := 666;
   NID_crl_distribution_points      : constant := 103;
   NID_certificate_policies         : constant := 89;
   NID_any_policy                   : constant := 746;
   NID_policy_mappings              : constant := 747;
   NID_authority_key_identifier     : constant := 90;
   NID_policy_constraints           : constant := 401;
   NID_ext_key_usage                : constant := 126;
   NID_freshest_crl                 : constant := 857;
   NID_inhibit_any_policy           : constant := 748;
   NID_target_information           : constant := 402;
   NID_no_rev_avail                 : constant := 403;
   NID_anyExtendedKeyUsage          : constant := 910;

   EVP_PKEY_OP_UNDEFINED     : constant := 0;
   EVP_PKEY_OP_PARAMGEN      : constant := 2 ** 1;
   EVP_PKEY_OP_KEYGEN        : constant := 2 ** 2;
   EVP_PKEY_OP_SIGN          : constant := 2 ** 3;
   EVP_PKEY_OP_VERIFY        : constant := 2 ** 4;
   EVP_PKEY_OP_VERIFYRECOVER : constant := 2 ** 5;
   EVP_PKEY_OP_SIGNCTX       : constant := 2 ** 6;
   EVP_PKEY_OP_VERIFYCTX     : constant := 2 ** 7;
   EVP_PKEY_OP_ENCRYPT       : constant := 2 ** 8;
   EVP_PKEY_OP_DECRYPT       : constant := 2 ** 9;
   EVP_PKEY_OP_DERIVE        : constant := 2 ** 10;

   EVP_PKEY_OP_TYPE_SIG  : constant := EVP_PKEY_OP_SIGN + EVP_PKEY_OP_VERIFY
                             + EVP_PKEY_OP_VERIFYRECOVER
                             + EVP_PKEY_OP_SIGNCTX + EVP_PKEY_OP_VERIFYCTX;

   EVP_PKEY_CTRL_MD            : constant := 1;
   EVP_PKEY_CTRL_PEER_KEY      : constant := 2;
   EVP_PKEY_CTRL_PKCS7_ENCRYPT : constant := 3;
   EVP_PKEY_CTRL_PKCS7_DECRYPT : constant := 4;
   EVP_PKEY_CTRL_PKCS7_SIGN    : constant := 5;
   EVP_PKEY_CTRL_SET_MAC_KEY   : constant := 6;
   EVP_PKEY_CTRL_DIGESTINIT    : constant := 7;

   type ASN1_STRING is record
      length : int;
      stype  : int;
      data   : Interfaces.C.Strings.chars_ptr;
   end record with Convention => C;

   type ASN1_OBJECT is null record with Convention => C;

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

   function CRYPTO_num_locks return Natural
     with Import, Convention => C, External_Name => "__aws_CRYPTO_num_locks";

   procedure CRYPTO_set_id_callback (Id_Function : Pointer)
     with Import, Convention => C,
          External_Name => "__aws_CRYPTO_set_id_callback";

   procedure CRYPTO_set_locking_callback (Locking_Function : Pointer)
     with Import, Convention => C,
          External_Name => "__aws_CRYPTO_set_locking_callback";

   function CRYPTO_get_locking_callback return Pointer
     with Import, Convention => C,
          External_Name => "CRYPTO_get_locking_callback";

   procedure CRYPTO_set_dynlock_create_callback (Create_Function : Pointer)
     with Import, Convention => C,
          External_Name => "__aws_CRYPTO_set_dynlock_create_callback";

   procedure CRYPTO_set_dynlock_lock_callback (Lock_Function : Pointer)
     with Import, Convention => C,
          External_Name => "__aws_CRYPTO_set_dynlock_lock_callback";

   procedure CRYPTO_set_dynlock_destroy_callback
     (Destroy_Function : Pointer)
     with Import, Convention => C,
          External_Name => "__aws_CRYPTO_set_dynlock_destroy_callback";

   function CRYPTO_get_dynlock_create_callback  return Pointer
     with Import, Convention => C,
          External_Name => "CRYPTO_get_dynlock_create_callback";

   function CRYPTO_get_dynlock_lock_callback    return Pointer
     with Import, Convention => C,
          External_Name => "CRYPTO_get_dynlock_lock_callback";

   function CRYPTO_get_dynlock_destroy_callback return Pointer
     with Import, Convention => C,
          External_Name => "CRYPTO_get_dynlock_destroy_callback";

   ------------------------------------------
   -- OpenSSL version information routines --
   ------------------------------------------

   OPENSSL_VERSION0 : constant := 0;
   OPENSSL_CFLAGS   : constant := 2;
   OPENSSL_BUILT_ON : constant := 3;
   OPENSSL_PLATFORM : constant := 4;
   OPENSSL_DIR      : constant := 5;

   function OpenSSL_version_num return long
     with Import, Convention => C, External_Name => "OpenSSL_version_num";
   --  Returns OpenSSL numeric release version identifier

   function SSLeay return long renames OpenSSL_version_num;

   function OpenSSL_version (T : int) return Cstr.chars_ptr
     with Import, Convention => C, External_Name => "OpenSSL_version";
   --  Returns version information line

   -------------------------------
   -- Context control routines  --
   -------------------------------

   function TLS_method         return SSL_Method
     with Import, Convention => C, External_Name => "TLS_method";

   function TLS_server_method  return SSL_Method
     with Import, Convention => C, External_Name => "TLS_server_method";

   function TLS_client_method  return SSL_Method
     with Import, Convention => C, External_Name => "TLS_client_method";

   function TLSv1_method         return SSL_Method
     with Import, Convention => C, External_Name => "TLSv1_method";

   function TLSv1_server_method  return SSL_Method
     with Import, Convention => C, External_Name => "TLSv1_server_method";

   function TLSv1_client_method  return SSL_Method
     with Import, Convention => C, External_Name => "TLSv1_client_method";

   function TLSv1_1_method         return SSL_Method
     with Import, Convention => C, External_Name => "TLSv1_1_method";

   function TLSv1_1_server_method  return SSL_Method
     with Import, Convention => C, External_Name => "TLSv1_1_server_method";

   function TLSv1_1_client_method  return SSL_Method
     with Import, Convention => C, External_Name => "TLSv1_1_client_method";

   function TLSv1_2_method         return SSL_Method
     with Import, Convention => C, External_Name => "TLSv1_2_method";

   function TLSv1_2_server_method  return SSL_Method
     with Import, Convention => C, External_Name => "TLSv1_2_server_method";

   function TLSv1_2_client_method  return SSL_Method
     with Import, Convention => C, External_Name => "TLSv1_2_client_method";

   function SSL_CTX_new (Meth : SSL_Method) return SSL_CTX
     with Import, Convention => C, External_Name => "SSL_CTX_new";

   procedure SSL_CTX_free (P1 : SSL_CTX)
     with Import, Convention => C, External_Name => "SSL_CTX_free";

   procedure SSL_CTX_set_quiet_shutdown (Ctx : SSL_CTX; Mode : int)
     with Import, Convention => C,
          External_Name => "SSL_CTX_set_quiet_shutdown";

   function SSL_CTX_set_options
     (Ctx : SSL_CTX; Op : unsigned_long) return unsigned_long
     with Import, Convention => C,
          External_Name => "__aws_SSL_CTX_set_options";

   function SSL_CTX_ctrl
     (Ctx : SSL_CTX; Cmd : int; Larg : long; Parg : Pointer) return long
     with Import, Convention => C, External_Name => "SSL_CTX_ctrl";

   function SSL_CTX_callback_ctrl
     (Ctx : SSL_CTX; Func : int; CB : Pointer) return long
     with Import, Convention => C, External_Name => "SSL_CTX_callback_ctrl";

   function SSL_CTX_set_tlsext_servername_callback
     (Ctx : SSL_CTX; CB : Pointer) return long is
      (SSL_CTX_callback_ctrl (Ctx, SSL_CTRL_SET_TLSEXT_SERVERNAME_CB, CB));

   function SSL_CTX_set_tlsext_servername_arg
     (Ctx : SSL_CTX; Arg : Pointer) return long is
     (SSL_CTX_ctrl (Ctx, SSL_CTRL_SET_TLSEXT_SERVERNAME_ARG, 0, Arg));

   -------------------------------------
   -- Library initialization routines --
   -------------------------------------

   function CRYPTO_set_mem_functions (M, R, F : System.Address) return int
     with Import, Convention => C, External_Name => "CRYPTO_set_mem_functions";

   function SSL_library_init return int
     with Import, Convention => C, External_Name => "__aws_SSL_library_init";

   procedure ERR_load_crypto_strings
     with Import, Convention => C, External_Name => "ERR_load_crypto_strings";

   procedure ERR_load_ssl_strings
     with Import, Convention => C, External_Name => "ERR_load_ssl_strings";

   --------------------------------
   -- Error information routines --
   --------------------------------

   function ERR_get_error return Error_Code
     with Import, Convention => C, External_Name => "ERR_get_error";

   function ERR_error_string
     (Code : Error_Code; Buffer : Cstr.chars_ptr) return Cstr.chars_ptr
     with Import, Convention => C, External_Name => "ERR_error_string";

   procedure ERR_error_string_n
     (Code : Error_Code; Buffer : Cstr.chars_ptr; Len : size_t)
     with Import, Convention => C, External_Name => "ERR_error_string_n";

   procedure ERR_remove_state (pid : int := 0)
     with Import, Convention => C, External_Name => "ERR_remove_state";

   -----------------------------------------
   -- Connection handler control routines --
   -----------------------------------------

   function SSL_new (Ctx : SSL_CTX) return SSL_Handle
     with Import, Convention => C, External_Name => "SSL_new";

   procedure SSL_free (SSL : SSL_Handle)
     with Import, Convention => C, External_Name => "SSL_free";

   function SSL_clear (SSL : SSL_Handle) return int
     with Import, Convention => C, External_Name => "SSL_clear";

   function SSL_ctrl
     (SSL : SSL_Handle; Cmd : int; Larg : int; Parg : Pointer) return long
     with Import, Convention => C, External_Name => "SSL_ctrl";

   function SSL_set_fd (S : SSL_Handle; Fd : int) return int
     with Import, Convention => C, External_Name => "SSL_set_fd";

   procedure SSL_set_bio (SSL : SSL_Handle; RBIO, WBIO : BIO_Access)
     with Import, Convention => C, External_Name => "SSL_set_bio";

   function SSL_get_rbio (SSL : SSL_Handle) return BIO_Access
     with Import, Convention => C, External_Name => "SSL_get_rbio";

   function SSL_get_wbio (SSL : SSL_Handle) return BIO_Access
     with Import, Convention => C, External_Name => "SSL_get_wbio";

   procedure SSL_set_read_ahead (S : SSL_Handle; Yes : int)
     with Import, Convention => C, External_Name => "SSL_set_read_ahead";

   function SSL_connect (SSL : SSL_Handle) return int
     with Import, Convention => C, External_Name => "SSL_connect";

   function SSL_accept (SSL : SSL_Handle) return int
     with Import, Convention => C, External_Name => "SSL_accept";

   procedure SSL_set_connect_state (SSL : SSL_Handle)
     with Import, Convention => C, External_Name => "SSL_set_connect_state";

   procedure SSL_set_accept_state (SSL : SSL_Handle)
     with Import, Convention => C, External_Name => "SSL_set_accept_state";

   function SSL_renegotiate (SSL : SSL_Handle) return int
     with Import, Convention => C, External_Name => "SSL_renegotiate";

   function SSL_do_handshake (SSL : SSL_Handle) return int
     with Import, Convention => C, External_Name => "SSL_do_handshake";

   function SSL_want (S : SSL_Handle) return int
     with Import, Convention => C, External_Name => "SSL_want";

   function SSL_want_nothing (S : SSL_Handle) return Boolean
   is (SSL_want (S) = SSL_NOTHING);

   function SSL_want_read (S : SSL_Handle) return Boolean
   is (SSL_want (S) = SSL_READING);

   function SSL_want_write (S : SSL_Handle) return Boolean
   is (SSL_want (S) = SSL_WRITING);

   function SSL_want_x509_lookup (S : SSL_Handle) return Boolean
   is (SSL_want (S) = SSL_X509_LOOKUP);

   function SSL_read (SSL : SSL_Handle; Buf : Pointer; Num : int) return int
     with Import, Convention => C, External_Name => "SSL_read";

   function SSL_peek (SSL : SSL_Handle; Buf : Pointer; Num : int) return int
     with Import, Convention => C, External_Name => "SSL_peek";

   function SSL_write (SSL : SSL_Handle; Buf : Pointer; Num : int) return int
     with Import, Convention => C, External_Name => "SSL_write";

   function SSL_pending (S : SSL_Handle) return int
     with Import, Convention => C, External_Name => "SSL_pending";

   function SSL_get_error (SSL : SSL_Handle; Ret : int) return int
     with Import, Convention => C, External_Name => "SSL_get_error";

   function SSL_state (SSL : SSL_Handle) return int
     with Import, Convention => C, External_Name => "SSL_state";

   function SSL_state_string (SSL : SSL_Handle) return Cstr.chars_ptr
     with Import, Convention => C, External_Name => "SSL_state_string";

   function SSL_state_string_long (SSL : SSL_Handle) return Cstr.chars_ptr
     with Import, Convention => C, External_Name => "SSL_state_string_long";

   function SSL_get_current_cipher (SSL : SSL_Handle) return access SSL_CIPHER
     with Import, Convention => C, External_Name => "SSL_get_current_cipher";

   function SSL_CIPHER_get_version (cipher : SSL_CIPHER) return Cstr.chars_ptr
     with Import, Convention => C, External_Name => "SSL_CIPHER_get_version";

   function SSL_CIPHER_get_name (cipher : SSL_CIPHER) return Cstr.chars_ptr
     with Import, Convention => C, External_Name => "SSL_CIPHER_get_name";

   function SSL_CIPHER_description
     (cipher : SSL_CIPHER;
      buf    : Cstr.char_array_access;
      size   : int) return Cstr.chars_ptr
     with Import, Convention => C, External_Name => "SSL_CIPHER_description";

   function SSL_get_cipher_list
     (SSL : SSL_Handle; priority : int) return Cstr.chars_ptr
     with Import, Convention => C, External_Name => "SSL_get_cipher_list";

   function SSL_shutdown (SSL : SSL_Handle) return int
     with Import, Convention => C, External_Name => "SSL_shutdown";

   procedure SSL_set_shutdown (SSL : SSL_Handle; Mode : int)
     with Import, Convention => C, External_Name => "SSL_set_shutdown";

   function SSL_get_SSL_CTX (SSL : SSL_Handle) return SSL_CTX
     with Import, Convention => C, External_Name => "SSL_get_SSL_CTX";

   function SSL_set_SSL_CTX (SSL : SSL_Handle; Ctx : SSL_CTX) return SSL_CTX
     with Import, Convention => C, External_Name => "SSL_set_SSL_CTX";

   function SSL_get_shutdown (SSL : SSL_Handle) return int
     with Import, Convention => C, External_Name => "SSL_get_shutdown";

   function SSL_set_tlsext_host_name
     (SSL : SSL_Handle; Name : char_array) return long
   is (SSL_ctrl
         (SSL, SSL_CTRL_SET_TLSEXT_HOSTNAME, TLSEXT_NAMETYPE_host_name,
          Name'Address));

   function SSL_get_servername
     (SSL  : SSL_Handle;
      Kind : int := TLSEXT_NAMETYPE_host_name) return Cstr.chars_ptr
     with Import, Convention => C, External_Name => "SSL_get_servername";

   ----------------------
   --  Crypto routines --
   ----------------------

   type EVP_MD is new Pointer;
   type EVP_MD_CTX is new Pointer;

   function EVP_MD_CTX_new return EVP_MD_CTX
     with Import, Convention => C, External_Name => "EVP_MD_CTX_new";

   procedure EVP_MD_CTX_free (Ctx : EVP_MD_CTX)
     with Import, Convention => C, External_Name => "EVP_MD_CTX_free";

   function EVP_DigestInit (ctx : EVP_MD_CTX; kind : EVP_MD) return int
     with Import, Convention => C, External_Name => "EVP_DigestInit";

   function EVP_DigestUpdate
     (ctx : EVP_MD_CTX; d : Pointer; cnt : size_t) return int
     with Import, Convention => C, External_Name => "EVP_DigestUpdate";

   function EVP_DigestFinal
     (ctx : EVP_MD_CTX; md : Pointer; s : access unsigned) return int
     with Import, Convention => C, External_Name => "EVP_DigestFinal";

   function EVP_DigestFinal_ex
     (ctx : EVP_MD_CTX; md : Pointer; s : access unsigned) return int
     with Import, Convention => C, External_Name => "EVP_DigestFinal_ex";

   function EVP_MD_size (md : EVP_MD) return int
     with Import, Convention => C, External_Name => "EVP_MD_size";

   function EVP_PKEY_new return EVP_PKEY
     with Import, Convention => C, External_Name => "EVP_PKEY_new";

   procedure EVP_PKEY_free (key : EVP_PKEY)
     with Import, Convention => C, External_Name => "EVP_PKEY_free";

   function EVP_md_null return EVP_MD
     with Import, Convention => C, External_Name => "EVP_md_null";

   function EVP_md2 return EVP_MD
     with Import, Convention => C, External_Name => "EVP_md2";

   function EVP_md5 return EVP_MD
     with Import, Convention => C, External_Name => "EVP_md5";

   function EVP_sha1 return EVP_MD
     with Import, Convention => C, External_Name => "EVP_sha1";

   function EVP_dss return EVP_MD
     with Import, Convention => C, External_Name => "EVP_dss";

   function EVP_dss1 return EVP_MD
     with Import, Convention => C, External_Name => "EVP_dss1";

   function EVP_mdc2 return EVP_MD
     with Import, Convention => C, External_Name => "EVP_mdc2";

   function EVP_ripemd160 return EVP_MD
     with Import, Convention => C, External_Name => "EVP_ripemd160";

   function EVP_sha224 return EVP_MD
     with Import, Convention => C, External_Name => "EVP_sha224";

   function EVP_sha256 return EVP_MD
     with Import, Convention => C, External_Name => "EVP_sha256";

   function EVP_sha384 return EVP_MD
     with Import, Convention => C, External_Name => "EVP_sha384";

   function EVP_sha512 return EVP_MD
     with Import, Convention => C, External_Name => "EVP_sha512";

   function EVP_PKEY_CTX_new (key : EVP_PKEY; e : ENGINE) return EVP_PKEY_CTX
     with Import, Convention => C, External_Name => "EVP_PKEY_CTX_new";

   procedure EVP_PKEY_CTX_free (key : EVP_PKEY_CTX)
     with Import, Convention => C, External_Name => "EVP_PKEY_CTX_free";

   function EVP_PKEY_sign_init (ctx : EVP_PKEY_CTX) return int
     with Import, Convention => C, External_Name => "EVP_PKEY_sign_init";

   function EVP_PKEY_CTX_ctrl
     (ctx : EVP_PKEY_CTX; keytype, optype, cmd, p1 : int; p2 : Pointer)
      return int
     with Import, Convention => C, External_Name => "EVP_PKEY_CTX_ctrl";

   function EVP_PKEY_CTX_set_signature_md
     (ctx : EVP_PKEY_CTX; md : EVP_MD) return int
   is
     (EVP_PKEY_CTX_ctrl
        (ctx, -1, EVP_PKEY_OP_TYPE_SIG, EVP_PKEY_CTRL_MD, 0, Pointer (md)));

   function EVP_PKEY_sign
     (ctx : EVP_PKEY_CTX;
      sig : Pointer; siglen : access size_t;
      tbs : Pointer; tbslen : size_t) return int
     with Import, Convention => C, External_Name => "EVP_PKEY_sign";

   function RSA_new return RSA
     with Import, Convention => C, External_Name => "RSA_new";

   procedure ENV_PKEY_free (key : EVP_PKEY)
     with Import, Convention => C, External_Name => "ENV_PKEY_free";

   procedure RSA_free (key : RSA)
     with Import, Convention => C, External_Name => "RSA_free";

   function RSA_sign
     (kind : int;
      m : Pointer; m_len : unsigned;
      sigret : Pointer; siglen : access unsigned;
      key : RSA) return int
     with Import, Convention => C, External_Name => "RSA_sign";

   type Generate_Key_Callback is access
     procedure (I1, I2 : Integer; Param : Pointer)
   with Convention => C;

   function RSA_generate_key
     (Bits     : int;
      E        : unsigned;
      Callback : Generate_Key_Callback;
      Cb_Arg   : Pointer) return RSA
     with Import, Convention => C, External_Name => "RSA_generate_key";

   function RSA_generate_key_ex
     (key : RSA; bits : int; e : BIGNUM; cb : Pointer) return int
     with Import, Convention => C, External_Name => "RSA_generate_key_ex";

   function EVP_PKEY_size (key : EVP_PKEY) return int
     with Import, Convention => C, External_Name => "EVP_PKEY_size";

   function RSA_size (key : RSA) return int
     with Import, Convention => C, External_Name => "RSA_size";

   function RSA_private_encrypt
     (flen : int; from : Pointer;
      to : Pointer; key : RSA; padding : int) return int
     with Import, Convention => C, External_Name => "RSA_private_encrypt";

   function RSA_public_decrypt
     (flen : int; from : Pointer;
      to : Pointer; key : RSA; padding : int) return int
     with Import, Convention => C, External_Name => "RSA_public_decrypt";

   function DH_new return DH
     with Import, Convention => C, External_Name => "DH_new";

   procedure DH_free (params : DH)
     with Import, Convention => C, External_Name => "DH_free";

   function DH_size (params : DH) return int
     with Import, Convention => C, External_Name => "DH_size";

   function DH_generate_parameters_ex
     (params    : DH;
      prime_len : int;
      generator : int;
      cb        : access constant BN_GENCB) return int
     with Import, Convention => C,
          External_Name => "DH_generate_parameters_ex";

   type pem_password_cb is access function
     (buf      : Cstr.chars_ptr;
      size     : int;
      rwflag   : int;
      userdata : Pointer) return int with Convention => C;

   function PEM_read_bio_PrivateKey
     (bp : BIO_Access;
      x  : access EVP_PKEY;
      cb : pem_password_cb;
      u  : Pointer) return EVP_PKEY
     with Import, Convention => C, External_Name => "PEM_read_bio_PrivateKey";

   function PEM_read_bio_RSAPrivateKey
     (bp : BIO_Access;
      x  : access RSA;
      cb : pem_password_cb;
      u  : Pointer) return RSA
     with Import, Convention => C,
          External_Name => "PEM_read_bio_RSAPrivateKey";

   function PEM_read_bio_DHparams
     (bp : BIO_Access;
      x  : access DH;
      cb : pem_password_cb;
      u  : Pointer) return DH
     with Import, Convention => C, External_Name => "PEM_read_bio_DHparams";

   function PEM_write_bio_DHparams (bp : BIO_Access; x : DH) return int
     with Import, Convention => C, External_Name => "PEM_write_bio_DHparams";

   function PEM_read_bio_X509
     (bp : BIO_Access;
      x  : access X509;
      cb : pem_password_cb;
      u  : Pointer) return X509
     with Import, Convention => C, External_Name => "PEM_read_bio_X509";

   function SSL_CTX_use_PrivateKey (Ctx : SSL_CTX; PK : EVP_PKEY) return int
     with Import, Convention => C,
          External_Name => "SSL_CTX_use_PrivateKey";

   function SSL_CTX_use_RSAPrivateKey (Ctx : SSL_CTX; PK : RSA) return int
     with Import, Convention => C,
          External_Name => "SSL_CTX_use_RSAPrivateKey";

   function SSL_use_RSAPrivateKey
     (SSL : SSL_Handle; Private_Key : RSA) return int
     with Import, Convention => C, External_Name => "SSL_use_RSAPrivateKey";

   function SSL_CTX_use_PrivateKey_file
     (Ctx : SSL_CTX; File : char_array; C_Type : int) return int
     with Import, Convention => C,
          External_Name => "SSL_CTX_use_PrivateKey_file";

   function SSL_use_PrivateKey_file
     (SSL : SSL_Handle; File : char_array; C_Type : int) return int
     with Import, Convention => C, External_Name => "SSL_use_PrivateKey_file";

   function SSL_CTX_use_certificate_file
     (Ctx : SSL_CTX; File : char_array; C_Type : int) return int
     with Import, Convention => C,
          External_Name => "SSL_CTX_use_certificate_file";

   function SSL_CTX_use_certificate_chain_file
     (Ctx : SSL_CTX; File : char_array) return int
     with Import, Convention => C,
          External_Name => "SSL_CTX_use_certificate_chain_file";

   function SSL_use_certificate_file
     (SSL : SSL_Handle; File : char_array; C_Type : int) return int
     with Import, Convention => C, External_Name => "SSL_use_certificate_file";

   function SSL_get_verify_result (SSL : SSL_Handle) return long
     with Import, Convention => C, External_Name => "SSL_get_verify_result";

   function SSL_CTX_check_private_key (Ctx : SSL_CTX) return int
     with Import, Convention => C,
          External_Name => "SSL_CTX_check_private_key";

   procedure SSL_CTX_set_verify
     (Ctx : SSL_CTX; Mode : int; Callback : Pointer)
     with Import, Convention => C, External_Name => "SSL_CTX_set_verify";

   function SSL_CTX_get_verify_mode (Ctx : SSL_CTX) return unsigned
     with Import, Convention => C, External_Name => "SSL_CTX_get_verify_mode";

   function SSL_get_verify_mode (Ctx : SSL_Handle) return unsigned
     with Import, Convention => C, External_Name => "SSL_get_verify_mode";

   function SSL_CTX_load_verify_locations
     (Cts : SSL_CTX; CAfile, CApath : Cstr.chars_ptr) return int
     with Import, Convention => C,
          External_Name => "SSL_CTX_load_verify_locations";

   function SSL_CTX_set1_param
     (Ctx : SSL_CTX; Param : X509_VERIFY_PARAM) return int
     with Import, Convention => C, External_Name => "SSL_CTX_set1_param";

   function SSL_CTX_set_cipher_list
     (Ctx : SSL_CTX; str : Cstr.chars_ptr) return int
     with Import, Convention => C, External_Name => "SSL_CTX_set_cipher_list";

   function SSL_load_client_CA_file
     (file : Cstr.chars_ptr) return STACK_OF_X509_NAME
     with Import, Convention => C, External_Name => "SSL_load_client_CA_file";

   function SSL_dup_CA_list
     (List : STACK_OF_X509_NAME) return  STACK_OF_X509_NAME
     with Import, Convention => C, External_Name => "SSL_dup_CA_list";

   procedure SSL_CTX_set_client_CA_list
     (Ctx : SSL_CTX; list : STACK_OF_X509_NAME)
     with Import, Convention => C,
          External_Name => "SSL_CTX_set_client_CA_list";

   procedure RAND_seed (Buf : Pointer; Num : Integer)
     with Import, Convention => C, External_Name => "RAND_seed";

   function RAND_status return Integer
     with Import, Convention => C, External_Name => "RAND_status";

   procedure RAND_set_rand_method (Method : access Rand_Meth_St)
     with Import, Convention => C, External_Name => "RAND_set_rand_method";

   --  Connection data

   function SSL_CTX_get_ex_new_index
     (Args                          : long;
      Argp                          : Pointer;
      New_Func, Dup_Func, Free_Func : Pointer) return int
     with Import, Convention => C,
          External_Name => "__aws_SSL_CTX_get_ex_new_index";

   function SSL_CTX_set_ex_data
     (Ctx : SSL_CTX; Idx : int; Arg : Pointer) return int
     with Import, Convention => C, External_Name => "SSL_CTX_set_ex_data";

   function SSL_CTX_get_ex_data (Ctx : SSL_CTX; Idx : int) return Pointer
     with Import, Convention => C, External_Name => "SSL_CTX_get_ex_data";

   function SSL_get_ex_data (Ctx : SSL_Handle; Idx : int) return Pointer
     with Import, Convention => C, External_Name => "SSL_get_ex_data";

   type Tmp_RSA_Callback is access function
     (SSL : SSL_Handle; Is_Export : int; Keylength : int) return RSA
     with Convention => C;

   procedure SSL_set_tmp_rsa_callback
     (SSL : SSL_Handle; RSA_CB : Tmp_RSA_Callback)
     with Import, Convention => C,
                  External_Name => "__aws_SSL_set_tmp_rsa_callback";
   --  This call do nothing in version 1.1 and later, remove it on older
   --  versions support terminate.

   type Tmp_DH_Callback is access function
     (SSL : SSL_Handle; Is_Export : int; Keylength : int) return DH
     with Convention => C;

   procedure SSL_CTX_set_tmp_dh_callback
     (Ctx : SSL_CTX; DH_CB : Tmp_DH_Callback)
     with Import, Convention => C,
          External_Name => "SSL_CTX_set_tmp_dh_callback";

   procedure SSL_set_tmp_dh_callback
     (SSL : SSL_Handle; DH_CB : Tmp_DH_Callback)
     with Import, Convention => C, External_Name => "SSL_set_tmp_dh_callback";

   --  Certificate

   function SSL_get_peer_certificate (SSL : SSL_Handle) return X509
     with Import, Convention => C, External_Name => "SSL_get_peer_certificate";

   procedure X509_free (X509 : Thin.X509)
     with Import, Convention => C, External_Name => "X509_free";

   function X509_NAME_oneline
     (Name : X509_NAME; Buf : Pointer; Size : int) return Cstr.chars_ptr
     with Import, Convention => C, External_Name => "X509_NAME_oneline";

   function X509_NAME_print_ex
     (Output : BIO_Access;
      Name   : X509_NAME;
      Indent : int;
      flags  : unsigned_long) return int
     with Import, Convention => C, External_Name => "X509_NAME_print_ex";

   function X509_NAME_entry_count (name : X509_NAME) return int
     with Import, Convention => C, External_Name => "X509_NAME_entry_count";

   function X509_NAME_get_index_by_NID
     (name : X509_NAME; nid : int; lastpos : int) return int
     with Import, Convention => C,
          External_Name => "X509_NAME_get_index_by_NID";

   function X509_NAME_get_entry
     (name : X509_NAME; loc : int) return access X509_NAME_ENTRY
     with Import, Convention => C, External_Name => "X509_NAME_get_entry";

   function X509_NAME_ENTRY_get_data
     (ne : access X509_NAME_ENTRY) return access ASN1_STRING
     with Import, Convention => C, External_Name => "X509_NAME_ENTRY_get_data";

   function X509_NAME_ENTRY_get_object
     (ne : access X509_NAME_ENTRY) return access ASN1_OBJECT
     with Import, Convention => C,
          External_Name => "X509_NAME_ENTRY_get_object";

   function OBJ_obj2txt
     (buf     : Cstr.char_array_access;
      buf_len : int;
      a       : access constant ASN1_OBJECT;
      no_name : int) return int
     with Import, Convention => C, External_Name => "OBJ_obj2txt";

   function OBJ_obj2nid (o : access constant ASN1_OBJECT) return int
     with Import, Convention => C, External_Name => "OBJ_obj2nid";

   function OBJ_nid2sn (n : int) return Cstr.chars_ptr
     with Import, Convention => C, External_Name => "OBJ_nid2sn";

   function X509_get_subject_name (X509 : Thin.X509) return X509_NAME
     with Import, Convention => C, External_Name => "X509_get_subject_name";

   function X509_get_issuer_name (X509 : Thin.X509) return X509_NAME
     with Import, Convention => C, External_Name => "X509_get_issuer_name";

   function X509_get_notAfter
     (X509 : Thin.X509) return access constant ASN1_STRING
     with Import, Convention => C, External_Name => "__aws_X509_get_notAfter";

   function X509_get_notBefore
     (X509 : Thin.X509) return access constant ASN1_STRING
     with Import, Convention => C, External_Name => "__aws_X509_get_notBefore";

   function X509_get_serialNumber
     (X509 : Thin.X509) return access constant ASN1_INTEGER
     with Import, Convention => C, External_Name => "X509_get_serialNumber";

   function X509_get_pubkey (X509 : Thin.X509) return EVP_PKEY
     with Import, Convention => C, External_Name => "X509_get_pubkey";

   function d2i_X509_bio (bp : BIO_Access; x : access X509) return X509
     with Import, Convention => C, External_Name => "d2i_X509_bio";

   function i2d_X509 (x : X509; p : access Pointer) return int
     with Import, Convention => C, External_Name => "i2d_X509";

   procedure SSL_CTX_set_default_verify_paths (Ctx : SSL_CTX)
     with Import, Convention => C,
          External_Name => "SSL_CTX_set_default_verify_paths";

   function SSL_get_ex_data_X509_STORE_CTX_idx return int
     with Import, Convention => C,
          External_Name => "SSL_get_ex_data_X509_STORE_CTX_idx";

   function X509_STORE_CTX_get_ex_data
     (Ctx : X509_STORE_CTX; Idx : int) return SSL_Handle
     with Import, Convention => C,
          External_Name => "X509_STORE_CTX_get_ex_data";

   function X509_STORE_CTX_get_current_cert (Ctx : X509_STORE_CTX) return X509
     with Import, Convention => C,
          External_Name => "X509_STORE_CTX_get_current_cert";

   function X509_STORE_CTX_get_error (Ctx : X509_STORE_CTX) return int
     with Import, Convention => C, External_Name => "X509_STORE_CTX_get_error";

   function X509_STORE_CTX_get_error_depth (Ctx : X509_STORE_CTX) return int
     with Import, Convention => C,
          External_Name => "X509_STORE_CTX_get_error_depth";

   function X509_verify_cert_error_string (Error : long) return Cstr.chars_ptr
     with Import, Convention => C,
          External_Name => "X509_verify_cert_error_string";

   function SSL_CTX_get_cert_store (Ctx : SSL_CTX) return X509_STORE
     with Import, Convention => C, External_Name => "SSL_CTX_get_cert_store";

   function X509_LOOKUP_file return X509_LOOKUP_METHOD
     with Import, Convention => C, External_Name => "X509_LOOKUP_file";

   function X509_STORE_add_lookup
     (Store : X509_STORE; Method : X509_LOOKUP_METHOD) return X509_LOOKUP
     with Import, Convention => C, External_Name => "X509_STORE_add_lookup";

   function X509_load_crl_file
     (Context : X509_LOOKUP; File : Strings.chars_ptr; Typ : int) return int
     with Import, Convention => C, External_Name => "X509_load_crl_file";

   function X509_STORE_set1_param
     (Store : X509_STORE; Param : X509_VERIFY_PARAM) return int
     with Import, Convention => C, External_Name => "X509_STORE_set1_param";

   --  Verify param

   function X509_VERIFY_PARAM_new return X509_VERIFY_PARAM
     with Import, Convention => C, External_Name => "X509_VERIFY_PARAM_new";

   function X509_VERIFY_PARAM_set_flags
     (Param : X509_VERIFY_PARAM; Flags : unsigned_long) return int
     with Import, Convention => C,
          External_Name => "X509_VERIFY_PARAM_set_flags";

   procedure X509_VERIFY_PARAM_free (Param : X509_VERIFY_PARAM)
     with Import, Convention => C, External_Name => "X509_VERIFY_PARAM_free";

   -------------------
   -- ASN1 routines --
   -------------------

   function i2c_ASN1_INTEGER
     (A  : access constant ASN1_INTEGER;
      Pp : access Strings.chars_ptr) return int
     with Import, Convention => C, External_Name => "i2c_ASN1_INTEGER";

   function ASN1_INTEGER_get (A : access constant ASN1_INTEGER) return long
     with Import, Convention => C, External_Name => "ASN1_INTEGER_get";

   function ASN1_INTEGER_to_BN
     (A  : access constant ASN1_INTEGER;
      Pp : access Strings.chars_ptr) return BIGNUM
     with Import, Convention => C, External_Name => "ASN1_INTEGER_to_BN";

   ------------
   -- BIGNUM --
   ------------

   function BN_bn2bin (BN : BIGNUM; Output : Strings.chars_ptr) return int
     with Import, Convention => C, External_Name => "BN_bn2bin";

   function BN_bn2hex (BN : BIGNUM) return Strings.chars_ptr
     with Import, Convention => C, External_Name => "BN_bn2hex";

   function BN_new return BIGNUM
     with Import, Convention => C, External_Name => "BN_new";

   procedure BN_free (BN : BIGNUM)
     with Import, Convention => C, External_Name => "BN_free";

   function BN_set_word (BN : BIGNUM; w : unsigned_long) return int
     with Import, Convention => C, External_Name => "BN_set_word";

   -------------------
   --  BIO routines --
   -------------------

   function BIO_s_socket return BIO_Method_Access
     with Import, Convention => C, External_Name => "BIO_s_socket";

   function BIO_s_mem return BIO_Method_Access
     with Import, Convention => C, External_Name => "BIO_s_mem";

   function BIO_s_file return BIO_Method_Access
     with Import, Convention => C, External_Name => "BIO_s_file";

   function BIO_s_null return BIO_Method_Access
     with Import, Convention => C, External_Name => "BIO_s_null";

   function BIO_new (Method : BIO_Method_St) return BIO_Access
     with Import, Convention => C, External_Name => "BIO_new";

   function BIO_new (Method : BIO_Method_Access) return BIO_Access
     with Import, Convention => C, External_Name => "BIO_new";

   function BIO_new_mem_buf (Buff : Pointer; Len : int) return BIO_Access
     with Import, Convention => C, External_Name => "BIO_new_mem_buf";

   function BIO_int_ctrl
     (Bp   : BIO_Access;
      Cmd  : int;
      Larg : long;
      Iarg : int) return long
     with Import, Convention => C, External_Name => "BIO_int_ctrl";

   procedure BIO_int_ctrl
     (Bp : BIO_Access; Cmd : int; Larg : long; Iarg : int)
     with Import, Convention => C, External_Name => "BIO_int_ctrl";
   --  BIO_set_fd(b,fd,c) BIO_int_ctrl(b,BIO_C_SET_FD,c,fd)

   function BIO_ctrl
     (Bp   : BIO_Access;
      Cmd  : int;
      Larg : long    := 0;
      Parg : Pointer := Null_Pointer) return long
     with Import, Convention => C, External_Name => "BIO_ctrl";

   function BIO_pending (Bp : BIO_Access) return int is
     (int (BIO_ctrl (Bp, BIO_CTRL_PENDING)));

   function BIO_wpending (Bp : BIO_Access) return int is
     (int (BIO_ctrl (Bp, BIO_CTRL_WPENDING)));

   function BIO_flush (Bp : BIO_Access) return int is
     (int (BIO_ctrl (Bp, BIO_CTRL_FLUSH)));

   function BIO_reset (Bp : BIO_Access) return int is
     (int (BIO_ctrl (Bp, BIO_CTRL_RESET)));

   procedure BIO_ctrl
     (Bp : BIO_Access; Cmd : int; Larg : long; Parg : Pointer)
     with Import, Convention => C, External_Name => "BIO_ctrl";
   --  BIO_get_fd(b,c) = BIO_ctrl(b,BIO_C_GET_FD,0,(char *)c)
   --  BIO_set_mem_eof_return(b,v)
   --  = BIO_ctrl(b,BIO_C_SET_BUF_MEM_EOF_RETURN,v,NULL)

   function BIO_ctrl
     (Bp   : BIO_Access;
      Cmd  : int;
      Larg : long;
      Parg : Cstr.chars_ptr) return int
     with Import, Convention => C, External_Name => "BIO_ctrl";

   function BIO_read_filename
     (Bp : BIO_Access; name : Cstr.chars_ptr) return int
   is (BIO_ctrl (Bp, BIO_C_SET_FILENAME, BIO_CLOSE + BIO_FP_READ, name));

   function BIO_write_filename
     (Bp : BIO_Access; name : Cstr.chars_ptr) return int
   is (BIO_ctrl (Bp, BIO_C_SET_FILENAME, BIO_CLOSE + BIO_FP_WRITE, name));

   function BIO_append_filename
     (Bp : BIO_Access; name : Cstr.chars_ptr) return int
   is (BIO_ctrl (Bp, BIO_C_SET_FILENAME, BIO_CLOSE + BIO_FP_APPEND, name));

   function BIO_rw_filename
     (Bp : BIO_Access; name : Cstr.chars_ptr) return int
   is (BIO_ctrl
         (Bp, BIO_C_SET_FILENAME, BIO_CLOSE + BIO_FP_READ + BIO_FP_WRITE,
          name));

   function BIO_read
     (BIO : BIO_Access; Data : Pointer; Len : int) return int
     with Import, Convention => C, External_Name => "BIO_read";

   function BIO_write
     (BIO : BIO_Access; Data : Pointer; Len : int) return int
     with Import, Convention => C, External_Name => "BIO_write";

   function BIO_new_bio_pair
     (bio1 : access BIO_Access; writebuf1 : size_t;
      bio2 : access BIO_Access; writebuf2 : size_t) return int
     with Import, Convention => C, External_Name => "BIO_new_bio_pair";

   function BIO_nread0 (BIO : BIO_Access; Buf : Pointer) return int
     with Import, Convention => C, External_Name => "BIO_nread0";

   function BIO_nread
     (BIO : BIO_Access; Buf : Pointer; Num : int) return int
     with Import, Convention => C, External_Name => "BIO_nread";
   --  Buf is the address of the buffer address

   function BIO_nwrite0 (BIO : BIO_Access; Buf : Pointer) return int
     with Import, Convention => C, External_Name => "BIO_nwrite0";

   function BIO_nwrite
     (BIO : BIO_Access; Buf : Pointer; Num : int) return int
     with Import, Convention => C, External_Name => "BIO_nwrite";
   --  Buf is the address of the buffer address

   function BIO_number_written (BIO : BIO_Access) return unsigned_long
     with Import, Convention => C, External_Name => "BIO_number_written";

   function BIO_number_read (BIO : BIO_Access) return unsigned_long
     with Import, Convention => C, External_Name => "BIO_number_read";

   function BIO_free (BIO : BIO_Access) return int
     with Import, Convention => C, External_Name => "BIO_free";

   procedure BIO_free (BIO : BIO_Access)
     with Import, Convention => C, External_Name => "BIO_free";

   function BIO_get_callback (BIO : BIO_Access) return BIO_callback
     with Import, Convention => C, External_Name => "BIO_get_callback";

   procedure BIO_set_callback (BIO : BIO_Access; callback : BIO_callback)
     with Import, Convention => C, External_Name => "BIO_set_callback";

   function BIO_get_callback_arg (BIO : BIO_Access) return Cstr.chars_ptr
     with Import, Convention => C, External_Name => "BIO_get_callback_arg";

   procedure BIO_set_callback_arg (BIO : BIO_Access; arg : BIO_Access)
     with Import, Convention => C, External_Name => "BIO_set_callback_arg";

   function BIO_debug_callback
     (BIO  : BIO_Access;
      cmd  : int;
      argp : Cstr.chars_ptr;
      argi : int;
      argl : long;
      ret  : long) return long
     with Import, Convention => C, External_Name => "BIO_debug_callback";

   function BIO_get_retry_BIO
     (BIO : BIO_Access; reason : access int) return BIO_Access
     with Import, Convention => C, External_Name => "BIO_get_retry_BIO";

   function BIO_get_retry_reason (BIO : BIO_Access) return int
     with Import, Convention => C, External_Name => "BIO_get_retry_reason";

   function BIO_should_read (BIO : BIO_Access) return Boolean is
     ((unsigned (BIO.Flags) and BIO_FLAGS_READ) > 0);

   function BIO_should_write (BIO : BIO_Access) return Boolean is
    ((unsigned (BIO.Flags) and BIO_FLAGS_WRITE) > 0);

   function BIO_should_io_special (BIO : BIO_Access) return Boolean is
    ((unsigned (BIO.Flags) and BIO_FLAGS_IO_SPECIAL) > 0);

   function BIO_retry_type (BIO : BIO_Access) return unsigned is
    (unsigned (BIO.Flags) and BIO_FLAGS_RWS);

   function BIO_should_retry (BIO : BIO_Access) return Boolean is
    ((unsigned (BIO.Flags) and BIO_FLAGS_SHOULD_RETRY) > 0);

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
   is (SSL_CTX_ctrl (Ctx, SSL_CTRL_SET_SESS_CACHE_MODE, Mode, Null_Pointer));

   function SSL_CTX_set_session_id_context
     (Ctx : SSL_CTX; sid_ctx : Pointer; sid_ctx_len : unsigned) return int
     with Import, Convention => C,
                  External_Name => "SSL_CTX_set_session_id_context";

   procedure SSL_CTX_flush_sessions (Ctx : SSL_CTX; Tm : long)
     with Import, Convention => C, External_Name => "SSL_CTX_flush_sessions";

   type Session_Record is null record;

   type SSL_Session is access all Session_Record;

   function SSL_set_session
     (SSL : SSL_Handle; session : SSL_Session) return int
     with Import, Convention => C, External_Name => "SSL_set_session";

   function SSL_get_session (SSL : SSL_Handle) return SSL_Session
     with Import, Convention => C, External_Name => "SSL_get_session";

   function SSL_get1_session (SSL : SSL_Handle) return SSL_Session
     with Import, Convention => C, External_Name => "SSL_get1_session";

   function SSL_session_reused (SSL : SSL_Handle) return int
     with Import, Convention => C, External_Name => "__aws_SSL_session_reused";

   function SSL_SESSION_get_id
     (s : SSL_Session; len : access unsigned) return Pointer
     with Import, Convention => C, External_Name => "SSL_SESSION_get_id";

   procedure SSL_SESSION_free (session : SSL_Session)
     with Import, Convention => C, External_Name => "SSL_SESSION_free";

end SSL.Thin;
