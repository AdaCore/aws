------------------------------------------------------------------------------
--                            Secure Sockets Layer                          --
--                                                                          --
--                      Copyright (C) 2005-2012, AdaCore                    --
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

--  Thin binding to GNU/TLS

with Interfaces.C.Strings;
with System;

package SSL.Thin is

   use Interfaces;
   package CS renames C.Strings;

   GNUTLS_MAX_ALGORITHM_NUM : constant := 16#0010#;
   GNUTLS_MAX_SESSION_ID    : constant := 16#0020#;

   GNUTLS_KEY_DIGITAL_SIGNATURE : constant := 16#0080#;
   GNUTLS_KEY_NON_REPUDIATION   : constant := 16#0040#;
   GNUTLS_KEY_KEY_ENCIPHERMENT  : constant := 16#0020#;
   GNUTLS_KEY_DATA_ENCIPHERMENT : constant := 16#0010#;
   GNUTLS_KEY_KEY_AGREEMENT     : constant := 8;
   GNUTLS_KEY_KEY_CERT_SIGN     : constant := 4;
   GNUTLS_KEY_CRL_SIGN          : constant := 2;
   GNUTLS_KEY_ENCIPHER_ONLY     : constant := 1;
   GNUTLS_KEY_DECIPHER_ONLY     : constant := 16#8000#;

   GNUTLS_E_SUCCESS                             : constant := 8#0000#;
   GNUTLS_E_UNKNOWN_COMPRESSION_ALGORITHM       : constant := -3;
   GNUTLS_E_UNKNOWN_CIPHER_TYPE                 : constant := -6;
   GNUTLS_E_LARGE_PACKET                        : constant := -7;
   GNUTLS_E_UNSUPPORTED_VERSION_PACKET          : constant := -8;
   GNUTLS_E_UNEXPECTED_PACKET_LENGTH            : constant := -9;
   GNUTLS_E_INVALID_SESSION                     : constant := -10;
   GNUTLS_E_FATAL_ALERT_RECEIVED                : constant := -12;
   GNUTLS_E_UNEXPECTED_PACKET                   : constant := -15;
   GNUTLS_E_WARNING_ALERT_RECEIVED              : constant := -16;
   GNUTLS_E_ERROR_IN_FINISHED_PACKET            : constant := -18;
   GNUTLS_E_UNEXPECTED_HANDSHAKE_PACKET         : constant := -19;
   GNUTLS_E_UNKNOWN_CIPHER_SUITE                : constant := -21;
   GNUTLS_E_UNWANTED_ALGORITHM                  : constant := -22;
   GNUTLS_E_MPI_SCAN_FAILED                     : constant := -23;
   GNUTLS_E_DECRYPTION_FAILED                   : constant := -24;
   GNUTLS_E_MEMORY_ERROR                        : constant := -25;
   GNUTLS_E_DECOMPRESSION_FAILED                : constant := -26;
   GNUTLS_E_COMPRESSION_FAILED                  : constant := -27;
   GNUTLS_E_AGAIN                               : constant := -28;
   GNUTLS_E_EXPIRED                             : constant := -29;
   GNUTLS_E_DB_ERROR                            : constant := -30;
   GNUTLS_E_SRP_PWD_ERROR                       : constant := -31;
   GNUTLS_E_INSUFFICIENT_CREDENTIALS            : constant := -32;
   GNUTLS_E_INSUFICIENT_CREDENTIALS             : constant := -32;
   GNUTLS_E_INSUFFICIENT_CRED                   : constant := -32;
   GNUTLS_E_INSUFICIENT_CRED                    : constant := -32;
   GNUTLS_E_HASH_FAILED                         : constant := -33;
   GNUTLS_E_BASE64_DECODING_ERROR               : constant := -34;
   GNUTLS_E_MPI_PRINT_FAILED                    : constant := -35;
   GNUTLS_E_REHANDSHAKE                         : constant := -37;
   GNUTLS_E_GOT_APPLICATION_DATA                : constant := -38;
   GNUTLS_E_RECORD_LIMIT_REACHED                : constant := -39;
   GNUTLS_E_ENCRYPTION_FAILED                   : constant := -40;
   GNUTLS_E_PK_ENCRYPTION_FAILED                : constant := -44;
   GNUTLS_E_PK_DECRYPTION_FAILED                : constant := -45;
   GNUTLS_E_PK_SIGN_FAILED                      : constant := -46;
   GNUTLS_E_X509_UNSUPPORTED_CRITICAL_EXTENSION : constant := -47;
   GNUTLS_E_KEY_USAGE_VIOLATION                 : constant := -48;
   GNUTLS_E_NO_CERTIFICATE_FOUND                : constant := -49;
   GNUTLS_E_INVALID_REQUEST                     : constant := -50;
   GNUTLS_E_SHORT_MEMORY_BUFFER                 : constant := -51;
   GNUTLS_E_INTERRUPTED                         : constant := -52;
   GNUTLS_E_PUSH_ERROR                          : constant := -53;
   GNUTLS_E_PULL_ERROR                          : constant := -54;
   GNUTLS_E_RECEIVED_ILLEGAL_PARAMETER          : constant := -55;
   GNUTLS_E_REQUESTED_DATA_NOT_AVAILABLE        : constant := -56;
   GNUTLS_E_PKCS1_WRONG_PAD                     : constant := -57;
   GNUTLS_E_RECEIVED_ILLEGAL_EXTENSION          : constant := -58;
   GNUTLS_E_INTERNAL_ERROR                      : constant := -59;
   GNUTLS_E_DH_PRIME_UNACCEPTABLE               : constant := -63;
   GNUTLS_E_FILE_ERROR                          : constant := -64;
   GNUTLS_E_TOO_MANY_EMPTY_PACKETS              : constant := -78;
   GNUTLS_E_UNKNOWN_PK_ALGORITHM                : constant := -80;
   GNUTLS_E_INIT_LIBEXTRA                       : constant := -82;
   GNUTLS_E_LIBRARY_VERSION_MISMATCH            : constant := -83;
   GNUTLS_E_NO_TEMPORARY_RSA_PARAMS             : constant := -84;
   GNUTLS_E_LZO_INIT_FAILED                     : constant := -85;
   GNUTLS_E_NO_COMPRESSION_ALGORITHMS           : constant := -86;
   GNUTLS_E_NO_CIPHER_SUITES                    : constant := -87;
   GNUTLS_E_OPENPGP_GETKEY_FAILED               : constant := -88;
   GNUTLS_E_PK_SIG_VERIFY_FAILED                : constant := -89;
   GNUTLS_E_ILLEGAL_SRP_USERNAME                : constant := -90;
   GNUTLS_E_SRP_PWD_PARSING_ERROR               : constant := -91;
   GNUTLS_E_NO_TEMPORARY_DH_PARAMS              : constant := -93;
   GNUTLS_E_ASN1_ELEMENT_NOT_FOUND              : constant := -67;
   GNUTLS_E_ASN1_IDENTIFIER_NOT_FOUND           : constant := -68;
   GNUTLS_E_ASN1_DER_ERROR                      : constant := -69;
   GNUTLS_E_ASN1_VALUE_NOT_FOUND                : constant := -70;
   GNUTLS_E_ASN1_GENERIC_ERROR                  : constant := -71;
   GNUTLS_E_ASN1_VALUE_NOT_VALID                : constant := -72;
   GNUTLS_E_ASN1_TAG_ERROR                      : constant := -73;
   GNUTLS_E_ASN1_TAG_IMPLICIT                   : constant := -74;
   GNUTLS_E_ASN1_TYPE_ANY_ERROR                 : constant := -75;
   GNUTLS_E_ASN1_SYNTAX_ERROR                   : constant := -76;
   GNUTLS_E_ASN1_DER_OVERFLOW                   : constant := -77;
   GNUTLS_E_OPENPGP_TRUSTDB_VERSION_UNSUPPORTED : constant := -81;
   GNUTLS_E_OPENPGP_UID_REVOKED                 : constant := -79;
   GNUTLS_E_CERTIFICATE_ERROR                   : constant := -43;
   GNUTLS_E_X509_CERTIFICATE_ERROR              : constant := -43;
   GNUTLS_E_CERTIFICATE_KEY_MISMATCH            : constant := -60;
   GNUTLS_E_UNSUPPORTED_CERTIFICATE_TYPE        : constant := -61;
   GNUTLS_E_X509_UNKNOWN_SAN                    : constant := -62;
   GNUTLS_E_OPENPGP_FINGERPRINT_UNSUPPORTED     : constant := -94;
   GNUTLS_E_X509_UNSUPPORTED_ATTRIBUTE          : constant := -95;
   GNUTLS_E_UNKNOWN_HASH_ALGORITHM              : constant := -96;
   GNUTLS_E_UNKNOWN_PKCS_CONTENT_TYPE           : constant := -97;
   GNUTLS_E_UNKNOWN_PKCS_BAG_TYPE               : constant := -98;
   GNUTLS_E_INVALID_PASSWORD                    : constant := -99;
   GNUTLS_E_MAC_VERIFY_FAILED                   : constant := -100;
   GNUTLS_E_CONSTRAINT_ERROR                    : constant := -101;
   GNUTLS_E_BASE64_ENCODING_ERROR               : constant := -201;
   GNUTLS_E_INCOMPATIBLE_GCRYPT_LIBRARY         : constant := -202;
   GNUTLS_E_INCOMPATIBLE_CRYPTO_LIBRARY         : constant := -202;
   GNUTLS_E_INCOMPATIBLE_LIBTASN1_LIBRARY       : constant := -203;
   GNUTLS_E_OPENPGP_KEYRING_ERROR               : constant := -204;
   GNUTLS_E_X509_UNSUPPORTED_OID                : constant := -205;
   GNUTLS_E_RANDOM_FAILED                       : constant := -206;
   GNUTLS_E_UNIMPLEMENTED_FEATURE               : constant := -1250;

   type time_t is new C.long;
   subtype ssize_t is C.int;

   type gnutls_cipher_algorithm_t is
     (GNUTLS_0,
      GNUTLS_CIPHER_NULL,
      GNUTLS_CIPHER_ARCFOUR_128,
      GNUTLS_CIPHER_3DES_CBC,
      GNUTLS_CIPHER_AES_128_CBC,
      GNUTLS_CIPHER_AES_256_CBC,
      GNUTLS_CIPHER_ARCFOUR_40);
   for gnutls_cipher_algorithm_t use
     (GNUTLS_0                  => 0,
      GNUTLS_CIPHER_NULL        => 1,
      GNUTLS_CIPHER_ARCFOUR_128 => 2,
      GNUTLS_CIPHER_3DES_CBC    => 3,
      GNUTLS_CIPHER_AES_128_CBC => 4,
      GNUTLS_CIPHER_AES_256_CBC => 5,
      GNUTLS_CIPHER_ARCFOUR_40  => 6);
   for gnutls_cipher_algorithm_t'Size use C.int'Size;

   type gnutls_kx_algorithm_t is
     (GNUTLS_0,
      GNUTLS_KX_RSA,
      GNUTLS_KX_DHE_DSS,
      GNUTLS_KX_DHE_RSA,
      GNUTLS_KX_ANON_DH,
      GNUTLS_KX_SRP,
      GNUTLS_KX_RSA_EXPORT,
      GNUTLS_KX_SRP_RSA,
      GNUTLS_KX_SRP_DSS);
   for gnutls_kx_algorithm_t use
     (GNUTLS_0             => 0,
      GNUTLS_KX_RSA        => 1,
      GNUTLS_KX_DHE_DSS    => 2,
      GNUTLS_KX_DHE_RSA    => 3,
      GNUTLS_KX_ANON_DH    => 4,
      GNUTLS_KX_SRP        => 5,
      GNUTLS_KX_RSA_EXPORT => 6,
      GNUTLS_KX_SRP_RSA    => 7,
      GNUTLS_KX_SRP_DSS    => 8);
   for gnutls_kx_algorithm_t'Size use C.int'Size;

   type gnutls_credentials_type_t is
     (GNUTLS_CRD_CERTIFICATE,
      GNUTLS_CRD_ANON,
      GNUTLS_CRD_SRP);
   for gnutls_credentials_type_t use
     (GNUTLS_CRD_CERTIFICATE => 1,
      GNUTLS_CRD_ANON        => 2,
      GNUTLS_CRD_SRP         => 3);
   for gnutls_credentials_type_t'Size use C.int'Size;

   type gnutls_mac_algorithm_t is
     (GNUTLS_0,
      GNUTLS_MAC_NULL,
      GNUTLS_MAC_MD5,
      GNUTLS_MAC_SHA,
      GNUTLS_MAC_RMD160);
   for gnutls_mac_algorithm_t use
     (GNUTLS_0          => 0,
      GNUTLS_MAC_NULL   => 1,
      GNUTLS_MAC_MD5    => 2,
      GNUTLS_MAC_SHA    => 3,
      GNUTLS_MAC_RMD160 => 4);
   for gnutls_mac_algorithm_t'Size use C.int'Size;

   type gnutls_digest_algorithm_t is
     (GNUTLS_DIG_NULL,
      GNUTLS_DIG_MD5,
      GNUTLS_DIG_SHA,
      GNUTLS_DIG_RIPEMD160);
   for gnutls_digest_algorithm_t use
     (GNUTLS_DIG_NULL      => 1,
      GNUTLS_DIG_MD5       => 2,
      GNUTLS_DIG_SHA       => 3,
      GNUTLS_DIG_RIPEMD160 => 4);
   for gnutls_digest_algorithm_t'Size use C.int'Size;

   type gnutls_compression_method_t is
     (GNUTLS_0,
      GNUTLS_COMP_NULL,
      GNUTLS_COMP_DEFLATE,
      GNUTLS_COMP_LZO -- only available if gnutls-extra has been initialized
     );
   for gnutls_compression_method_t use
     (GNUTLS_0            => 0,
      GNUTLS_COMP_NULL    => 1,
      GNUTLS_COMP_DEFLATE => 2,
      GNUTLS_COMP_LZO     => 3);
   for gnutls_compression_method_t'Size use C.int'Size;

   type gnutls_connection_end_t is mod 2**4;
   for gnutls_connection_end_t'Size use C.int'Size;

   GNUTLS_SERVER   : constant gnutls_connection_end_t := 1;
   GNUTLS_CLIENT   : constant gnutls_connection_end_t := 2;
   GNUTLS_DATAGRAM : constant gnutls_connection_end_t := 4;
   GNUTLS_NONBLOCK : constant gnutls_connection_end_t := 8;

   type gnutls_certificate_verify_flags is mod 1024;
   for gnutls_certificate_verify_flags'Size use C.int'Size;
   subtype certificate_verify_flags is gnutls_certificate_verify_flags;

   --  GNUTLS_VERIFY_ prefix removed from gnutls_certificate_verify_flags
   --  constants, it is not necessary for Ada type control and namespaces.

   DISABLE_CA_SIGN             : constant certificate_verify_flags := 1;
   ALLOW_X509_V1_CA_CRT        : constant certificate_verify_flags := 2;
   DO_NOT_ALLOW_SAME           : constant certificate_verify_flags := 4;
   ALLOW_ANY_X509_V1_CA_CRT    : constant certificate_verify_flags := 8;
   ALLOW_SIGN_RSA_MD2          : constant certificate_verify_flags := 16;
   ALLOW_SIGN_RSA_MD5          : constant certificate_verify_flags := 32;
   DISABLE_TIME_CHECKS         : constant certificate_verify_flags := 64;
   DISABLE_TRUSTED_TIME_CHECKS : constant certificate_verify_flags := 128;
   DO_NOT_ALLOW_X509_V1_CA_CRT : constant certificate_verify_flags := 256;
   DISABLE_CRL_CHECKS          : constant certificate_verify_flags := 512;

   type gnutls_alert_level_t is (GNUTLS_AL_WARNING, GNUTLS_AL_FATAL);
   for gnutls_alert_level_t use (GNUTLS_AL_WARNING => 1, GNUTLS_AL_FATAL => 2);
   for gnutls_alert_level_t'Size use C.int'Size;

   type gnutls_alert_description_t is
     (GNUTLS_A_CLOSE_NOTIFY,
      GNUTLS_A_UNEXPECTED_MESSAGE,
      GNUTLS_A_BAD_RECORD_MAC,
      GNUTLS_A_DECRYPTION_FAILED,
      GNUTLS_A_RECORD_OVERFLOW,
      GNUTLS_A_DECOMPRESSION_FAILURE,
      GNUTLS_A_HANDSHAKE_FAILURE,
      GNUTLS_A_SSL3_NO_CERTIFICATE,
      GNUTLS_A_BAD_CERTIFICATE,
      GNUTLS_A_UNSUPPORTED_CERTIFICATE,
      GNUTLS_A_CERTIFICATE_REVOKED,
      GNUTLS_A_CERTIFICATE_EXPIRED,
      GNUTLS_A_CERTIFICATE_UNKNOWN,
      GNUTLS_A_ILLEGAL_PARAMETER,
      GNUTLS_A_UNKNOWN_CA,
      GNUTLS_A_ACCESS_DENIED,
      GNUTLS_A_DECODE_ERROR,
      GNUTLS_A_DECRYPT_ERROR,
      GNUTLS_A_EXPORT_RESTRICTION,
      GNUTLS_A_PROTOCOL_VERSION,
      GNUTLS_A_INSUFFICIENT_SECURITY,
      GNUTLS_A_INTERNAL_ERROR,
      GNUTLS_A_USER_CANCELED,
      GNUTLS_A_NO_RENEGOTIATION,
      GNUTLS_A_UNSUPPORTED_EXTENSION,
      GNUTLS_A_CERTIFICATE_UNOBTAINABLE,
      GNUTLS_A_UNRECOGNIZED_NAME,
      GNUTLS_A_UNKNOWN_SRP_USERNAME,
      GNUTLS_A_MISSING_SRP_USERNAME);
   for gnutls_alert_description_t use
     (GNUTLS_A_CLOSE_NOTIFY             => 0,
      GNUTLS_A_UNEXPECTED_MESSAGE       => 10,
      GNUTLS_A_BAD_RECORD_MAC           => 20,
      GNUTLS_A_DECRYPTION_FAILED        => 21,
      GNUTLS_A_RECORD_OVERFLOW          => 22,
      GNUTLS_A_DECOMPRESSION_FAILURE    => 30,
      GNUTLS_A_HANDSHAKE_FAILURE        => 40,
      GNUTLS_A_SSL3_NO_CERTIFICATE      => 41,
      GNUTLS_A_BAD_CERTIFICATE          => 42,
      GNUTLS_A_UNSUPPORTED_CERTIFICATE  => 43,
      GNUTLS_A_CERTIFICATE_REVOKED      => 44,
      GNUTLS_A_CERTIFICATE_EXPIRED      => 45,
      GNUTLS_A_CERTIFICATE_UNKNOWN      => 46,
      GNUTLS_A_ILLEGAL_PARAMETER        => 47,
      GNUTLS_A_UNKNOWN_CA               => 48,
      GNUTLS_A_ACCESS_DENIED            => 49,
      GNUTLS_A_DECODE_ERROR             => 50,
      GNUTLS_A_DECRYPT_ERROR            => 51,
      GNUTLS_A_EXPORT_RESTRICTION       => 60,
      GNUTLS_A_PROTOCOL_VERSION         => 70,
      GNUTLS_A_INSUFFICIENT_SECURITY    => 71,
      GNUTLS_A_INTERNAL_ERROR           => 80,
      GNUTLS_A_USER_CANCELED            => 90,
      GNUTLS_A_NO_RENEGOTIATION         => 100,
      GNUTLS_A_UNSUPPORTED_EXTENSION    => 110,
      GNUTLS_A_CERTIFICATE_UNOBTAINABLE => 111,
      GNUTLS_A_UNRECOGNIZED_NAME        => 112,
      GNUTLS_A_UNKNOWN_SRP_USERNAME     => 120,
      GNUTLS_A_MISSING_SRP_USERNAME     => 121);
   for gnutls_alert_description_t'Size use C.int'Size;

   type gnutls_handshake_description_t is
     (GNUTLS_HANDSHAKE_HELLO_REQUEST,
      GNUTLS_HANDSHAKE_CLIENT_HELLO,
      GNUTLS_HANDSHAKE_SERVER_HELLO,
      GNUTLS_HANDSHAKE_CERTIFICATE_PKT,
      GNUTLS_HANDSHAKE_SERVER_KEY_EXCHANGE,
      GNUTLS_HANDSHAKE_CERTIFICATE_REQUEST,
      GNUTLS_HANDSHAKE_SERVER_HELLO_DONE,
      GNUTLS_HANDSHAKE_CERTIFICATE_VERIFY,
      GNUTLS_HANDSHAKE_CLIENT_KEY_EXCHANGE,
      GNUTLS_HANDSHAKE_FINISHED);
   for gnutls_handshake_description_t use
     (GNUTLS_HANDSHAKE_HELLO_REQUEST       => 0,
      GNUTLS_HANDSHAKE_CLIENT_HELLO        => 1,
      GNUTLS_HANDSHAKE_SERVER_HELLO        => 2,
      GNUTLS_HANDSHAKE_CERTIFICATE_PKT     => 11,
      GNUTLS_HANDSHAKE_SERVER_KEY_EXCHANGE => 12,
      GNUTLS_HANDSHAKE_CERTIFICATE_REQUEST => 13,
      GNUTLS_HANDSHAKE_SERVER_HELLO_DONE   => 14,
      GNUTLS_HANDSHAKE_CERTIFICATE_VERIFY  => 15,
      GNUTLS_HANDSHAKE_CLIENT_KEY_EXCHANGE => 16,
      GNUTLS_HANDSHAKE_FINISHED            => 20);
   for gnutls_handshake_description_t'Size use C.int'Size;

   type gnutls_certificate_request_t is
     (GNUTLS_CERT_IGNORE,
      GNUTLS_CERT_REQUEST,
      GNUTLS_CERT_REQUIRE);
   for gnutls_certificate_request_t'Size use C.int'Size;

   type gnutls_openpgp_key_status_t is
     (GNUTLS_OPENPGP_KEY,
      GNUTLS_OPENPGP_KEY_FINGERPRINT);
   for gnutls_openpgp_key_status_t'Size use C.int'Size;

   type gnutls_close_request_t is (GNUTLS_SHUT_RDWR, GNUTLS_SHUT_WR);
   for gnutls_close_request_t use
     (GNUTLS_SHUT_RDWR => 0,
      GNUTLS_SHUT_WR   => 1);
   for gnutls_close_request_t'Size use C.int'Size;

   type gnutls_protocol_t is
     (GNUTLS_0, GNUTLS_SSL3, GNUTLS_TLS1_0, GNUTLS_TLS1_1);
   for gnutls_protocol_t use
     (GNUTLS_0      => 0,
      GNUTLS_SSL3   => 1,
      GNUTLS_TLS1_0 => 2,
      GNUTLS_TLS1_1 => 3);
   for gnutls_protocol_t'Size use C.int'Size;

   type gnutls_certificate_type_t is
     (GNUTLS_0, GNUTLS_CRT_X509, GNUTLS_CRT_OPENPGP);
   for gnutls_certificate_type_t use
     (GNUTLS_0 => 0, GNUTLS_CRT_X509 => 1, GNUTLS_CRT_OPENPGP => 2);
   for gnutls_certificate_type_t'Size use C.int'Size;

   type gnutls_x509_crt_fmt_t is (GNUTLS_X509_FMT_DER, GNUTLS_X509_FMT_PEM);
   for gnutls_x509_crt_fmt_t'Size use C.int'Size;

   type gnutls_pk_algorithm_t is
     (GNUTLS_PK_RSA,
      GNUTLS_PK_DSA,
      GNUTLS_PK_UNKNOWN);
   for gnutls_pk_algorithm_t use
     (GNUTLS_PK_RSA     => 1,
      GNUTLS_PK_DSA     => 2,
      GNUTLS_PK_UNKNOWN => 255);
   for gnutls_pk_algorithm_t'Size use C.int'Size;

   type gnutls_sign_algorithm_t is
     (GNUTLS_SIGN_RSA_SHA,
      GNUTLS_SIGN_DSA_SHA,
      GNUTLS_SIGN_RSA_MD5,
      GNUTLS_SIGN_RSA_MD2,
      GNUTLS_SIGN_UNKNOWN);
   for gnutls_sign_algorithm_t use
     (GNUTLS_SIGN_RSA_SHA => 1,
      GNUTLS_SIGN_DSA_SHA => 2,
      GNUTLS_SIGN_RSA_MD5 => 3,
      GNUTLS_SIGN_RSA_MD2 => 4,
      GNUTLS_SIGN_UNKNOWN => 255);
   for gnutls_sign_algorithm_t'Size use C.int'Size;

   type gnutls_server_name_type_t is (GNUTLS_NAME_DNS);
   for gnutls_server_name_type_t use (GNUTLS_NAME_DNS => 1);
   for gnutls_server_name_type_t'Size use C.int'Size;

   subtype a_c_signed_char_t is System.Address;
   type a_size_t is access all C.size_t;
   type gnutls_transport_ptr_t is new C.int;

   type gnutls_session_t;
   type gnutls_datum_t;
   type gnutls_retr_st;

   type gnutls_srp_server_credentials_function is new System.Address;
   type gnutls_srp_client_credentials_function is new System.Address;
   type gnutls_certificate_server_retrieve_function is new System.Address;
   type gnutls_params_function is new System.Address;

   type STRUCT_DSTRUCT;

   type gnutls_session_t is access all STRUCT_DSTRUCT;
   type gnutls_dh_params_t is access all STRUCT_DSTRUCT;
   type gnutls_rsa_params_t is access all STRUCT_DSTRUCT;
   type gnutls_certificate_credentials_t is access all STRUCT_DSTRUCT;
   type gnutls_anon_server_credentials_t is access all STRUCT_DSTRUCT;
   type gnutls_anon_client_credentials_t is access all STRUCT_DSTRUCT;
   type a_gnutls_datum_t is access constant gnutls_datum_t;
   type gnutls_x509_privkey_t is access all STRUCT_DSTRUCT;
   type gnutls_x509_crl_t is access all STRUCT_DSTRUCT;
   type gnutls_x509_crt_t is access all STRUCT_DSTRUCT;
   type a_gnutls_x509_crt_t is access all gnutls_x509_crt_t;
   type a_gnutls_x509_crl_t is access all gnutls_x509_crl_t;
   type gnutls_srp_server_credentials_t is access all STRUCT_DSTRUCT;
   type gnutls_srp_client_credentials_t is access all STRUCT_DSTRUCT;
   type gnutls_openpgp_key_t is access all STRUCT_DSTRUCT;
   type gnutls_openpgp_privkey_t is access all STRUCT_DSTRUCT;
   type gnutls_pubkey_t is access all STRUCT_DSTRUCT;
   type gnutls_privkey_t is access all STRUCT_DSTRUCT;

   type gnutls_retr_st is record
      cert_type  : gnutls_certificate_type_t;
      cert_x509  : a_gnutls_x509_crt_t;
      ncerts     : C.int;              -- one for pgp keys
      key_x509   : gnutls_x509_privkey_t;
      deinit_all : C.unsigned;         -- if non zero all keys will be deinited
   end record;
   pragma Convention (C, gnutls_retr_st);

   type gnutls_datum_t is record
      data : a_c_signed_char_t;
      size : C.unsigned;
   end record;
   pragma Convention (C, gnutls_datum_t);

   type gnutls_pcert_st is record
      pubkey : gnutls_pubkey_t;
      cert   : gnutls_datum_t;
      c_type : gnutls_certificate_type_t;
   end record;
   pragma Convention (C, gnutls_pcert_st);

   type a_gnutls_pcert_st is access all gnutls_pcert_st;

   type gnutls_certificate_retrieve_function2 is access function
     (Session         : gnutls_session_t;
      req_ca_rdn      : access constant gnutls_datum_t;
      nreqs           : C.int;
      pk_algos        : access constant gnutls_pk_algorithm_t;
      pk_algos_length : C.int;
      st              : access a_gnutls_pcert_st;
      pcert_length    : access C.unsigned;
      privkey         : access gnutls_privkey_t) return C.int;
   pragma Convention (C, gnutls_certificate_retrieve_function2);
   --  !!! This ñallback in version 3.0.3 defined different in man and in file
   --  gnutls.h. This is a new definition. Old definition was ended by field
   --  st : access gnutls_pcert_st.

   type gnutls_certificate_client_retrieve_function is access function
     (Session         : gnutls_session_t;
      Req_CA_DN       : access constant gnutls_datum_t;
      nreqs           : C.int;
      pk_algos        : access constant gnutls_pk_algorithm_t;
      pk_algos_length : C.int;
      st              : access gnutls_retr_st) return C.int;
   pragma Convention (C, gnutls_certificate_client_retrieve_function);

   type gnutls_certificate_verify_function is access function
     (Session : gnutls_session_t) return C.int;
   pragma Convention (C, gnutls_certificate_verify_function);

   type gnutls_db_store_func is access function
     (p1   : System.Address;
      key  : gnutls_datum_t;
      data : gnutls_datum_t) return C.int;
   pragma Convention (C, gnutls_db_store_func);

   type gnutls_db_remove_func is access function
     (p1  : System.Address; key : gnutls_datum_t) return C.int;
   pragma Convention (C, gnutls_db_remove_func);

   type gnutls_db_retr_func is access function
     (p1  : System.Address; key : gnutls_datum_t) return gnutls_datum_t;
   pragma Convention (C, gnutls_db_retr_func);

   type STRUCT_DSTRUCT is null record;
   pragma Convention (C, STRUCT_DSTRUCT);

   type gnutls_alloc_function is access function
     (p1 : C.size_t) return System.Address;
   pragma Convention (C, gnutls_alloc_function);

   type gnutls_calloc_function is access function
     (p1 : C.size_t; p2 : C.size_t) return System.Address;
   pragma Convention (C, gnutls_calloc_function);

   type gnutls_is_secure_function is access function
     (p1 : System.Address) return C.int;
   pragma Convention (C, gnutls_is_secure_function);

   type gnutls_free_function is access procedure (p1 : System.Address);
   pragma Convention (C, gnutls_free_function);

   type gnutls_realloc_function is access function
     (p1 : System.Address; p2 : C.size_t) return System.Address;
   pragma Convention (C, gnutls_realloc_function);

   type gnutls_log_func is access procedure
     (level : C.int; text : CS.chars_ptr);
   pragma Convention (C, gnutls_log_func);

   gnutls_malloc                   : constant gnutls_alloc_function;
   gnutls_calloc                   : constant gnutls_calloc_function;
   gnutls_free                     : constant gnutls_free_function;
   gnutls_strdup                   : constant System.Address;
   gnutls_srp_2048_group_prime     : constant gnutls_datum_t;
   gnutls_srp_2048_group_generator : constant gnutls_datum_t;
   gnutls_srp_1536_group_prime     : constant gnutls_datum_t;
   gnutls_srp_1536_group_generator : constant gnutls_datum_t;
   gnutls_srp_1024_group_prime     : constant gnutls_datum_t;
   gnutls_srp_1024_group_generator : constant gnutls_datum_t;

   function gnutls_pk_algorithm_get_name
     (algorithm : gnutls_pk_algorithm_t) return CS.chars_ptr;

   function gnutls_sign_algorithm_get_name
     (algorithm : gnutls_sign_algorithm_t) return CS.chars_ptr;

   function gnutls_init
     (session : access gnutls_session_t;
      con_end : gnutls_connection_end_t) return C.int;

   procedure gnutls_deinit (session : gnutls_session_t);

   function gnutls_bye
     (session : gnutls_session_t;
      how     : gnutls_close_request_t) return C.int;

   function gnutls_handshake (session : gnutls_session_t) return C.int;

   function gnutls_rehandshake (session : gnutls_session_t) return C.int;

   function gnutls_alert_get
     (session : gnutls_session_t) return gnutls_alert_description_t;

   function gnutls_alert_send
     (p1 : gnutls_session_t;
      p2 : gnutls_alert_level_t;
      p3 : gnutls_alert_description_t) return C.int;

   function gnutls_alert_send_appropriate
     (session : gnutls_session_t; err : C.int) return C.int;

   function gnutls_alert_get_name
     (alert : gnutls_alert_description_t) return  CS.chars_ptr;

   function gnutls_cipher_get
     (session : gnutls_session_t) return gnutls_cipher_algorithm_t;

   function gnutls_kx_get
     (session : gnutls_session_t) return gnutls_kx_algorithm_t;

   function gnutls_mac_get
     (session : gnutls_session_t) return gnutls_mac_algorithm_t;

   function gnutls_compression_get
     (session : gnutls_session_t) return gnutls_compression_method_t;

   function gnutls_certificate_type_get
     (session : gnutls_session_t) return gnutls_certificate_type_t;

   function gnutls_cipher_get_key_size
     (algorithm : gnutls_cipher_algorithm_t) return C.size_t;

   function gnutls_cipher_get_name
     (p1 : gnutls_cipher_algorithm_t) return CS.chars_ptr;

   function gnutls_mac_get_name
     (p1 : gnutls_mac_algorithm_t) return CS.chars_ptr;

   function gnutls_compression_get_name
     (p1 : gnutls_compression_method_t) return CS.chars_ptr;

   function gnutls_kx_get_name
     (algorithm : gnutls_kx_algorithm_t) return CS.chars_ptr;

   function gnutls_certificate_type_get_name
     (c_type : gnutls_certificate_type_t) return CS.chars_ptr;

   function gnutls_error_is_fatal (error : C.int) return C.int;

   function gnutls_error_to_alert
     (err : C.int; level : access C.int) return C.int;

   procedure gnutls_perror (error : C.int);

   function gnutls_strerror (error : C.int) return CS.chars_ptr;

   procedure gnutls_handshake_set_private_extensions
     (session : gnutls_session_t; allow : C.int);

   function gnutls_handshake_get_last_out
     (session : gnutls_session_t) return gnutls_handshake_description_t;

   function gnutls_handshake_get_last_in
     (session : gnutls_session_t) return gnutls_handshake_description_t;

   function gnutls_record_send
     (session    : gnutls_session_t;
      data       : System.Address;
      sizeofdata : C.size_t) return ssize_t;

   function gnutls_record_recv
     (session    : gnutls_session_t;
      data       : System.Address;
      sizeofdata : C.size_t) return ssize_t;

   function gnutls_record_get_direction
     (session : gnutls_session_t) return C.int;

   function gnutls_record_get_max_size
     (session : gnutls_session_t) return C.size_t;

   function gnutls_record_set_max_size
     (session : gnutls_session_t; size : C.size_t) return C.size_t;

   function gnutls_record_check_pending
     (session : gnutls_session_t) return C.size_t;

   function gnutls_server_name_set
     (session     : gnutls_session_t;
      c_type      : gnutls_server_name_type_t;
      name        : System.Address;
      name_length : C.size_t) return C.int;

   function gnutls_server_name_get
     (session     : gnutls_session_t;
      data        : System.Address;
      data_length : a_size_t;
      c_type      : access C.unsigned;
      indx        : C.unsigned) return C.int;

   function gnutls_cipher_set_priority
     (session : gnutls_session_t; p2 : System.Address) return C.int;

   function gnutls_mac_set_priority
     (session : gnutls_session_t; p2 : System.Address) return C.int;

   function gnutls_compression_set_priority
     (session : gnutls_session_t; p2 : System.Address) return C.int;

   function gnutls_kx_set_priority
     (session : gnutls_session_t; p2 : System.Address) return C.int;

   function gnutls_protocol_set_priority
     (session : gnutls_session_t; p2 : System.Address) return C.int;

   function gnutls_certificate_type_set_priority
     (session : gnutls_session_t; p2 : System.Address) return C.int;

   function gnutls_set_default_priority
     (session : gnutls_session_t) return C.int;

   function gnutls_set_default_export_priority
     (session : gnutls_session_t) return C.int;

   function gnutls_cipher_suite_get_name
     (kx_algorithm     : gnutls_kx_algorithm_t;
      cipher_algorithm : gnutls_cipher_algorithm_t;
      mac_algorithm    : gnutls_mac_algorithm_t) return CS.chars_ptr;

   function gnutls_protocol_get_version
     (session : gnutls_session_t) return gnutls_protocol_t;

   function gnutls_protocol_get_name
     (version : gnutls_protocol_t) return CS.chars_ptr;

   function gnutls_session_set_data
     (session           : gnutls_session_t;
      session_data      : System.Address;
      session_data_size : C.size_t) return C.int;

   function gnutls_session_get_data
     (session           : gnutls_session_t;
      session_data      : System.Address;
      session_data_size : a_size_t) return C.int;

   function gnutls_session_get_id
     (session         : gnutls_session_t;
      session_id      : System.Address;
      session_id_size : a_size_t) return C.int;

   function gnutls_session_is_resumed
     (session : gnutls_session_t) return C.int;

   procedure gnutls_db_set_cache_expiration
     (session : gnutls_session_t; seconds : C.int);

   procedure gnutls_db_remove_session (session : gnutls_session_t);

   procedure gnutls_db_set_retrieve_function
     (p1 : gnutls_session_t; p2 : gnutls_db_retr_func);

   procedure gnutls_db_set_remove_function
     (p1 : gnutls_session_t; p2 : gnutls_db_remove_func);

   procedure gnutls_db_set_store_function
     (p1 : gnutls_session_t; p2 : gnutls_db_store_func);

   procedure gnutls_db_set_ptr
     (p1 : gnutls_session_t; db_ptr : System.Address);

   function gnutls_db_get_ptr (p1 : gnutls_session_t) return System.Address;

   function gnutls_db_check_entry
     (session       : gnutls_session_t;
      session_entry : gnutls_datum_t) return C.int;

   procedure gnutls_handshake_set_max_packet_length
     (session : gnutls_session_t; max : C.int);

   function gnutls_check_version (p1 : CS.chars_ptr) return CS.chars_ptr;

   function gnutls_credentials_clear (session : gnutls_session_t) return C.int;

   function gnutls_credentials_set
     (p1     : gnutls_session_t;
      c_type : gnutls_credentials_type_t;
      cred   : System.Address) return C.int;

   function gnutls_credentials_set
     (p1     : gnutls_session_t;
      c_type : gnutls_credentials_type_t := GNUTLS_CRD_ANON;
      cred   : gnutls_anon_client_credentials_t) return C.int;

   function gnutls_credentials_set
     (p1     : gnutls_session_t;
      c_type : gnutls_credentials_type_t := GNUTLS_CRD_ANON;
      cred   : gnutls_anon_server_credentials_t) return C.int;

   function gnutls_credentials_set
     (p1     : gnutls_session_t;
      c_type : gnutls_credentials_type_t := GNUTLS_CRD_CERTIFICATE;
      cred   : gnutls_certificate_credentials_t) return C.int;

   procedure gnutls_anon_free_server_credentials
     (sc : gnutls_anon_server_credentials_t);

   function gnutls_anon_allocate_server_credentials
     (sc : access gnutls_anon_server_credentials_t) return C.int;

   procedure gnutls_anon_set_server_dh_params
     (res       : gnutls_anon_server_credentials_t;
      dh_params : gnutls_dh_params_t);

   procedure gnutls_anon_free_client_credentials
     (sc : gnutls_anon_client_credentials_t);

   function gnutls_anon_allocate_client_credentials
     (sc : access gnutls_anon_client_credentials_t) return C.int;

   procedure gnutls_certificate_free_credentials
     (sc : gnutls_certificate_credentials_t);

   function gnutls_certificate_allocate_credentials
     (sc : access gnutls_certificate_credentials_t) return C.int;

   procedure gnutls_certificate_free_keys
     (sc : gnutls_certificate_credentials_t);

   procedure gnutls_certificate_free_cas
     (sc : gnutls_certificate_credentials_t);

   procedure gnutls_certificate_free_ca_names
     (sc : gnutls_certificate_credentials_t);

   procedure gnutls_certificate_free_crls
     (sc : gnutls_certificate_credentials_t);

   procedure gnutls_certificate_set_dh_params
     (res : gnutls_certificate_credentials_t;
      p2  : gnutls_dh_params_t);

   procedure gnutls_certificate_set_rsa_export_params
     (res        : gnutls_certificate_credentials_t;
      rsa_params : gnutls_rsa_params_t);

   procedure gnutls_certificate_set_verify_flags
     (res   : gnutls_certificate_credentials_t;
      flags : C.unsigned);

   procedure gnutls_certificate_set_verify_limits
     (res       : gnutls_certificate_credentials_t;
      max_bits  : C.unsigned;
      max_depth : C.unsigned);

   procedure gnutls_certificate_set_verify_function
     (cred : gnutls_certificate_credentials_t;
      func : gnutls_certificate_verify_function);

   function gnutls_certificate_set_x509_trust_file
     (res    : gnutls_certificate_credentials_t;
      CAFILE : CS.chars_ptr;
      p3     : gnutls_x509_crt_fmt_t) return C.int;

   function gnutls_certificate_set_x509_trust_mem
     (res : gnutls_certificate_credentials_t;
      CA  : a_gnutls_datum_t;
      p3  : gnutls_x509_crt_fmt_t) return C.int;

   function gnutls_certificate_set_x509_crl_file
     (res     : gnutls_certificate_credentials_t;
      crlfile : CS.chars_ptr;
      c_type  : gnutls_x509_crt_fmt_t) return C.int;

   function gnutls_certificate_set_x509_crl_mem
     (res    : gnutls_certificate_credentials_t;
      CRL    : a_gnutls_datum_t;
      c_type : gnutls_x509_crt_fmt_t) return C.int;

   function gnutls_certificate_set_x509_key_file
     (res      : gnutls_certificate_credentials_t;
      CERTFILE : CS.chars_ptr;
      KEYFILE  : CS.chars_ptr;
      p4       : gnutls_x509_crt_fmt_t) return C.int;

   function gnutls_certificate_set_x509_key_mem
     (res  : gnutls_certificate_credentials_t;
      cert : a_gnutls_datum_t;
      key  : a_gnutls_datum_t;
      p4   : gnutls_x509_crt_fmt_t) return C.int;

   function gnutls_certificate_set_x509_key
     (res            : gnutls_certificate_credentials_t;
      cert_list      : a_gnutls_x509_crt_t;
      cert_list_size : C.int;
      key            : gnutls_x509_privkey_t) return C.int;

   function gnutls_certificate_set_x509_trust
     (res          : gnutls_certificate_credentials_t;
      ca_list      : a_gnutls_x509_crt_t;
      ca_list_size : C.int) return C.int;

   function gnutls_certificate_set_x509_crl
     (res           : gnutls_certificate_credentials_t;
      crl_list      : a_gnutls_x509_crl_t;
      crl_list_size : C.int) return C.int;

   function gnutls_x509_crt_init
     (cert : access gnutls_x509_crt_t) return C.int;

   procedure gnutls_x509_crt_deinit (cert : gnutls_x509_crt_t);

   function gnutls_x509_crt_import
     (cert   : gnutls_x509_crt_t;
      data   : gnutls_datum_t;
      format : gnutls_x509_crt_fmt_t) return C.int;

   function gnutls_x509_crt_get_dn
     (cert       : gnutls_x509_crt_t;
      buf        : CS.chars_ptr;
      sizeof_buf : access C.size_t) return C.int;

   function gnutls_x509_crt_get_issuer_dn
     (cert       : gnutls_x509_crt_t;
      buf        : CS.chars_ptr;
      sizeof_buf : access C.size_t) return C.int;

   function gnutls_global_init return C.int;

   procedure gnutls_global_deinit;

   procedure gnutls_global_set_mem_functions
     (p1 : gnutls_alloc_function;
      p2 : gnutls_alloc_function;
      p3 : gnutls_is_secure_function;
      p4 : gnutls_realloc_function;
      p5 : gnutls_free_function);

   procedure gnutls_global_set_log_function (log_func : gnutls_log_func);

   procedure gnutls_global_set_log_level (level : C.int);

   function gnutls_dh_params_init
     (p1 : access gnutls_dh_params_t) return C.int;

   procedure gnutls_dh_params_deinit (p1 : gnutls_dh_params_t);

   function gnutls_dh_params_import_raw
     (dh_params : gnutls_dh_params_t;
      prime     : a_gnutls_datum_t;
      generator : a_gnutls_datum_t) return C.int;

   function gnutls_dh_params_import_pkcs3
     (params       : gnutls_dh_params_t;
      pkcs3_params : a_gnutls_datum_t;
      format       : gnutls_x509_crt_fmt_t) return C.int;

   function gnutls_dh_params_generate2
     (params : gnutls_dh_params_t;
      bits   : C.int) return C.int;

   function gnutls_dh_params_export_pkcs3
     (params           : gnutls_dh_params_t;
      format           : gnutls_x509_crt_fmt_t;
      params_data      : a_c_signed_char_t;
      params_data_size : a_size_t) return C.int;

   function gnutls_dh_params_export_raw
     (params    : gnutls_dh_params_t;
      prime     : a_gnutls_datum_t;
      generator : a_gnutls_datum_t;
      bits      : access C.unsigned) return C.int;

   function gnutls_dh_params_cpy
     (dst : gnutls_dh_params_t;
      src : gnutls_dh_params_t) return C.int;

   function gnutls_rsa_params_init
     (rsa_params : access gnutls_rsa_params_t) return C.int;

   procedure gnutls_rsa_params_deinit (rsa_params : gnutls_rsa_params_t);

   function gnutls_rsa_params_cpy
     (dst : gnutls_rsa_params_t;
      src : gnutls_rsa_params_t) return C.int;

   function gnutls_rsa_params_import_raw
     (rsa_params : gnutls_rsa_params_t;
      m          : a_gnutls_datum_t;
      e          : a_gnutls_datum_t;
      d          : a_gnutls_datum_t;
      p          : a_gnutls_datum_t;
      q          : a_gnutls_datum_t;
      u          : a_gnutls_datum_t) return C.int;

   function gnutls_rsa_params_generate2
     (params : gnutls_rsa_params_t;
      bits   : C.int) return C.int;

   function gnutls_rsa_params_export_raw
     (params : gnutls_rsa_params_t;
      m      : a_gnutls_datum_t;
      e      : a_gnutls_datum_t;
      d      : a_gnutls_datum_t;
      p      : a_gnutls_datum_t;
      q      : a_gnutls_datum_t;
      u      : a_gnutls_datum_t;
      bits   : access C.unsigned) return C.int;

   function gnutls_rsa_params_export_pkcs1
     (params           : gnutls_rsa_params_t;
      format           : gnutls_x509_crt_fmt_t;
      params_data      : a_c_signed_char_t;
      params_data_size : a_size_t) return C.int;

   function gnutls_rsa_params_import_pkcs1
     (params       : gnutls_rsa_params_t;
      pkcs1_params : a_gnutls_datum_t;
      format       : gnutls_x509_crt_fmt_t) return C.int;

   function gnutls_transport_get_ptr
     (session : gnutls_session_t) return gnutls_transport_ptr_t;

   procedure gnutls_transport_get_ptr2
     (session  : gnutls_session_t;
      recv_ptr : gnutls_transport_ptr_t;
      send_ptr : gnutls_transport_ptr_t);

   procedure gnutls_transport_set_lowat
     (session : gnutls_session_t; num : C.int);

   procedure gnutls_transport_set_ptr
     (session : gnutls_session_t; ptr : gnutls_transport_ptr_t);

   procedure gnutls_transport_set_push_function
     (session : gnutls_session_t; push_func : System.Address);

   procedure gnutls_transport_set_pull_function
     (session : gnutls_session_t; pull_func : System.Address);

   procedure gnutls_session_set_ptr
     (session : gnutls_session_t; ptr : System.Address);

   function gnutls_session_get_ptr
     (session : gnutls_session_t) return System.Address;

   procedure gnutls_openpgp_send_key
     (session : gnutls_session_t; status : gnutls_openpgp_key_status_t);

   function gnutls_fingerprint
     (algo        : gnutls_digest_algorithm_t;
      data        : a_gnutls_datum_t;
      result      : System.Address;
      result_size : a_size_t) return C.int;

   procedure gnutls_srp_free_client_credentials
     (sc : gnutls_srp_client_credentials_t);

   function gnutls_srp_allocate_client_credentials
     (sc : access gnutls_srp_client_credentials_t) return C.int;

   function gnutls_srp_set_client_credentials
     (res      : gnutls_srp_client_credentials_t;
      username : CS.chars_ptr;
      password : CS.chars_ptr) return C.int;

   procedure gnutls_srp_free_server_credentials
     (sc : gnutls_srp_server_credentials_t);

   function gnutls_srp_allocate_server_credentials
     (sc : access gnutls_srp_server_credentials_t) return C.int;

   function gnutls_srp_set_server_credentials_file
     (res                : gnutls_srp_server_credentials_t;
      password_file      : CS.chars_ptr;
      password_conf_file : CS.chars_ptr) return C.int;

   function gnutls_srp_server_get_username
     (state : gnutls_session_t) return CS.chars_ptr;

   function gnutls_srp_verifier
     (username : CS.chars_ptr;
      password : CS.chars_ptr;
      salt     : a_gnutls_datum_t;
      g        : a_gnutls_datum_t;
      n        : a_gnutls_datum_t;
      res      : a_gnutls_datum_t) return C.int;

   procedure gnutls_srp_set_server_credentials_function
     (p1 : gnutls_srp_server_credentials_t;
      p2 : gnutls_srp_server_credentials_function);

   procedure gnutls_srp_set_client_credentials_function
     (p1 : gnutls_srp_client_credentials_t;
      p2 : gnutls_srp_client_credentials_function);

   function gnutls_auth_get_type
     (session : gnutls_session_t) return gnutls_credentials_type_t;

   function gnutls_auth_server_get_type
     (session : gnutls_session_t) return gnutls_credentials_type_t;

   function gnutls_auth_client_get_type
     (session : gnutls_session_t) return gnutls_credentials_type_t;

   procedure gnutls_dh_set_prime_bits
     (session : gnutls_session_t; bits : C.int);

   function gnutls_dh_get_secret_bits (p1 : gnutls_session_t) return C.int;

   function gnutls_dh_get_peers_public_bits
     (p1 : gnutls_session_t) return C.int;

   function gnutls_dh_get_prime_bits (p1 : gnutls_session_t) return C.int;

   function gnutls_dh_get_group
     (p1    : gnutls_session_t;
      gen   : a_gnutls_datum_t;
      prime : a_gnutls_datum_t) return C.int;

   function gnutls_dh_get_pubkey
     (p1 : gnutls_session_t; pub : a_gnutls_datum_t) return C.int;

   function gnutls_rsa_export_get_pubkey
     (session : gnutls_session_t;
      exp     : a_gnutls_datum_t;
      c_mod   : a_gnutls_datum_t) return C.int;

   function gnutls_rsa_export_get_modulus_bits
     (session : gnutls_session_t) return C.int;

   procedure gnutls_certificate_set_retrieve_function2
     (cred : gnutls_certificate_credentials_t;
      func : gnutls_certificate_retrieve_function2);

   procedure gnutls_certificate_client_set_retrieve_function
     (cred : gnutls_certificate_credentials_t;
      func : gnutls_certificate_client_retrieve_function);

   procedure gnutls_certificate_server_set_retrieve_function
     (cred : gnutls_certificate_credentials_t;
      func : gnutls_certificate_server_retrieve_function);

   procedure gnutls_certificate_server_set_request
     (p1 : gnutls_session_t; p2 : gnutls_certificate_request_t);

   function gnutls_pkcs3_extract_dh_params
     (params     : a_gnutls_datum_t;
      format     : gnutls_x509_crt_fmt_t;
      prime      : a_gnutls_datum_t;
      generator  : a_gnutls_datum_t;
      prime_bits : C.int) return C.int;

   function gnutls_pkcs3_export_dh_params
     (prime            : a_gnutls_datum_t;
      generator        : a_gnutls_datum_t;
      format           : gnutls_x509_crt_fmt_t;
      params_data      : a_c_signed_char_t;
      params_data_size : C.int) return C.int;

   function gnutls_certificate_get_peers
     (p1        : gnutls_session_t;
      list_size : access C.unsigned) return a_gnutls_datum_t;

   function gnutls_certificate_get_ours
     (session : gnutls_session_t) return a_gnutls_datum_t;

   function gnutls_certificate_activation_time_peers
     (session : gnutls_session_t) return time_t;

   function gnutls_certificate_expiration_time_peers
     (session : gnutls_session_t) return time_t;

   function gnutls_certificate_client_get_request_status
     (p1 : gnutls_session_t) return C.int;

   function gnutls_certificate_verify_peers2
     (p1 : gnutls_session_t; status : access C.unsigned) return C.int;

   function gnutls_pem_base64_encode
     (header      : CS.chars_ptr;
      data        : a_gnutls_datum_t;
      result      : CS.chars_ptr;
      result_size : a_size_t) return C.int;

   function gnutls_pem_base64_decode
     (header      : CS.chars_ptr;
      b64_data    : a_gnutls_datum_t;
      result      : a_c_signed_char_t;
      result_size : a_size_t) return C.int;

   function gnutls_pem_base64_encode_alloc
     (header : CS.chars_ptr;
      data   : a_gnutls_datum_t;
      result : a_gnutls_datum_t) return C.int;

   function gnutls_pem_base64_decode_alloc
     (header   : CS.chars_ptr;
      b64_data : a_gnutls_datum_t;
      result   : a_gnutls_datum_t) return C.int;

   procedure gnutls_certificate_set_params_function
     (res  : gnutls_certificate_credentials_t;
      func : gnutls_params_function);

   procedure gnutls_anon_set_params_function
     (res  : gnutls_certificate_credentials_t;
      func : gnutls_params_function);

   --------------------------
   -- GCrypt thread safety --
   --------------------------

   GCRY_THREAD_OPTION_DEFAULT : constant := 0;
   GCRY_THREAD_OPTION_USER    : constant := 1;
   GCRY_THREAD_OPTION_PTH     : constant := 2;
   GCRY_THREAD_OPTION_PTHREAD : constant := 3;
   --  enum gcry_thread_option

   GCRYCTL_SET_THREAD_CBS : constant := 47;
   --  enum gcry_ctl_cmds
   --  Codes used with the gcry_control function.

   type gcry_thread_cbs is record
      Option        : C.int          := GCRY_THREAD_OPTION_DEFAULT;
      Init          : System.Address := System.Null_Address;
      Mutex_Init    : System.Address := System.Null_Address;
      Mutex_Destroy : System.Address := System.Null_Address;
      Mutex_Lock    : System.Address := System.Null_Address;
      Mutex_Unlock  : System.Address := System.Null_Address;
      Read          : System.Address := System.Null_Address;
      Write         : System.Address := System.Null_Address;
      Select_Socket : System.Address := System.Null_Address;
      WaitPID       : System.Address := System.Null_Address;
      Accept_Socket : System.Address := System.Null_Address;
      Connect       : System.Address := System.Null_Address;
      SendMsg       : System.Address := System.Null_Address;
      RecvMsg       : System.Address := System.Null_Address;
   end record;
   pragma Convention (C, gcry_thread_cbs);

   subtype gpg_error_t is C.unsigned;
   subtype gcry_error_t is gpg_error_t;

   function aws_gcry_set_thread_cbs
     (Thread_CBS : gcry_thread_cbs) return gcry_error_t;
   pragma Import (C, aws_gcry_set_thread_cbs, "__aws_gcry_set_thread_cbs");
   --  Calls gcry_control (GCRYCTL_SET_THREAD_CBS, cbs) over C module gcry.c
   --  because varargs does not supported in Ada directly.

   function gcry_strerror (Err : gcry_error_t) return CS.chars_ptr;
   pragma Import (C, gcry_strerror, "gcry_strerror");

   function gcry_strsource (Err : gcry_error_t) return CS.chars_ptr;
   pragma Import (C, gcry_strsource, "gcry_strsource");

   --------------------------------------------------------------------
   -- Tricks to support AWS.Net.SSL specification compatibility with --
   -- OpenSSL thin binding.                                          --
   --------------------------------------------------------------------

   function SSLeay (Dummy : C.int := 0) return C.int;
   pragma Import (C, SSLeay, "gnutls_check_version");

   subtype SSL_Handle is gnutls_session_t;

   Null_Handle : constant SSL_Handle := null;

   type Boolean_Access is access Boolean;

   type BIO_Access is record
      Handshaken : Boolean_Access;
   end record;
   --  Need to be renamed into something like Internal_Type together with
   --  OpenSSL implementation after merge into trunk.

private

   pragma Import
     (C, gnutls_pk_algorithm_get_name, "gnutls_pk_algorithm_get_name");

   pragma Import
     (C, gnutls_sign_algorithm_get_name, "gnutls_sign_algorithm_get_name");

   pragma Import (C, gnutls_init, "gnutls_init");

   pragma Import (C, gnutls_deinit, "gnutls_deinit");

   pragma Import (C, gnutls_bye, "gnutls_bye");

   pragma Import (C, gnutls_handshake, "gnutls_handshake");

   pragma Import (C, gnutls_rehandshake, "gnutls_rehandshake");

   pragma Import (C, gnutls_alert_get, "gnutls_alert_get");

   pragma Import (C, gnutls_alert_send, "gnutls_alert_send");

   pragma Import
     (C, gnutls_alert_send_appropriate, "gnutls_alert_send_appropriate");

   pragma Import (C, gnutls_alert_get_name, "gnutls_alert_get_name");

   pragma Import (C, gnutls_cipher_get, "gnutls_cipher_get");

   pragma Import (C, gnutls_kx_get, "gnutls_kx_get");

   pragma Import (C, gnutls_mac_get, "gnutls_mac_get");

   pragma Import (C, gnutls_compression_get, "gnutls_compression_get");

   pragma Import
     (C, gnutls_certificate_type_get, "gnutls_certificate_type_get");

   pragma Import (C, gnutls_cipher_get_key_size, "gnutls_cipher_get_key_size");

   pragma Import (C, gnutls_cipher_get_name, "gnutls_cipher_get_name");

   pragma Import (C, gnutls_mac_get_name, "gnutls_mac_get_name");

   pragma Import
     (C, gnutls_compression_get_name, "gnutls_compression_get_name");

   pragma Import (C, gnutls_kx_get_name, "gnutls_kx_get_name");

   pragma Import
     (C, gnutls_certificate_type_get_name, "gnutls_certificate_type_get_name");

   pragma Import (C, gnutls_error_is_fatal, "gnutls_error_is_fatal");

   pragma Import (C, gnutls_error_to_alert, "gnutls_error_to_alert");

   pragma Import (C, gnutls_perror, "gnutls_perror");

   pragma Import (C, gnutls_strerror, "gnutls_strerror");

   pragma Import
     (C,
      gnutls_handshake_set_private_extensions,
      "gnutls_handshake_set_private_extensions");

   pragma Import
     (C, gnutls_handshake_get_last_out, "gnutls_handshake_get_last_out");

   pragma Import
     (C, gnutls_handshake_get_last_in, "gnutls_handshake_get_last_in");

   pragma Import (C, gnutls_record_send, "gnutls_record_send");

   pragma Import (C, gnutls_record_recv, "gnutls_record_recv");

   pragma Import
     (C, gnutls_record_get_direction, "gnutls_record_get_direction");

   pragma Import
     (C, gnutls_record_get_max_size, "gnutls_record_get_max_size");

   pragma Import
     (C, gnutls_record_set_max_size, "gnutls_record_set_max_size");

   pragma Import
     (C, gnutls_record_check_pending, "gnutls_record_check_pending");

   pragma Import (C, gnutls_server_name_set, "gnutls_server_name_set");

   pragma Import (C, gnutls_server_name_get, "gnutls_server_name_get");

   pragma Import
     (C, gnutls_cipher_set_priority, "gnutls_cipher_set_priority");

   pragma Import (C, gnutls_mac_set_priority, "gnutls_mac_set_priority");

   pragma Import
     (C, gnutls_compression_set_priority, "gnutls_compression_set_priority");

   pragma Import (C, gnutls_kx_set_priority, "gnutls_kx_set_priority");

   pragma Import
     (C, gnutls_protocol_set_priority, "gnutls_protocol_set_priority");

   pragma Import
     (C,
      gnutls_certificate_type_set_priority,
      "gnutls_certificate_type_set_priority");

   pragma Import
     (C, gnutls_set_default_priority, "gnutls_set_default_priority");

   pragma Import
     (C,
      gnutls_set_default_export_priority,
      "gnutls_set_default_export_priority");

   pragma Import
     (C, gnutls_cipher_suite_get_name, "gnutls_cipher_suite_get_name");

   pragma Import
     (C,
      gnutls_protocol_get_version,
      "gnutls_protocol_get_version");

   pragma Import (C, gnutls_protocol_get_name, "gnutls_protocol_get_name");

   pragma Import (C, gnutls_session_set_data, "gnutls_session_set_data");

   pragma Import (C, gnutls_session_get_data, "gnutls_session_get_data");

   pragma Import (C, gnutls_session_get_id, "gnutls_session_get_id");

   pragma Import (C, gnutls_session_is_resumed, "gnutls_session_is_resumed");

   pragma Import
     (C, gnutls_db_set_cache_expiration, "gnutls_db_set_cache_expiration");

   pragma Import (C, gnutls_db_remove_session, "gnutls_db_remove_session");

   pragma Import
     (C, gnutls_db_set_retrieve_function, "gnutls_db_set_retrieve_function");

   pragma Import
     (C, gnutls_db_set_remove_function, "gnutls_db_set_remove_function");

   pragma Import
     (C, gnutls_db_set_store_function, "gnutls_db_set_store_function");

   pragma Import (C, gnutls_db_set_ptr, "gnutls_db_set_ptr");

   pragma Import (C, gnutls_db_get_ptr, "gnutls_db_get_ptr");

   pragma Import (C, gnutls_db_check_entry, "gnutls_db_check_entry");

   pragma Import
     (C,
      gnutls_handshake_set_max_packet_length,
      "gnutls_handshake_set_max_packet_length");

   pragma Import (C, gnutls_check_version, "gnutls_check_version");

   pragma Import (C, gnutls_credentials_clear, "gnutls_credentials_clear");

   pragma Import (C, gnutls_credentials_set, "gnutls_credentials_set");

   pragma Import
     (C,
      gnutls_anon_free_server_credentials,
      "gnutls_anon_free_server_credentials");

   pragma Import
     (C,
      gnutls_anon_allocate_server_credentials,
      "gnutls_anon_allocate_server_credentials");

   pragma Import
     (C, gnutls_anon_set_server_dh_params, "gnutls_anon_set_server_dh_params");

   pragma Import
     (C,
      gnutls_anon_free_client_credentials,
      "gnutls_anon_free_client_credentials");

   pragma Import
     (C,
      gnutls_anon_allocate_client_credentials,
      "gnutls_anon_allocate_client_credentials");

   pragma Import
     (C,
      gnutls_certificate_free_credentials,
      "gnutls_certificate_free_credentials");

   pragma Import
     (C,
      gnutls_certificate_allocate_credentials,
      "gnutls_certificate_allocate_credentials");

   pragma Import
     (C, gnutls_certificate_free_keys, "gnutls_certificate_free_keys");

   pragma Import
     (C, gnutls_certificate_free_cas, "gnutls_certificate_free_cas");

   pragma Import
     (C, gnutls_certificate_free_ca_names, "gnutls_certificate_free_ca_names");

   pragma Import
     (C, gnutls_certificate_free_crls, "gnutls_certificate_free_crls");

   pragma Import
     (C, gnutls_certificate_set_dh_params, "gnutls_certificate_set_dh_params");

   pragma Import
     (C,
      gnutls_certificate_set_rsa_export_params,
      "gnutls_certificate_set_rsa_export_params");

   pragma Import
     (C,
      gnutls_certificate_set_verify_flags,
      "gnutls_certificate_set_verify_flags");

   pragma Import
     (C,
      gnutls_certificate_set_verify_limits,
      "gnutls_certificate_set_verify_limits");

   pragma Import
     (C,
      gnutls_certificate_set_verify_function,
      "gnutls_certificate_set_verify_function");

   pragma Import
     (C,
      gnutls_certificate_set_x509_trust_file,
      "gnutls_certificate_set_x509_trust_file");

   pragma Import
     (C,
      gnutls_certificate_set_x509_trust_mem,
      "gnutls_certificate_set_x509_trust_mem");

   pragma Import
     (C,
      gnutls_certificate_set_x509_crl_file,
      "gnutls_certificate_set_x509_crl_file");

   pragma Import
     (C,
      gnutls_certificate_set_x509_crl_mem,
      "gnutls_certificate_set_x509_crl_mem");

   pragma Import
     (C,
      gnutls_certificate_set_x509_key_file,
      "gnutls_certificate_set_x509_key_file");

   pragma Import
     (C,
      gnutls_certificate_set_x509_key_mem,
      "gnutls_certificate_set_x509_key_mem");

   pragma Import
     (C, gnutls_certificate_set_x509_key, "gnutls_certificate_set_x509_key");

   pragma Import
     (C,
      gnutls_certificate_set_x509_trust,
      "gnutls_certificate_set_x509_trust");

   pragma Import
     (C, gnutls_certificate_set_x509_crl, "gnutls_certificate_set_x509_crl");

   pragma Import (C, gnutls_x509_crt_init, "gnutls_x509_crt_init");
   pragma Import (C, gnutls_x509_crt_deinit, "gnutls_x509_crt_deinit");
   pragma Import (C, gnutls_x509_crt_import, "gnutls_x509_crt_import");
   pragma Import (C, gnutls_x509_crt_get_dn, "gnutls_x509_crt_get_dn");
   pragma Import
     (C, gnutls_x509_crt_get_issuer_dn, "gnutls_x509_crt_get_issuer_dn");

   pragma Import (C, gnutls_global_init, "gnutls_global_init");

   pragma Import (C, gnutls_global_deinit, "gnutls_global_deinit");

   pragma Import
     (C, gnutls_global_set_mem_functions, "gnutls_global_set_mem_functions");

   pragma Import
     (C, gnutls_global_set_log_function, "gnutls_global_set_log_function");

   pragma Import
     (C, gnutls_global_set_log_level, "gnutls_global_set_log_level");

   pragma Import (C, gnutls_dh_params_init, "gnutls_dh_params_init");

   pragma Import (C, gnutls_dh_params_deinit, "gnutls_dh_params_deinit");

   pragma Import
     (C, gnutls_dh_params_import_raw, "gnutls_dh_params_import_raw");

   pragma Import
     (C, gnutls_dh_params_import_pkcs3, "gnutls_dh_params_import_pkcs3");

   pragma Import
     (C, gnutls_dh_params_generate2, "gnutls_dh_params_generate2");

   pragma Import
     (C, gnutls_dh_params_export_pkcs3, "gnutls_dh_params_export_pkcs3");

   pragma Import
     (C, gnutls_dh_params_export_raw, "gnutls_dh_params_export_raw");

   pragma Import (C, gnutls_dh_params_cpy, "gnutls_dh_params_cpy");

   pragma Import (C, gnutls_rsa_params_init, "gnutls_rsa_params_init");

   pragma Import (C, gnutls_rsa_params_deinit, "gnutls_rsa_params_deinit");

   pragma Import (C, gnutls_rsa_params_cpy, "gnutls_rsa_params_cpy");

   pragma Import
     (C, gnutls_rsa_params_import_raw, "gnutls_rsa_params_import_raw");

   pragma Import
     (C, gnutls_rsa_params_generate2, "gnutls_rsa_params_generate2");

   pragma Import
     (C, gnutls_rsa_params_export_raw, "gnutls_rsa_params_export_raw");

   pragma Import
     (C, gnutls_rsa_params_export_pkcs1, "gnutls_rsa_params_export_pkcs1");

   pragma Import
     (C, gnutls_rsa_params_import_pkcs1, "gnutls_rsa_params_import_pkcs1");

   pragma Import (C, gnutls_transport_get_ptr, "gnutls_transport_get_ptr");

   pragma Import (C, gnutls_transport_get_ptr2, "gnutls_transport_get_ptr2");

   pragma Import
     (C, gnutls_transport_set_lowat, "gnutls_transport_set_lowat");

   pragma Import (C, gnutls_transport_set_ptr, "gnutls_transport_set_ptr");

   pragma Import
     (C,
      gnutls_transport_set_push_function,
      "gnutls_transport_set_push_function");

   pragma Import
     (C,
      gnutls_transport_set_pull_function,
      "gnutls_transport_set_pull_function");

   pragma Import (C, gnutls_session_set_ptr, "gnutls_session_set_ptr");

   pragma Import (C, gnutls_session_get_ptr, "gnutls_session_get_ptr");

   pragma Import (C, gnutls_openpgp_send_key, "gnutls_openpgp_send_key");

   pragma Import (C, gnutls_fingerprint, "gnutls_fingerprint");

   pragma Import
     (C,
      gnutls_srp_free_client_credentials,
      "gnutls_srp_free_client_credentials");

   pragma Import
     (C,
      gnutls_srp_allocate_client_credentials,
      "gnutls_srp_allocate_client_credentials");

   pragma Import
     (C,
      gnutls_srp_set_client_credentials,
      "gnutls_srp_set_client_credentials");

   pragma Import
     (C,
      gnutls_srp_free_server_credentials,
      "gnutls_srp_free_server_credentials");

   pragma Import
     (C,
      gnutls_srp_allocate_server_credentials,
      "gnutls_srp_allocate_server_credentials");

   pragma Import
     (C,
      gnutls_srp_set_server_credentials_file,
      "gnutls_srp_set_server_credentials_file");

   pragma Import
     (C, gnutls_srp_server_get_username, "gnutls_srp_server_get_username");

   pragma Import (C, gnutls_srp_verifier, "gnutls_srp_verifier");

   pragma Import
     (C,
      gnutls_srp_set_server_credentials_function,
      "gnutls_srp_set_server_credentials_function");

   pragma Import
     (C,
      gnutls_srp_set_client_credentials_function,
      "gnutls_srp_set_client_credentials_function");

   pragma Import (C, gnutls_auth_get_type, "gnutls_auth_get_type");

   pragma Import
     (C, gnutls_auth_server_get_type, "gnutls_auth_server_get_type");

   pragma Import
     (C, gnutls_auth_client_get_type, "gnutls_auth_client_get_type");

   pragma Import (C, gnutls_dh_set_prime_bits, "gnutls_dh_set_prime_bits");

   pragma Import (C, gnutls_dh_get_secret_bits, "gnutls_dh_get_secret_bits");

   pragma Import
     (C, gnutls_dh_get_peers_public_bits, "gnutls_dh_get_peers_public_bits");

   pragma Import (C, gnutls_dh_get_prime_bits, "gnutls_dh_get_prime_bits");

   pragma Import (C, gnutls_dh_get_group, "gnutls_dh_get_group");

   pragma Import (C, gnutls_dh_get_pubkey, "gnutls_dh_get_pubkey");

   pragma Import
     (C, gnutls_rsa_export_get_pubkey, "gnutls_rsa_export_get_pubkey");

   pragma Import
     (C,
      gnutls_rsa_export_get_modulus_bits,
      "gnutls_rsa_export_get_modulus_bits");

   pragma Import
     (C,
      gnutls_certificate_set_retrieve_function2,
      "gnutls_certificate_set_retrieve_function2");

   pragma Import
     (C,
      gnutls_certificate_client_set_retrieve_function,
      "gnutls_certificate_client_set_retrieve_function");

   pragma Import
     (C,
      gnutls_certificate_server_set_retrieve_function,
      "gnutls_certificate_server_set_retrieve_function");

   pragma Import
     (C,
      gnutls_certificate_server_set_request,
      "gnutls_certificate_server_set_request");

   pragma Import
     (C, gnutls_pkcs3_extract_dh_params, "gnutls_pkcs3_extract_dh_params");

   pragma Import
     (C, gnutls_pkcs3_export_dh_params, "gnutls_pkcs3_export_dh_params");

   pragma Import
     (C, gnutls_certificate_get_peers, "gnutls_certificate_get_peers");

   pragma Import
     (C, gnutls_certificate_get_ours, "gnutls_certificate_get_ours");

   pragma Import
     (C,
      gnutls_certificate_activation_time_peers,
      "gnutls_certificate_activation_time_peers");

   pragma Import
     (C,
      gnutls_certificate_expiration_time_peers,
      "gnutls_certificate_expiration_time_peers");

   pragma Import
     (C,
      gnutls_certificate_client_get_request_status,
      "gnutls_certificate_client_get_request_status");

   pragma Import
     (C, gnutls_certificate_verify_peers2, "gnutls_certificate_verify_peers2");

   pragma Import (C, gnutls_pem_base64_encode, "gnutls_pem_base64_encode");

   pragma Import (C, gnutls_pem_base64_decode, "gnutls_pem_base64_decode");

   pragma Import
     (C, gnutls_pem_base64_encode_alloc, "gnutls_pem_base64_encode_alloc");

   pragma Import
     (C, gnutls_pem_base64_decode_alloc, "gnutls_pem_base64_decode_alloc");

   pragma Import
     (C,
      gnutls_certificate_set_params_function,
      "gnutls_certificate_set_params_function");

   pragma Import
     (C, gnutls_anon_set_params_function, "gnutls_anon_set_params_function");

   pragma Import (C, gnutls_malloc, "gnutls_malloc");
   pragma Import (C, gnutls_calloc, "gnutls_calloc");
   pragma Import (C, gnutls_free, "gnutls_free");
   pragma Import (C, gnutls_strdup, "gnutls_strdup");
   pragma Import (C, gnutls_srp_2048_group_prime,
                    "gnutls_srp_2048_group_prime");
   pragma Import (C, gnutls_srp_2048_group_generator,
                    "gnutls_srp_2048_group_generator");
   pragma Import (C, gnutls_srp_1536_group_prime,
                    "gnutls_srp_1536_group_prime");
   pragma Import (C, gnutls_srp_1536_group_generator,
                    "gnutls_srp_1536_group_generator");
   pragma Import (C, gnutls_srp_1024_group_prime,
                    "gnutls_srp_1024_group_prime");
   pragma Import (C, gnutls_srp_1024_group_generator,
                    "gnutls_srp_1024_group_generator");

end SSL.Thin;
