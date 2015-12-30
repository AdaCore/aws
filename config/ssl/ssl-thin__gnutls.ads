------------------------------------------------------------------------------
--                            Secure Sockets Layer                          --
--                                                                          --
--                      Copyright (C) 2005-2016, AdaCore                    --
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

--  Thin binding to GNUTLS

with Ada.Streams;
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

   GNUTLS_X509_CRT_LIST_IMPORT_FAIL_IF_EXCEED : constant := 1;
   GNUTLS_X509_CRT_LIST_FAIL_IF_UNSORTED      : constant := 2;

   GNUTLS_PRIVKEY_IMPORT_AUTO_RELEASE : constant := 1;
   GNUTLS_PRIVKEY_IMPORT_COPY         : constant := 2;
   GNUTLS_PRIVKEY_DISABLE_CALLBACKS   : constant := 4;
   GNUTLS_PRIVKEY_SIGN_FLAG_TLS1_RSA  : constant := 16;

   type time_t is new C.long;
   subtype ssize_t is C.int;

   type gnutls_cipher_algorithm_t is new C.int;
   type gnutls_kx_algorithm_t is new C.int;
   type gnutls_credentials_type_t is new C.int;
   type gnutls_mac_algorithm_t is new C.int;
   type gnutls_digest_algorithm_t is new C.int;
   type gnutls_compression_method_t is new C.int;

   GNUTLS_CRD_CERTIFICATE : constant gnutls_credentials_type_t := 1;
   GNUTLS_CRD_ANON        : constant gnutls_credentials_type_t := 2;
   GNUTLS_CRD_SRP         : constant gnutls_credentials_type_t := 3;
   GNUTLS_CRD_PSK         : constant gnutls_credentials_type_t := 4;
   GNUTLS_CRD_IA          : constant gnutls_credentials_type_t := 5;

   GNUTLS_SERVER               : constant C.unsigned := 1;
   GNUTLS_CLIENT               : constant C.unsigned := 2;
   GNUTLS_DATAGRAM             : constant C.unsigned := 4;
   GNUTLS_NONBLOCK             : constant C.unsigned := 8;
   GNUTLS_NO_EXTENSIONS        : constant C.unsigned := 16;
   GNUTLS_NO_REPLAY_PROTECTION : constant C.unsigned := 32;

   GNUTLS_MAC_UNKNOWN : constant gnutls_mac_algorithm_t := 0;
   GNUTLS_MAC_NULL    : constant gnutls_mac_algorithm_t := 1;
   GNUTLS_MAC_MD5     : constant gnutls_mac_algorithm_t := 2;
   GNUTLS_MAC_SHA1    : constant gnutls_mac_algorithm_t := 3;
   GNUTLS_MAC_RMD160  : constant gnutls_mac_algorithm_t := 4;
   GNUTLS_MAC_MD2     : constant gnutls_mac_algorithm_t := 5;
   GNUTLS_MAC_SHA256  : constant gnutls_mac_algorithm_t := 6;
   GNUTLS_MAC_SHA384  : constant gnutls_mac_algorithm_t := 7;
   GNUTLS_MAC_SHA512  : constant gnutls_mac_algorithm_t := 8;
   GNUTLS_MAC_SHA224  : constant gnutls_mac_algorithm_t := 9;
   GNUTLS_MAC_AEAD    : constant gnutls_mac_algorithm_t := 200;
   --  indicates that MAC is on the cipher

   type gnutls_certificate_verify_flags is new C.int;
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
   ALLOW_UNSORTED_CHAIN        : constant certificate_verify_flags := 2 ** 10;
   DO_NOT_ALLOW_UNSORTED_CHAIN : constant certificate_verify_flags := 2 ** 11;

   type gnutls_alert_level_t is (GNUTLS_AL_WARNING, GNUTLS_AL_FATAL);
   for gnutls_alert_level_t use (GNUTLS_AL_WARNING => 1, GNUTLS_AL_FATAL => 2);
   for gnutls_alert_level_t'Size use C.int'Size;

   type gnutls_alert_description_t is new C.int;
   subtype alert_description is gnutls_alert_description_t;

   GNUTLS_A_CLOSE_NOTIFY             : constant alert_description := 0;
   GNUTLS_A_UNEXPECTED_MESSAGE       : constant alert_description := 10;
   GNUTLS_A_BAD_RECORD_MAC           : constant alert_description := 20;
   GNUTLS_A_DECRYPTION_FAILED        : constant alert_description := 21;
   GNUTLS_A_RECORD_OVERFLOW          : constant alert_description := 22;
   GNUTLS_A_DECOMPRESSION_FAILURE    : constant alert_description := 30;
   GNUTLS_A_HANDSHAKE_FAILURE        : constant alert_description := 40;
   GNUTLS_A_SSL3_NO_CERTIFICATE      : constant alert_description := 41;
   GNUTLS_A_BAD_CERTIFICATE          : constant alert_description := 42;
   GNUTLS_A_UNSUPPORTED_CERTIFICATE  : constant alert_description := 43;
   GNUTLS_A_CERTIFICATE_REVOKED      : constant alert_description := 44;
   GNUTLS_A_CERTIFICATE_EXPIRED      : constant alert_description := 45;
   GNUTLS_A_CERTIFICATE_UNKNOWN      : constant alert_description := 46;
   GNUTLS_A_ILLEGAL_PARAMETER        : constant alert_description := 47;
   GNUTLS_A_UNKNOWN_CA               : constant alert_description := 48;
   GNUTLS_A_ACCESS_DENIED            : constant alert_description := 49;
   GNUTLS_A_DECODE_ERROR             : constant alert_description := 50;
   GNUTLS_A_DECRYPT_ERROR            : constant alert_description := 51;
   GNUTLS_A_EXPORT_RESTRICTION       : constant alert_description := 60;
   GNUTLS_A_PROTOCOL_VERSION         : constant alert_description := 70;
   GNUTLS_A_INSUFFICIENT_SECURITY    : constant alert_description := 71;
   GNUTLS_A_INTERNAL_ERROR           : constant alert_description := 80;
   GNUTLS_A_USER_CANCELED            : constant alert_description := 90;
   GNUTLS_A_NO_RENEGOTIATION         : constant alert_description := 100;
   GNUTLS_A_UNSUPPORTED_EXTENSION    : constant alert_description := 110;
   GNUTLS_A_CERTIFICATE_UNOBTAINABLE : constant alert_description := 111;
   GNUTLS_A_UNRECOGNIZED_NAME        : constant alert_description := 112;
   GNUTLS_A_UNKNOWN_SRP_USERNAME     : constant alert_description := 120;
   GNUTLS_A_MISSING_SRP_USERNAME     : constant alert_description := 121;

   type gnutls_handshake_description_t is new C.int;
   subtype handshake_description is gnutls_handshake_description_t;

   GNUTLS_HANDSHAKE_HELLO_REQUEST       : constant handshake_description := 0;
   GNUTLS_HANDSHAKE_CLIENT_HELLO        : constant handshake_description := 1;
   GNUTLS_HANDSHAKE_SERVER_HELLO        : constant handshake_description := 2;
   GNUTLS_HANDSHAKE_CERTIFICATE_PKT     : constant handshake_description := 11;
   GNUTLS_HANDSHAKE_SERVER_KEY_EXCHANGE : constant handshake_description := 12;
   GNUTLS_HANDSHAKE_CERTIFICATE_REQUEST : constant handshake_description := 13;
   GNUTLS_HANDSHAKE_SERVER_HELLO_DONE   : constant handshake_description := 14;
   GNUTLS_HANDSHAKE_CERTIFICATE_VERIFY  : constant handshake_description := 15;
   GNUTLS_HANDSHAKE_CLIENT_KEY_EXCHANGE : constant handshake_description := 16;
   GNUTLS_HANDSHAKE_FINISHED            : constant handshake_description := 20;

   type gnutls_certificate_status_t is
     (GNUTLS_CERT_INVALID,
      GNUTLS_CERT_REVOKED,
      GNUTLS_CERT_SIGNER_NOT_FOUND,
      GNUTLS_CERT_SIGNER_NOT_CA,
      GNUTLS_CERT_INSECURE_ALGORITHM,
      GNUTLS_CERT_NOT_ACTIVATED,
      GNUTLS_CERT_EXPIRED,
      GNUTLS_CERT_SIGNATURE_FAILURE,
      GNUTLS_CERT_REVOCATION_DATA_SUPERSEDED,
      GNUTLS_CERT_UNEXPECTED_OWNER,
      GNUTLS_CERT_REVOCATION_DATA_ISSUED_IN_FUTURE,
      GNUTLS_CERT_SIGNER_CONSTRAINTS_FAILURE,
      GNUTLS_CERT_MISMATCH);
   for gnutls_certificate_status_t use
     (GNUTLS_CERT_INVALID                          => 2,
      GNUTLS_CERT_REVOKED                          => 32,
      GNUTLS_CERT_SIGNER_NOT_FOUND                 => 64,
      GNUTLS_CERT_SIGNER_NOT_CA                    => 128,
      GNUTLS_CERT_INSECURE_ALGORITHM               => 256,
      GNUTLS_CERT_NOT_ACTIVATED                    => 512,
      GNUTLS_CERT_EXPIRED                          => 1024,
      GNUTLS_CERT_SIGNATURE_FAILURE                => 2048,
      GNUTLS_CERT_REVOCATION_DATA_SUPERSEDED       => 4096,
      GNUTLS_CERT_UNEXPECTED_OWNER                 => 2**14,
      GNUTLS_CERT_REVOCATION_DATA_ISSUED_IN_FUTURE => 2**15,
      GNUTLS_CERT_SIGNER_CONSTRAINTS_FAILURE       => 2**16,
      GNUTLS_CERT_MISMATCH                         => 2**17);
   for gnutls_certificate_status_t'Size use C.int'Size;

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

   type gnutls_protocol_t is new C.int;

   GNUTLS_SSL3            : constant gnutls_protocol_t := 1;
   GNUTLS_TLS1_0          : constant gnutls_protocol_t := 2;
   GNUTLS_TLS1_1          : constant gnutls_protocol_t := 3;
   GNUTLS_TLS1_2          : constant gnutls_protocol_t := 4;
   GNUTLS_DTLS1_0         : constant gnutls_protocol_t := 5;
   GNUTLS_DTLS0_9         : constant gnutls_protocol_t := 6;
   GNUTLS_VERSION_UNKNOWN : constant gnutls_protocol_t := 16#FF#;
   GNUTLS_TLS1            : constant gnutls_protocol_t := GNUTLS_TLS1_0;
   GNUTLS_VERSION_MAX     : constant gnutls_protocol_t := GNUTLS_DTLS0_9;

   type gnutls_certificate_type_t is new C.int;

   GNUTLS_CRT_X509    : constant gnutls_certificate_type_t := 1;
   GNUTLS_CRT_OPENPGP : constant gnutls_certificate_type_t := 2;

   type gnutls_x509_crt_fmt_t is (GNUTLS_X509_FMT_DER, GNUTLS_X509_FMT_PEM);
   for gnutls_x509_crt_fmt_t'Size use C.int'Size;

   type gnutls_pk_algorithm_t is new C.int;

   GNUTLS_PK_UNKNOWN : constant gnutls_pk_algorithm_t := 0;
   GNUTLS_PK_RSA     : constant gnutls_pk_algorithm_t := 1;
   GNUTLS_PK_DSA     : constant gnutls_pk_algorithm_t := 2;
   GNUTLS_PK_DH      : constant gnutls_pk_algorithm_t := 3;
   GNUTLS_PK_EC      : constant gnutls_pk_algorithm_t := 4;

   type gnutls_sign_algorithm_t is new C.int;

   GNUTLS_SIGN_RSA_SHA : constant gnutls_sign_algorithm_t := 1;
   GNUTLS_SIGN_DSA_SHA : constant gnutls_sign_algorithm_t := 2;
   GNUTLS_SIGN_RSA_MD5 : constant gnutls_sign_algorithm_t := 3;
   GNUTLS_SIGN_RSA_MD2 : constant gnutls_sign_algorithm_t := 4;
   GNUTLS_SIGN_UNKNOWN : constant gnutls_sign_algorithm_t := 255;

   type gnutls_sec_param_t is new C.int;
   GNUTLS_SEC_PARAM_UNKNOWN   : constant gnutls_sec_param_t with Import,
      Convention => C, External_Name => "_AWS_GNUTLS_SEC_PARAM_UNKNOWN";
   GNUTLS_SEC_PARAM_INSECURE  : constant gnutls_sec_param_t with Import,
      Convention => C, External_Name => "_AWS_GNUTLS_SEC_PARAM_INSECURE";
   GNUTLS_SEC_PARAM_EXPORT    : constant gnutls_sec_param_t with Import,
      Convention => C, External_Name => "_AWS_GNUTLS_SEC_PARAM_EXPORT";
   GNUTLS_SEC_PARAM_VERY_WEAK : constant gnutls_sec_param_t with Import,
      Convention => C, External_Name => "_AWS_GNUTLS_SEC_PARAM_VERY_WEAK";
   GNUTLS_SEC_PARAM_WEAK      : constant gnutls_sec_param_t with Import,
      Convention => C, External_Name => "_AWS_GNUTLS_SEC_PARAM_WEAK";
   GNUTLS_SEC_PARAM_LOW       : constant gnutls_sec_param_t with Import,
      Convention => C, External_Name => "_AWS_GNUTLS_SEC_PARAM_LOW";
   GNUTLS_SEC_PARAM_LEGACY    : constant gnutls_sec_param_t with Import,
      Convention => C, External_Name => "_AWS_GNUTLS_SEC_PARAM_LEGACY";
   GNUTLS_SEC_PARAM_MEDIUM    : constant gnutls_sec_param_t with Import,
      Convention => C, External_Name => "_AWS_GNUTLS_SEC_PARAM_NORMAL";
   --  MEDIUM parameter imported as NORMAL for compartibility with different
   --  GNUTLS versions. MEDIUM appeared at GNUTLS version 3.3.
   GNUTLS_SEC_PARAM_HIGH      : constant gnutls_sec_param_t with Import,
      Convention => C, External_Name => "_AWS_GNUTLS_SEC_PARAM_HIGH";
   GNUTLS_SEC_PARAM_ULTRA     : constant gnutls_sec_param_t with Import,
      Convention => C, External_Name => "_AWS_GNUTLS_SEC_PARAM_ULTRA";

   GNUTLS_SEC_PARAM_NORMAL : constant gnutls_sec_param_t :=
      GNUTLS_SEC_PARAM_MEDIUM;

   type chars_constant_access is access constant C.char_array
     with Size => Standard'Address_Size, Convention => C;

   GNUTLS_OID_X520_COUNTRY_NAME  : aliased constant C.char_array := "2.5.4.6";
   GNUTLS_OID_X520_COMMON_NAME   : aliased constant C.char_array := "2.5.4.3";
   GNUTLS_OID_X520_LOCALITY_NAME : aliased constant C.char_array := "2.5.4.7";

   type gnutls_params_type_t is new C.int;
   GNUTLS_PARAMS_RSA_EXPORT : constant gnutls_params_type_t := 1;
   GNUTLS_PARAMS_DH         : constant gnutls_params_type_t := 2;
   GNUTLS_PARAMS_ECDH       : constant gnutls_params_type_t := 3;

   type gnutls_server_name_type_t is (GNUTLS_NAME_DNS);
   for gnutls_server_name_type_t use (GNUTLS_NAME_DNS => 1);
   for gnutls_server_name_type_t'Size use C.int'Size;

   subtype a_unsigned_char_t is System.Address;
   type a_size_t is access all C.size_t;
   type gnutls_transport_ptr_t is new C.int;

   type gnutls_session_t;
   type gnutls_datum_t;
   type gnutls_retr_st;

   type gnutls_srp_server_credentials_function is new System.Address;
   type gnutls_srp_client_credentials_function is new System.Address;
   type gnutls_certificate_server_retrieve_function is new System.Address;

   type STRUCT_DSTRUCT;

   type gnutls_session_t is access all STRUCT_DSTRUCT;
   type gnutls_dh_params_t is access all STRUCT_DSTRUCT;
   type gnutls_ecdh_params_t is access all STRUCT_DSTRUCT;
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
   type gnutls_priority_t is access all STRUCT_DSTRUCT;

   subtype gnutls_rsa_params_t is gnutls_x509_privkey_t;
   subtype Private_Key is gnutls_privkey_t;

   type gnutls_retr_st is record
      cert_type  : gnutls_certificate_type_t;
      cert_x509  : a_gnutls_x509_crt_t;
      ncerts     : C.int;              -- one for pgp keys
      key_x509   : gnutls_x509_privkey_t;
      deinit_all : C.unsigned;         -- if non zero all keys will be deinited
   end record with Convention => C;

   type gnutls_datum_t is record
      data : a_unsigned_char_t;
      size : C.unsigned;
   end record with Convention => C_Pass_By_Copy;

   type Session_Record (Id_Size : Ada.Streams.Stream_Element_Count) is
   record
      Id   : Ada.Streams.Stream_Element_Array (1 .. Id_Size);
      Data : aliased gnutls_datum_t;
   end record;
   --  To support AWS.Net.SSL.Session_Type

   type gnutls_pcert_st is record
      pubkey : gnutls_pubkey_t;
      cert   : gnutls_datum_t;
      c_type : gnutls_certificate_type_t;
   end record with Convention => C;

   type a_gnutls_pcert_st is access all gnutls_pcert_st;

   type gnutls_certificate_retrieve_function2 is access function
     (Session         : gnutls_session_t;
      req_ca_rdn      : access constant gnutls_datum_t;
      nreqs           : C.int;
      pk_algos        : access constant gnutls_pk_algorithm_t;
      pk_algos_length : C.int;
      pcert           : access a_gnutls_pcert_st;
      pcert_length    : access C.unsigned;
      privkey         : access gnutls_privkey_t) return C.int
     with Convention => C;
   --  ??? This callback in version 3.0.3 defined different in man and in file
   --  gnutls.h. This is a new definition. Old definition was ended by field
   --  st : access gnutls_pcert_st.

   type gnutls_certificate_client_retrieve_function is access function
     (Session         : gnutls_session_t;
      Req_CA_DN       : access constant gnutls_datum_t;
      nreqs           : C.int;
      pk_algos        : access constant gnutls_pk_algorithm_t;
      pk_algos_length : C.int;
      st              : access gnutls_retr_st) return C.int
     with Convention => C;

   type gnutls_certificate_verify_function is access function
     (Session : gnutls_session_t) return C.int
     with Convention => C;

   type gnutls_db_store_func is access function
     (p1   : System.Address;
      key  : gnutls_datum_t;
      data : gnutls_datum_t) return C.int
     with Convention => C;

   type gnutls_db_remove_func is access function
     (p1 : System.Address; key : gnutls_datum_t) return C.int
     with Convention => C;

   type gnutls_db_retr_func is access function
     (p1  : System.Address; key : gnutls_datum_t) return gnutls_datum_t
     with Convention => C;

   type STRUCT_DSTRUCT is null record with Convention => C;

   type gnutls_alloc_function is access function
     (p1 : C.size_t) return System.Address
     with Convention => C;

   type gnutls_calloc_function is access function
     (p1 : C.size_t; p2 : C.size_t) return System.Address
     with Convention => C;

   type gnutls_is_secure_function is access function
     (p1 : System.Address) return C.int
     with Convention => C;

   type gnutls_free_function is access procedure (p1 : System.Address)
     with Convention => C;

   type gnutls_realloc_function is access function
     (p1 : System.Address; p2 : C.size_t) return System.Address
     with Convention => C;

   type gnutls_log_func is access procedure
     (level : C.int; text : CS.chars_ptr)
     with Convention => C;

   type gnutls_audit_log_func is access procedure
     (sessn : gnutls_session_t; level : C.int; text : CS.chars_ptr)
     with Convention => C;

   type gnutls_params_st_union
     (kind : gnutls_params_type_t := GNUTLS_PARAMS_DH) is
   record
      case kind is
         when GNUTLS_PARAMS_DH         => dh         : gnutls_dh_params_t;
         when GNUTLS_PARAMS_ECDH       => ecdh       : gnutls_ecdh_params_t;
         when GNUTLS_PARAMS_RSA_EXPORT => rsa_export : gnutls_rsa_params_t;
         when others                   => unknown    : C.int;
      end case;
   end record with Unchecked_Union, Convention => C;

   type gnutls_params_st is record
      kind   : gnutls_params_type_t;
      params : gnutls_params_st_union;
      deinit : C.int;
   end record with Convention => C;

   type gnutls_params_function is access function
     (sessn  : gnutls_session_t;
      kind   : gnutls_params_type_t;
      params : access gnutls_params_st) return C.int with Convention => C;

   gnutls_malloc                   : constant gnutls_alloc_function
     with Import, Convention => C;
   gnutls_calloc                   : constant gnutls_calloc_function
     with Import, Convention => C;
   gnutls_free                     : constant gnutls_free_function
     with Import, Convention => C;
   gnutls_strdup                   : constant System.Address
     with Import, Convention => C;

   gnutls_srp_2048_group_prime     : constant gnutls_datum_t
     with Import, Convention => C;
   gnutls_srp_2048_group_generator : constant gnutls_datum_t
     with Import, Convention => C;

   gnutls_srp_1536_group_prime     : constant gnutls_datum_t
     with Import, Convention => C;
   gnutls_srp_1536_group_generator : constant gnutls_datum_t
     with Import, Convention => C;

   gnutls_srp_1024_group_prime     : constant gnutls_datum_t
     with Import, Convention => C;
   gnutls_srp_1024_group_generator : constant gnutls_datum_t
     with Import, Convention => C;

   function gnutls_pk_algorithm_get_name
     (algorithm : gnutls_pk_algorithm_t) return CS.chars_ptr
     with Import, Convention => C;

   function gnutls_sign_algorithm_get_name
     (algorithm : gnutls_sign_algorithm_t) return CS.chars_ptr
     with Import, Convention => C;

   function gnutls_init
     (session : access gnutls_session_t; flags : C.unsigned) return C.int
     with Import, Convention => C;

   procedure gnutls_deinit (session : gnutls_session_t)
     with Import, Convention => C;

   function gnutls_bye
     (session : gnutls_session_t;
      how     : gnutls_close_request_t) return C.int
     with Import, Convention => C;

   function gnutls_handshake (session : gnutls_session_t) return C.int
     with Import, Convention => C;

   function gnutls_rehandshake (session : gnutls_session_t) return C.int
     with Import, Convention => C;

   function gnutls_alert_get
     (session : gnutls_session_t) return gnutls_alert_description_t
     with Import, Convention => C;

   function gnutls_alert_send
     (p1 : gnutls_session_t;
      p2 : gnutls_alert_level_t;
      p3 : gnutls_alert_description_t) return C.int
     with Import, Convention => C;

   function gnutls_alert_send_appropriate
     (session : gnutls_session_t; err : C.int) return C.int
     with Import, Convention => C;

   function gnutls_alert_get_name
     (alert : gnutls_alert_description_t) return  CS.chars_ptr
     with Import, Convention => C;

   function gnutls_cipher_get
     (session : gnutls_session_t) return gnutls_cipher_algorithm_t
     with Import, Convention => C;

   function gnutls_kx_get
     (session : gnutls_session_t) return gnutls_kx_algorithm_t
     with Import, Convention => C;

   function gnutls_mac_get
     (session : gnutls_session_t) return gnutls_mac_algorithm_t
     with Import, Convention => C;

   function gnutls_compression_get
     (session : gnutls_session_t) return gnutls_compression_method_t
     with Import, Convention => C;

   function gnutls_certificate_type_get
     (session : gnutls_session_t) return gnutls_certificate_type_t
     with Import, Convention => C;

   function gnutls_cipher_get_key_size
     (algorithm : gnutls_cipher_algorithm_t) return C.size_t
     with Import, Convention => C;

   function gnutls_cipher_get_name
     (p1 : gnutls_cipher_algorithm_t) return CS.chars_ptr
     with Import, Convention => C;

   function gnutls_mac_get_name
     (p1 : gnutls_mac_algorithm_t) return CS.chars_ptr
     with Import, Convention => C;

   function gnutls_compression_get_name
     (p1 : gnutls_compression_method_t) return CS.chars_ptr
     with Import, Convention => C;

   function gnutls_kx_get_name
     (algorithm : gnutls_kx_algorithm_t) return CS.chars_ptr
     with Import, Convention => C;

   function gnutls_certificate_type_get_name
     (c_type : gnutls_certificate_type_t) return CS.chars_ptr
     with Import, Convention => C;

   function gnutls_error_is_fatal (error : C.int) return C.int
     with Import, Convention => C;

   function gnutls_error_to_alert
     (err : C.int; level : access C.int) return C.int
     with Import, Convention => C;

   procedure gnutls_perror (error : C.int)
     with Import, Convention => C;

   function gnutls_strerror (error : C.int) return CS.chars_ptr
     with Import, Convention => C;

   procedure gnutls_handshake_set_private_extensions
     (session : gnutls_session_t; allow : C.int)
     with Import, Convention => C;

   function gnutls_handshake_get_last_out
     (session : gnutls_session_t) return gnutls_handshake_description_t
     with Import, Convention => C;

   function gnutls_handshake_get_last_in
     (session : gnutls_session_t) return gnutls_handshake_description_t
     with Import, Convention => C;

   function gnutls_record_send
     (session    : gnutls_session_t;
      data       : System.Address;
      sizeofdata : C.size_t) return ssize_t
     with Import, Convention => C;

   function gnutls_record_recv
     (session    : gnutls_session_t;
      data       : System.Address;
      sizeofdata : C.size_t) return ssize_t
     with Import, Convention => C;

   function gnutls_record_get_direction
     (session : gnutls_session_t) return C.int
     with Import, Convention => C;

   function gnutls_record_get_max_size
     (session : gnutls_session_t) return C.size_t
     with Import, Convention => C;

   function gnutls_record_set_max_size
     (session : gnutls_session_t; size : C.size_t) return C.size_t
     with Import, Convention => C;

   function gnutls_record_check_pending
     (session : gnutls_session_t) return C.size_t
     with Import, Convention => C;

   function gnutls_server_name_set
     (session     : gnutls_session_t;
      c_type      : gnutls_server_name_type_t;
      name        : System.Address;
      name_length : C.size_t) return C.int
     with Import, Convention => C;

   function gnutls_server_name_get
     (session     : gnutls_session_t;
      data        : System.Address;
      data_length : a_size_t;
      c_type      : access gnutls_server_name_type_t;
      indx        : C.unsigned) return C.int
     with Import, Convention => C;

   function gnutls_priority_init
     (priority_cache : access gnutls_priority_t;
      priorities     : CS.chars_ptr;
      err_pos        : access CS.chars_ptr) return C.int
     with Import, Convention => C;

   procedure gnutls_priority_deinit (priority_cache : gnutls_priority_t)
     with Import, Convention => C;

   function gnutls_priority_get_cipher_suite_index
     (pcache : gnutls_priority_t;
      idx    : C.unsigned;
      sidx   : access C.unsigned) return C.int
     with Import, Convention => C;

   function gnutls_priority_set
     (session : gnutls_session_t; priority : gnutls_priority_t) return C.int
     with Import, Convention => C;

   function gnutls_priority_set_direct
     (session    : gnutls_session_t;
      priorities : CS.chars_ptr;
      err_pos    : access CS.chars_ptr) return C.int
     with Import, Convention => C;

   function gnutls_priority_certificate_type_list
     (pcache : gnutls_priority_t; list : System.Address) return C.int
     with Import, Convention => C;

   function gnutls_priority_sign_list
     (pcache : gnutls_priority_t; list : System.Address) return C.int
     with Import, Convention => C;

   function gnutls_priority_protocol_list
     (pcache : gnutls_priority_t; list : System.Address) return C.int
     with Import, Convention => C;

   function gnutls_priority_compression_list
     (pcache : gnutls_priority_t; list : System.Address) return C.int
     with Import, Convention => C;

   function gnutls_priority_ecc_curve_list
     (pcache : gnutls_priority_t; list : System.Address) return C.int
     with Import, Convention => C;

   function gnutls_cipher_set_priority
     (session : gnutls_session_t; p2 : System.Address) return C.int
     with Import, Convention => C;

   function gnutls_mac_set_priority
     (session : gnutls_session_t; p2 : System.Address) return C.int
     with Import, Convention => C;

   function gnutls_compression_set_priority
     (session : gnutls_session_t; p2 : System.Address) return C.int
     with Import, Convention => C;

   function gnutls_kx_set_priority
     (session : gnutls_session_t; p2 : System.Address) return C.int
     with Import, Convention => C;

   function gnutls_protocol_set_priority
     (session : gnutls_session_t; p2 : System.Address) return C.int
     with Import, Convention => C;

   function gnutls_certificate_type_set_priority
     (session : gnutls_session_t; p2 : System.Address) return C.int
     with Import, Convention => C;

   function gnutls_set_default_priority
     (session : gnutls_session_t) return C.int
     with Import, Convention => C;

   function gnutls_set_default_export_priority
     (session : gnutls_session_t) return C.int
     with Import, Convention => C;

   function gnutls_cipher_suite_info
     (idx         : C.size_t;
      cs_id       : access C.unsigned_char;
      kx          : access gnutls_kx_algorithm_t;
      cipher      : access gnutls_cipher_algorithm_t;
      mac         : access gnutls_mac_algorithm_t;
      min_version : access gnutls_protocol_t) return CS.chars_ptr
     with Import, Convention => C;

   function gnutls_cipher_suite_get_name
     (kx_algorithm     : gnutls_kx_algorithm_t;
      cipher_algorithm : gnutls_cipher_algorithm_t;
      mac_algorithm    : gnutls_mac_algorithm_t) return CS.chars_ptr
     with Import, Convention => C;

   function gnutls_protocol_get_version
     (session : gnutls_session_t) return gnutls_protocol_t
     with Import, Convention => C;

   function gnutls_protocol_get_name
     (version : gnutls_protocol_t) return CS.chars_ptr
     with Import, Convention => C;

   function gnutls_protocol_list return access gnutls_protocol_t
     with Import, Convention => C;

   function gnutls_session_set_data
     (session           : gnutls_session_t;
      session_data      : System.Address;
      session_data_size : C.size_t) return C.int
     with Import, Convention => C;

   function gnutls_session_get_data
     (session           : gnutls_session_t;
      session_data      : System.Address;
      session_data_size : a_size_t) return C.int
     with Import, Convention => C;

   function gnutls_session_get_data2
     (session : gnutls_session_t; data : a_gnutls_datum_t) return C.int
     with Import, Convention => C;

   function gnutls_session_get_id
     (session         : gnutls_session_t;
      session_id      : System.Address;
      session_id_size : a_size_t) return C.int
     with Import, Convention => C;

   function gnutls_session_is_resumed
     (session : gnutls_session_t) return C.int
     with Import, Convention => C;

   procedure gnutls_db_set_cache_expiration
     (session : gnutls_session_t; seconds : C.int)
     with Import, Convention => C;

   function gnutls_db_check_entry_time
     (Item : a_gnutls_datum_t) return C.long
     with Import, Convention => C;

   procedure gnutls_db_remove_session (session : gnutls_session_t)
     with Import, Convention => C;

   procedure gnutls_db_set_retrieve_function
     (session : gnutls_session_t; retr_func : gnutls_db_retr_func)
     with Import, Convention => C;

   procedure gnutls_db_set_remove_function
     (session : gnutls_session_t; rem_func : gnutls_db_remove_func)
     with Import, Convention => C;

   procedure gnutls_db_set_store_function
     (session : gnutls_session_t; store_func : gnutls_db_store_func)
     with Import, Convention => C;

   procedure gnutls_db_set_ptr
     (session : gnutls_session_t; db_ptr : System.Address)
     with Import, Convention => C;

   function gnutls_db_get_ptr (p1 : gnutls_session_t) return System.Address
     with Import, Convention => C;

   function gnutls_db_check_entry
     (session       : gnutls_session_t;
      session_entry : gnutls_datum_t) return C.int
     with Import, Convention => C;

   procedure gnutls_handshake_set_max_packet_length
     (session : gnutls_session_t; max : C.int)
     with Import, Convention => C;

   function gnutls_check_version (p1 : CS.chars_ptr) return CS.chars_ptr
     with Import, Convention => C;

   function gnutls_credentials_clear (session : gnutls_session_t) return C.int
     with Import, Convention => C;

   function gnutls_credentials_set
     (p1     : gnutls_session_t;
      c_type : gnutls_credentials_type_t;
      cred   : System.Address) return C.int
     with Import, Convention => C;

   function gnutls_credentials_set
     (p1     : gnutls_session_t;
      c_type : gnutls_credentials_type_t := GNUTLS_CRD_ANON;
      cred   : gnutls_anon_client_credentials_t) return C.int
     with Import, Convention => C;

   function gnutls_credentials_set
     (p1     : gnutls_session_t;
      c_type : gnutls_credentials_type_t := GNUTLS_CRD_ANON;
      cred   : gnutls_anon_server_credentials_t) return C.int
     with Import, Convention => C;

   function gnutls_credentials_set
     (p1     : gnutls_session_t;
      c_type : gnutls_credentials_type_t := GNUTLS_CRD_CERTIFICATE;
      cred   : gnutls_certificate_credentials_t) return C.int
     with Import, Convention => C;

   procedure gnutls_anon_free_server_credentials
     (sc : gnutls_anon_server_credentials_t)
     with Import, Convention => C;

   function gnutls_anon_allocate_server_credentials
     (sc : access gnutls_anon_server_credentials_t) return C.int
     with Import, Convention => C;

   procedure gnutls_anon_set_server_dh_params
     (res       : gnutls_anon_server_credentials_t;
      dh_params : gnutls_dh_params_t)
     with Import, Convention => C;

   procedure gnutls_anon_free_client_credentials
     (sc : gnutls_anon_client_credentials_t)
     with Import, Convention => C;

   function gnutls_anon_allocate_client_credentials
     (sc : access gnutls_anon_client_credentials_t) return C.int
     with Import, Convention => C;

   procedure gnutls_certificate_free_credentials
     (sc : gnutls_certificate_credentials_t)
     with Import, Convention => C;

   function gnutls_certificate_allocate_credentials
     (sc : access gnutls_certificate_credentials_t) return C.int
     with Import, Convention => C;

   procedure gnutls_certificate_free_keys
     (sc : gnutls_certificate_credentials_t)
     with Import, Convention => C;

   procedure gnutls_certificate_free_cas
     (sc : gnutls_certificate_credentials_t)
     with Import, Convention => C;

   procedure gnutls_certificate_free_ca_names
     (sc : gnutls_certificate_credentials_t)
     with Import, Convention => C;

   procedure gnutls_certificate_free_crls
     (sc : gnutls_certificate_credentials_t)
     with Import, Convention => C;

   procedure gnutls_certificate_set_dh_params
     (res : gnutls_certificate_credentials_t;
      p2  : gnutls_dh_params_t)
     with Import, Convention => C;

   procedure gnutls_certificate_set_verify_flags
     (res   : gnutls_certificate_credentials_t;
      flags : C.unsigned)
     with Import, Convention => C;

   procedure gnutls_certificate_set_verify_limits
     (res       : gnutls_certificate_credentials_t;
      max_bits  : C.unsigned;
      max_depth : C.unsigned)
     with Import, Convention => C;

   procedure gnutls_certificate_set_verify_function
     (cred : gnutls_certificate_credentials_t;
      func : gnutls_certificate_verify_function)
     with Import, Convention => C;

   function gnutls_certificate_set_x509_trust_file
     (res    : gnutls_certificate_credentials_t;
      CAFILE : CS.chars_ptr;
      p3     : gnutls_x509_crt_fmt_t) return C.int
     with Import, Convention => C;

   function gnutls_certificate_set_x509_trust_mem
     (res : gnutls_certificate_credentials_t;
      CA  : a_gnutls_datum_t;
      p3  : gnutls_x509_crt_fmt_t) return C.int
     with Import, Convention => C;

   function gnutls_certificate_set_x509_crl_file
     (res     : gnutls_certificate_credentials_t;
      crlfile : CS.chars_ptr;
      c_type  : gnutls_x509_crt_fmt_t) return C.int
     with Import, Convention => C;

   function gnutls_certificate_set_x509_crl_mem
     (res    : gnutls_certificate_credentials_t;
      CRL    : a_gnutls_datum_t;
      c_type : gnutls_x509_crt_fmt_t) return C.int
     with Import, Convention => C;

   function gnutls_certificate_set_x509_key_file
     (res      : gnutls_certificate_credentials_t;
      certfile : CS.chars_ptr;
      keyfile  : CS.chars_ptr;
      p4       : gnutls_x509_crt_fmt_t) return C.int
     with Import, Convention => C;

   function gnutls_certificate_set_x509_key_mem
     (res  : gnutls_certificate_credentials_t;
      cert : a_gnutls_datum_t;
      key  : a_gnutls_datum_t;
      p4   : gnutls_x509_crt_fmt_t) return C.int
     with Import, Convention => C;

   function gnutls_certificate_set_x509_key_mem2
     (res   : gnutls_certificate_credentials_t;
      cert  : a_gnutls_datum_t;
      key   : a_gnutls_datum_t;
      p4    : gnutls_x509_crt_fmt_t;
      pass  : CS.chars_ptr;
      flags : C.unsigned) return C.int
     with Import, Convention => C;

   function gnutls_certificate_set_x509_key
     (res            : gnutls_certificate_credentials_t;
      cert_list      : a_gnutls_x509_crt_t;
      cert_list_size : C.int;
      key            : gnutls_x509_privkey_t) return C.int
     with Import, Convention => C;

   function gnutls_certificate_set_x509_trust
     (res          : gnutls_certificate_credentials_t;
      ca_list      : a_gnutls_x509_crt_t;
      ca_list_size : C.int) return C.int
     with Import, Convention => C;

   function gnutls_certificate_set_x509_crl
     (res           : gnutls_certificate_credentials_t;
      crl_list      : a_gnutls_x509_crl_t;
      crl_list_size : C.int) return C.int
     with Import, Convention => C;

   procedure gnutls_certificate_send_x509_rdn_sequence
     (session : gnutls_session_t; status : C.int)
     with Import, Convention => C;

   function gnutls_x509_rdn_get
     (idn        : a_gnutls_datum_t;
      buf        : CS.chars_ptr;
      sizeof_buf : access C.size_t) return C.int
     with Import, Convention => C;

   function gnutls_x509_crt_init
     (cert : access gnutls_x509_crt_t) return C.int
     with Import, Convention => C;

   procedure gnutls_x509_crt_deinit (cert : gnutls_x509_crt_t)
     with Import, Convention => C;

   function gnutls_x509_crt_export2
     (cert   : gnutls_x509_crt_t;
      format : gnutls_x509_crt_fmt_t;
      data   : access gnutls_datum_t) return C.int
     with Import, Convention => C;

   function gnutls_x509_privkey_init
     (key : access gnutls_x509_privkey_t) return C.int
     with Import, Convention => C;

   function gnutls_x509_privkey_generate
     (key   : gnutls_x509_privkey_t;
      algo  : gnutls_pk_algorithm_t;
      bits  : C.unsigned;
      flags : C.unsigned) return C.int with Import, Convention => C;

   procedure gnutls_x509_privkey_deinit (key : gnutls_x509_privkey_t)
     with Import, Convention => C;

   function gnutls_x509_privkey_import
     (key    : gnutls_x509_privkey_t;
      data   : a_gnutls_datum_t;
      format : gnutls_x509_crt_fmt_t) return C.int
     with Import, Convention => C;

   function gnutls_x509_crt_list_import
     (certs    : access gnutls_x509_crt_t;
      cert_max : access C.unsigned;
      data     : a_gnutls_datum_t;
      format   : gnutls_x509_crt_fmt_t;
      flags    : C.unsigned) return C.int
     with Import, Convention => C;

   function gnutls_privkey_init
     (key : access gnutls_privkey_t) return C.int
     with Import, Convention => C;

   procedure gnutls_privkey_deinit (key : gnutls_privkey_t)
     with Import, Convention => C;

   function gnutls_privkey_import_x509
     (pkey  : gnutls_privkey_t;
      key   : gnutls_x509_privkey_t;
      flags : C.unsigned) return C.int
     with Import, Convention => C;

   function gnutls_privkey_import_x509_raw
     (pkey     : gnutls_privkey_t;
      data     : a_gnutls_datum_t;
      format   : gnutls_x509_crt_fmt_t;
      password : CS.chars_ptr;
      flags    : C.unsigned) return C.int
     with Import, Convention => C;

   function gnutls_privkey_sign_data
     (signer    : gnutls_privkey_t;
      hash      : gnutls_digest_algorithm_t;
      flags     : C.unsigned;
      data      : a_gnutls_datum_t;
      signature : access gnutls_datum_t) return C.int
     with Import, Convention => C;

   function gnutls_privkey_sign_raw_data
     (key       : gnutls_privkey_t;
      flags     : C.unsigned;
      data      : a_gnutls_datum_t;
      signature : access gnutls_datum_t) return C.int
     with Import, Convention => C;

   function gnutls_x509_crt_import
     (cert   : gnutls_x509_crt_t;
      data   : a_gnutls_datum_t;
      format : gnutls_x509_crt_fmt_t) return C.int
     with Import, Convention => C;

   function gnutls_x509_crt_get_dn
     (cert       : gnutls_x509_crt_t;
      buf        : CS.chars_ptr;
      sizeof_buf : access C.size_t) return C.int
     with Import, Convention => C;

   function gnutls_x509_crt_get_dn_by_oid
     (cert     : gnutls_x509_crt_t;
      oid      : chars_constant_access;
      indx     : C.int;
      raw_flag : C.unsigned;
      buf      : System.Address;
      buf_size : access C.size_t) return C.int
     with Import, Convention => C;

   function gnutls_x509_crt_get_issuer_dn
     (cert       : gnutls_x509_crt_t;
      buf        : CS.chars_ptr;
      sizeof_buf : access C.size_t) return C.int
     with Import, Convention => C;

   function gnutls_x509_crt_get_serial
     (cert       : gnutls_x509_crt_t;
      buf        : System.Address;
      sizeof_buf : access C.size_t) return C.int
     with Import, Convention => C;

   function gnutls_x509_crt_get_activation_time
     (cert : gnutls_x509_crt_t) return time_t
     with Import, Convention => C;

   function gnutls_x509_crt_get_expiration_time
     (cert : gnutls_x509_crt_t) return time_t
     with Import, Convention => C;

   function gnutls_pcert_list_import_x509_raw
     (pcerts    : access gnutls_pcert_st;
      pcert_max : access C.unsigned;
      data      : a_gnutls_datum_t;
      format    : gnutls_x509_crt_fmt_t;
      flags     : C.unsigned) return C.int
     with Import, Convention => C;

   procedure gnutls_pcert_deinit (pcerts : in out gnutls_pcert_st)
     with Import, Convention => C;

   function gnutls_global_init return C.int
     with Import, Convention => C;

   procedure gnutls_global_deinit
     with Import, Convention => C;

   procedure gnutls_global_set_mem_functions
     (alloc_func        : gnutls_alloc_function;
      secure_alloc_func : gnutls_alloc_function;
      is_secure_func    : gnutls_is_secure_function;
      realloc_func      : gnutls_realloc_function;
      free_func         : gnutls_free_function)
     with Import, Convention => C;

   procedure gnutls_global_set_mem_functions
     (alloc_func        : System.Address;
      secure_alloc_func : System.Address;
      is_secure_func    : gnutls_is_secure_function;
      realloc_func      : System.Address;
      free_func         : gnutls_free_function)
     with Import, Convention => C;
   --  Could not use strict typing because System.Memory redefined size_t type

   procedure gnutls_global_set_log_function (log_func : gnutls_log_func)
     with Import, Convention => C;

   procedure gnutls_global_set_log_level (level : C.int)
     with Import, Convention => C;

   procedure gnutls_global_set_audit_log_function
     (log_func : gnutls_audit_log_func)
     with Import, Convention => C;

   function gnutls_sec_param_to_pk_bits
     (algo  : gnutls_pk_algorithm_t;
      param : gnutls_sec_param_t) return C.unsigned
     with Import, Convention => C;

   function gnutls_dh_params_init
     (p1 : access gnutls_dh_params_t) return C.int
     with Import, Convention => C;

   procedure gnutls_dh_params_deinit (p1 : gnutls_dh_params_t)
     with Import, Convention => C;

   function gnutls_dh_params_import_raw
     (dh_params : gnutls_dh_params_t;
      prime     : a_gnutls_datum_t;
      generator : a_gnutls_datum_t) return C.int
     with Import, Convention => C;

   function gnutls_dh_params_import_pkcs3
     (params       : gnutls_dh_params_t;
      pkcs3_params : a_gnutls_datum_t;
      format       : gnutls_x509_crt_fmt_t) return C.int
     with Import, Convention => C;

   function gnutls_dh_params_generate2
     (params : gnutls_dh_params_t;
      bits   : C.unsigned) return C.int
     with Import, Convention => C;

   function gnutls_dh_params_export_pkcs3
     (params           : gnutls_dh_params_t;
      format           : gnutls_x509_crt_fmt_t;
      params_data      : a_unsigned_char_t;
      params_data_size : a_size_t) return C.int
     with Import, Convention => C;

   function gnutls_dh_params_export_raw
     (params    : gnutls_dh_params_t;
      prime     : a_gnutls_datum_t;
      generator : a_gnutls_datum_t;
      bits      : access C.unsigned) return C.int
     with Import, Convention => C;

   function gnutls_dh_params_cpy
     (dst : gnutls_dh_params_t;
      src : gnutls_dh_params_t) return C.int
     with Import, Convention => C;

   function gnutls_transport_get_ptr
     (session : gnutls_session_t) return gnutls_transport_ptr_t
     with Import, Convention => C;

   procedure gnutls_transport_get_ptr2
     (session  : gnutls_session_t;
      recv_ptr : gnutls_transport_ptr_t;
      send_ptr : gnutls_transport_ptr_t)
     with Import, Convention => C;

   procedure gnutls_transport_set_lowat
     (session : gnutls_session_t; num : C.int)
     with Import, Convention => C;

   procedure gnutls_transport_set_ptr
     (session : gnutls_session_t; ptr : gnutls_transport_ptr_t)
     with Import, Convention => C;

   procedure gnutls_transport_set_push_function
     (session : gnutls_session_t; push_func : System.Address)
     with Import, Convention => C;

   procedure gnutls_transport_set_pull_function
     (session : gnutls_session_t; pull_func : System.Address)
     with Import, Convention => C;

   procedure gnutls_session_set_ptr
     (session : gnutls_session_t; ptr : System.Address)
     with Import, Convention => C;

   function gnutls_session_get_ptr
     (session : gnutls_session_t) return System.Address
     with Import, Convention => C;

   procedure gnutls_openpgp_send_key
     (session : gnutls_session_t; status : gnutls_openpgp_key_status_t)
     with Import, Convention => C;

   function gnutls_fingerprint
     (algo        : gnutls_digest_algorithm_t;
      data        : a_gnutls_datum_t;
      result      : System.Address;
      result_size : a_size_t) return C.int
     with Import, Convention => C;

   procedure gnutls_srp_free_client_credentials
     (sc : gnutls_srp_client_credentials_t)
     with Import, Convention => C;

   function gnutls_srp_allocate_client_credentials
     (sc : access gnutls_srp_client_credentials_t) return C.int
     with Import, Convention => C;

   function gnutls_srp_set_client_credentials
     (res      : gnutls_srp_client_credentials_t;
      username : CS.chars_ptr;
      password : CS.chars_ptr) return C.int
     with Import, Convention => C;

   procedure gnutls_srp_free_server_credentials
     (sc : gnutls_srp_server_credentials_t)
     with Import, Convention => C;

   function gnutls_srp_allocate_server_credentials
     (sc : access gnutls_srp_server_credentials_t) return C.int
     with Import, Convention => C;

   function gnutls_srp_set_server_credentials_file
     (res                : gnutls_srp_server_credentials_t;
      password_file      : CS.chars_ptr;
      password_conf_file : CS.chars_ptr) return C.int
     with Import, Convention => C;

   function gnutls_srp_server_get_username
     (state : gnutls_session_t) return CS.chars_ptr
     with Import, Convention => C;

   function gnutls_srp_verifier
     (username : CS.chars_ptr;
      password : CS.chars_ptr;
      salt     : a_gnutls_datum_t;
      g        : a_gnutls_datum_t;
      n        : a_gnutls_datum_t;
      res      : a_gnutls_datum_t) return C.int
     with Import, Convention => C;

   procedure gnutls_srp_set_server_credentials_function
     (p1 : gnutls_srp_server_credentials_t;
      p2 : gnutls_srp_server_credentials_function)
     with Import, Convention => C;

   procedure gnutls_srp_set_client_credentials_function
     (p1 : gnutls_srp_client_credentials_t;
      p2 : gnutls_srp_client_credentials_function)
     with Import, Convention => C;

   function gnutls_auth_get_type
     (session : gnutls_session_t) return gnutls_credentials_type_t
     with Import, Convention => C;

   function gnutls_auth_server_get_type
     (session : gnutls_session_t) return gnutls_credentials_type_t
     with Import, Convention => C;

   function gnutls_auth_client_get_type
     (session : gnutls_session_t) return gnutls_credentials_type_t
     with Import, Convention => C;

   procedure gnutls_dh_set_prime_bits
     (session : gnutls_session_t; bits : C.int)
     with Import, Convention => C;

   function gnutls_dh_get_secret_bits (p1 : gnutls_session_t) return C.int
     with Import, Convention => C;

   function gnutls_dh_get_peers_public_bits
     (p1 : gnutls_session_t) return C.int
     with Import, Convention => C;

   function gnutls_dh_get_prime_bits (p1 : gnutls_session_t) return C.int
     with Import, Convention => C;

   function gnutls_dh_get_group
     (p1    : gnutls_session_t;
      gen   : a_gnutls_datum_t;
      prime : a_gnutls_datum_t) return C.int
     with Import, Convention => C;

   function gnutls_dh_get_pubkey
     (p1 : gnutls_session_t; pub : a_gnutls_datum_t) return C.int
     with Import, Convention => C;

   function gnutls_rsa_export_get_pubkey
     (session : gnutls_session_t;
      exp     : a_gnutls_datum_t;
      c_mod   : a_gnutls_datum_t) return C.int
     with Import, Convention => C;

   function gnutls_rsa_export_get_modulus_bits
     (session : gnutls_session_t) return C.int
     with Import, Convention => C;

   procedure gnutls_certificate_set_retrieve_function2
     (cred : gnutls_certificate_credentials_t;
      func : gnutls_certificate_retrieve_function2)
     with Import, Convention => C;

   procedure gnutls_certificate_client_set_retrieve_function
     (cred : gnutls_certificate_credentials_t;
      func : gnutls_certificate_client_retrieve_function)
     with Import, Convention => C;

   procedure gnutls_certificate_server_set_retrieve_function
     (cred : gnutls_certificate_credentials_t;
      func : gnutls_certificate_server_retrieve_function)
     with Import, Convention => C;

   procedure gnutls_certificate_server_set_request
     (p1 : gnutls_session_t; p2 : gnutls_certificate_request_t)
     with Import, Convention => C;

   function gnutls_certificate_get_peers
     (p1        : gnutls_session_t;
      list_size : access C.unsigned) return a_gnutls_datum_t
     with Import, Convention => C;

   function gnutls_certificate_get_ours
     (session : gnutls_session_t) return a_gnutls_datum_t
     with Import, Convention => C;

   function gnutls_certificate_activation_time_peers
     (session : gnutls_session_t) return time_t
     with Import, Convention => C;

   function gnutls_certificate_expiration_time_peers
     (session : gnutls_session_t) return time_t
     with Import, Convention => C;

   function gnutls_certificate_client_get_request_status
     (p1 : gnutls_session_t) return C.int
     with Import, Convention => C;

   function gnutls_certificate_verify_peers2
     (p1 : gnutls_session_t; status : access C.unsigned) return C.int
     with Import, Convention => C;

   function gnutls_pem_base64_encode
     (header      : CS.chars_ptr;
      data        : a_gnutls_datum_t;
      result      : CS.chars_ptr;
      result_size : a_size_t) return C.int
     with Import, Convention => C;

   function gnutls_pem_base64_decode
     (header      : CS.chars_ptr;
      b64_data    : a_gnutls_datum_t;
      result      : a_unsigned_char_t;
      result_size : a_size_t) return C.int
     with Import, Convention => C;

   function gnutls_pem_base64_encode_alloc
     (header : CS.chars_ptr;
      data   : a_gnutls_datum_t;
      result : a_gnutls_datum_t) return C.int
     with Import, Convention => C;

   function gnutls_pem_base64_decode_alloc
     (header   : CS.chars_ptr;
      b64_data : a_gnutls_datum_t;
      result   : a_gnutls_datum_t) return C.int
     with Import, Convention => C;

   procedure gnutls_certificate_set_params_function
     (res  : gnutls_certificate_credentials_t;
      func : gnutls_params_function)
     with Import, Convention => C;

   procedure gnutls_anon_set_params_function
     (res  : gnutls_anon_server_credentials_t;
      func : gnutls_params_function)
     with Import, Convention => C;

   -----------------------------
   -- SessionTicket, RFC 5077 --
   -----------------------------

   function gnutls_session_ticket_key_generate
     (key : access gnutls_datum_t) return C.int with Import, Convention => C;

   function gnutls_session_ticket_enable_client
     (session : gnutls_session_t) return C.int with Import, Convention => C;

   function gnutls_session_ticket_enable_server
     (session : gnutls_session_t;
      key     : access constant gnutls_datum_t) return C.int
     with Import, Convention => C;

   function gnutls_key_generate
     (key : access gnutls_datum_t; key_size : C.unsigned) return C.int
     with Import, Convention => C;

   --------------------------------------------------------------------
   -- Tricks to support AWS.Net.SSL specification compatibility with --
   -- OpenSSL thin binding.                                          --
   --------------------------------------------------------------------

   function SSLeay (Dummy : C.int := 0) return C.int
     with Import, Convention => C, External_Name => "gnutls_check_version";

   subtype SSL_Handle is gnutls_session_t;

   Null_Handle : constant SSL_Handle := null;

   Null_Private_Key : constant Private_Key := null;

   type Boolean_Access is access Boolean;

   type BIO_Access is record
      Handshaken : Boolean_Access;
   end record;
   --  Need to be renamed into something like Internal_Type together with
   --  OpenSSL implementation after merge into trunk.

end SSL.Thin;
