------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2002-2017, AdaCore                     --
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

--  Thin binding to the LDAP API (derived from ldap.h)

--  This has been tested with OpenLDAP and wldap32.dll on Windows paltforms

with Interfaces.C.Strings;
with System;

package AWS.LDAP.Thin is

   package C renames Interfaces.C;

   use type Interfaces.C.int;

   subtype chars_ptr is C.Strings.chars_ptr;

   function NS (S : String) return chars_ptr renames C.Strings.New_String;

   LDAP_VERSION1 : constant := 1;
   LDAP_VERSION2 : constant := 2;
   LDAP_VERSION3 : constant := 3;

   LDAP_VERSION_MIN : constant := LDAP_VERSION2;
   LDAP_VERSION     : constant := LDAP_VERSION2;
   LDAP_VERSION_MAX : constant := LDAP_VERSION3;

   --  We'll use 2000+draft revision for our API version number
   --  As such, the number will be above the old RFC but below
   --  whatever number does finally get assigned

   LDAP_API_VERSION : constant := 2004;
   LDAP_VENDOR_NAME : constant C.Strings.chars_ptr := NS ("OpenLDAP");

   --  vendor version number
   --       2.0.0  -> 20000
   --       2.3.16 -> 20316

   LDAP_VENDOR_VERSION : constant := 20023;

   --  OpenLDAP API Features

   LDAP_API_FEATURE_X_OPENLDAP : constant := LDAP_VENDOR_VERSION;

   LDAP_API_FEATURE_THREAD_SAFE : constant := 1;

   LDAP_PORT  : constant := 389;   --  ldap://     default LDAP port
   LDAPS_PORT : constant := 636;   --  ldaps://    default LDAP over TLS port

   LDAP_ROOT_DSE                   : constant chars_ptr := NS ("");
   LDAP_NO_ATTRS                   : constant chars_ptr := NS ("1.1");
   LDAP_ALL_USER_ATTRIBUTES        : constant chars_ptr := NS ("*");
   LDAP_ALL_OPERATIONAL_ATTRIBUTES : constant chars_ptr := NS ("+");
   --  OpenLDAP extension

   --  LDAP_OPTions defined by draft-ldapext-ldap-c-api-02
   --  0x0000 - 0x0fff reserved for api options
   --  0x1000 - 0x3fff reserved for api extended options
   --  0x4000 - 0x7fff reserved for private and experimental options

   LDAP_OPT_API_INFO               : constant := 16#0000#;
   LDAP_OPT_DESC                   : constant := 16#0001#;  --  deprecated
   LDAP_OPT_DEREF                  : constant := 16#0002#;
   LDAP_OPT_SIZELIMIT              : constant := 16#0003#;
   LDAP_OPT_TIMELIMIT              : constant := 16#0004#;
   LDAP_OPT_REFERRALS              : constant := 16#0008#;
   LDAP_OPT_RESTART                : constant := 16#0009#;
   LDAP_OPT_PROTOCOL_VERSION       : constant := 16#0011#;
   LDAP_OPT_SERVER_CONTROLS        : constant := 16#0012#;
   LDAP_OPT_CLIENT_CONTROLS        : constant := 16#0013#;
   LDAP_OPT_API_FEATURE_INFO       : constant := 16#0015#;
   LDAP_OPT_HOST_NAME              : constant := 16#0030#;
   LDAP_OPT_ERROR_NUMBER           : constant := 16#0031#;
   LDAP_OPT_ERROR_STRING           : constant := 16#0032#;
   LDAP_OPT_MATCHED_DN             : constant := 16#0033#;

   LDAP_OPT_PRIVATE_EXTENSION_BASE : constant := 16#4000#;
   --  to 16#7FFF# inclusive

   --  private and experimental options

   --  OpenLDAP specific options

   LDAP_OPT_DEBUG_LEVEL     : constant := 16#5001#; -- debug level
   LDAP_OPT_TIMEOUT         : constant := 16#5002#; -- default timeout
   LDAP_OPT_REFHOPLIMIT     : constant := 16#5003#; -- ref hop limit
   LDAP_OPT_NETWORK_TIMEOUT : constant := 16#5005#; -- socket level timeout
   LDAP_OPT_URI             : constant := 16#5006#;

   --  OpenLDAP TLS options

   LDAP_OPT_X_TLS              : constant := 16#6000#;
   LDAP_OPT_X_TLS_CTX          : constant := 16#6001#;  -- SSL CTX
   LDAP_OPT_X_TLS_CACERTFILE   : constant := 16#6002#;
   LDAP_OPT_X_TLS_CACERTDIR    : constant := 16#6003#;
   LDAP_OPT_X_TLS_CERTFILE     : constant := 16#6004#;
   LDAP_OPT_X_TLS_KEYFILE      : constant := 16#6005#;
   LDAP_OPT_X_TLS_REQUIRE_CERT : constant := 16#6006#;
   LDAP_OPT_X_TLS_CIPHER_SUITE : constant := 16#6008#;
   LDAP_OPT_X_TLS_RANDOM_FILE  : constant := 16#6009#;

   LDAP_OPT_X_TLS_NEVER        : constant := 0;
   LDAP_OPT_X_TLS_HARD         : constant := 1;
   LDAP_OPT_X_TLS_DEMAND       : constant := 2;
   LDAP_OPT_X_TLS_ALLOW        : constant := 3;
   LDAP_OPT_X_TLS_TRY          : constant := 4;

   --  OpenLDAP SASL options

   LDAP_OPT_X_SASL_MECH         : constant := 16#6100#;
   LDAP_OPT_X_SASL_REALM        : constant := 16#6101#;
   LDAP_OPT_X_SASL_AUTHCID      : constant := 16#6102#;
   LDAP_OPT_X_SASL_AUTHZID      : constant := 16#6103#;
   LDAP_OPT_X_SASL_SSF          : constant := 16#6104#; -- read-only
   LDAP_OPT_X_SASL_SSF_EXTERNAL : constant := 16#6105#; -- write-only
   LDAP_OPT_X_SASL_SECPROPS     : constant := 16#6106#; -- write-only
   LDAP_OPT_X_SASL_SSF_MIN      : constant := 16#6107#;
   LDAP_OPT_X_SASL_SSF_MAX      : constant := 16#6108#;
   LDAP_OPT_X_SASL_MAXBUFSIZE   : constant := 16#6109#;

   --  on/off values
   LDAP_OPT_ON  : constant := 1;
   LDAP_OPT_OFF : constant := 0;

   --  ldap_get_option() and ldap_set_option() return values.
   --  As later versions may return other values indicating
   --  failure, current applications should only compare returned
   --  value against LDAP_OPT_SUCCESS.

   LDAP_OPT_SUCCESS : constant := 0;
   LDAP_OPT_ERROR   : constant := -1;

   LDAP_API_INFO_VERSION : constant := 1;

   LDAP_FEATURE_INFO_VERSION : constant := 1;
   --  version of api feature structure

   --  LDAP Controls

   LDAP_CONTROL_MANAGEDSAIT  : constant chars_ptr :=
                                 NS ("2.16.840.1.113730.3.4.2");

   --  Experimental Controls

   LDAP_CONTROL_SORTREQUEST  : constant chars_ptr :=
                                 NS ("1.2.840.113556.1.4.473");
   LDAP_CONTROL_SORTRESPONSE : constant chars_ptr :=
                                 NS ("1.2.840.113556.1.4.474");
   LDAP_CONTROL_VLVREQUEST   : constant chars_ptr :=
                                 NS ("2.16.840.1.113730.3.4.9");
   LDAP_CONTROL_VLVRESPONSE  : constant chars_ptr :=
                                 NS ("2.16.840.1.113730.3.4.10");

   --  LDAP Unsolicited Notifications

   LDAP_NOTICE_OF_DISCONNECTION : constant chars_ptr :=
                                    NS ("1.3.6.1.4.1.1466.20036");
   LDAP_NOTICE_DISCONNECT    : constant chars_ptr :=
                                 LDAP_NOTICE_OF_DISCONNECTION;

   --  LDAP Extended Operations

   LDAP_EXOP_START_TLS       : constant chars_ptr :=
                                 NS ("1.3.6.1.4.1.1466.20037");

   LDAP_EXOP_X_MODIFY_PASSWD : constant chars_ptr :=
                                 NS ("1.3.6.1.4.1.4203.1.11.1");

   LDAP_TAG_EXOP_X_MODIFY_PASSWD_ID  : constant := 16#80#;
   LDAP_TAG_EXOP_X_MODIFY_PASSWD_OLD : constant := 16#81#;
   LDAP_TAG_EXOP_X_MODIFY_PASSWD_NEW : constant := 16#82#;
   LDAP_TAG_EXOP_X_MODIFY_PASSWD_GEN : constant := 16#80#;

   --  Specific LDAP instantiations of BER types we know about

   --  Overview of LBER tag construction
   --
   --       Bits
   --       ______
   --       8 7 | CLASS
   --       0 0 = UNIVERSAL
   --       0 1 = APPLICATION
   --       1 0 = CONTEXT-SPECIFIC
   --       1 1 = PRIVATE
   --               _____
   --               | 6 | DATA-TYPE
   --                 0 = PRIMITIVE
   --                 1 = CONSTRUCTED
   --                       ___________
   --                       | 5 ... 1 | TAG-NUMBER

   --  General stuff

   LDAP_TAG_MESSAGE        : constant := 16#30#;
   --  Constructed + 16
   LDAP_TAG_MSGID          : constant := 16#02#;
   --  Integer
   LDAP_TAG_LDAPDN         : constant := 16#04#;
   --  Octect string
   LDAP_TAG_LDAPCRED       : constant := 16#04#;
   --  Octect string
   LDAP_TAG_CONTROLS       : constant := 16#A0#;
   --  Context specific + constructed + 0
   LDAP_TAG_REFERRAL       : constant := 16#A3#;
   --  Context specific + constructed + 3

   LDAP_TAG_NEWSUPERIOR    : constant := 16#80#;
   --  Context-specific + primitive + 0

   LDAP_TAG_EXOP_REQ_OID   : constant := 16#80#;
   --  Context specific + primitive
   LDAP_TAG_EXOP_REQ_VALUE : constant := 16#81#;
   --  Context specific + primitive
   LDAP_TAG_EXOP_RES_OID   : constant := 16#8a#;
   --  Context specific + primitive
   LDAP_TAG_EXOP_RES_VALUE : constant := 16#8b#;
   --  Context specific + primitive

   LDAP_TAG_SASL_RES_CREDS : constant := 16#87#;
   --  Context specific + primitive

   --  Possible operations a client can invoke

   LDAP_REQ_BIND                   : constant := 16#60#;
   --  Application + constructed
   LDAP_REQ_UNBIND                 : constant := 16#42#;
   --  Application + primitive
   LDAP_REQ_SEARCH                 : constant := 16#63#;
   --  Application + constructed
   LDAP_REQ_MODIFY                 : constant := 16#66#;
   --  Application + constructed
   LDAP_REQ_ADD                    : constant := 16#68#;
   --  Application + constructed
   LDAP_REQ_DELETE                 : constant := 16#4a#;
   --  Application + primitive
   LDAP_REQ_MODDN                  : constant := 16#6c#;
   --  Application + constructed
   LDAP_REQ_MODRDN                 : constant := LDAP_REQ_MODDN;
   LDAP_REQ_RENAME                 : constant := LDAP_REQ_MODDN;
   LDAP_REQ_COMPARE                : constant := 16#6e#;
   --  Application + constructed
   LDAP_REQ_ABANDON                : constant := 16#50#;
   --  Application + primitive
   LDAP_REQ_EXTENDED               : constant := 16#77#;
   --  Application + constructed

   --  Possible result types a server can return

   LDAP_RES_BIND                   : constant := 16#61#;
   --  Application + constructed
   LDAP_RES_SEARCH_ENTRY           : constant := 16#64#;
   --  Application + constructed
   LDAP_RES_SEARCH_REFERENCE       : constant := 16#73#;
   --  V3: application + constructed
   LDAP_RES_SEARCH_RESULT          : constant := 16#65#;
   --  Application + constructed
   LDAP_RES_MODIFY                 : constant := 16#67#;
   --  Application + constructed
   LDAP_RES_ADD                    : constant := 16#69#;
   --  Application + constructed
   LDAP_RES_DELETE                 : constant := 16#6b#;
   --  Application + constructed
   LDAP_RES_MODDN                  : constant := 16#6d#;
   --  Application + constructed
   LDAP_RES_MODRDN                 : constant := LDAP_RES_MODDN;
   --  Application + constructed
   LDAP_RES_RENAME                 : constant := LDAP_RES_MODDN;
   --  Application + constructed
   LDAP_RES_COMPARE                : constant := 16#6f#;
   --  Application + constructed
   LDAP_RES_EXTENDED               : constant := 16#78#;
   --  V3: application + constructed
   LDAP_RES_EXTENDED_PARTIAL       : constant := 16#79#;
   --  V3+: application + constructed

   LDAP_RES_ANY                    : constant := -1;
   LDAP_RES_UNSOLICITED            : constant := 0;

   --  Sasl methods

   LDAP_SASL_SIMPLE : constant System.Address := System.Null_Address;
   LDAP_SASL_NULL   : constant chars_ptr      := NS ("");

   --  Authentication methods available

   LDAP_AUTH_NONE   : constant := 16#00#; -- no authentication
   LDAP_AUTH_SIMPLE : constant := 16#80#; -- context specific + primitive
   LDAP_AUTH_SASL   : constant := 16#A3#; -- context specific + constructed
   LDAP_AUTH_KRBV4  : constant := 16#Ff#; -- means do both of the following
   LDAP_AUTH_KRBV41 : constant := 16#81#; -- context specific + primitive
   LDAP_AUTH_KRBV42 : constant := 16#82#; -- context specific + primitive

   --  Filter types

   LDAP_FILTER_AND        : constant := 16#A0#;
   --  Context specific + constructed */
   LDAP_FILTER_OR         : constant := 16#A1#;
   --  Context specific + constructed */
   LDAP_FILTER_NOT        : constant := 16#A2#;
   --  Context specific + constructed */
   LDAP_FILTER_EQUALITY   : constant := 16#A3#;
   --  Context specific + constructed */
   LDAP_FILTER_SUBSTRINGS : constant := 16#A4#;
   --  Context specific + constructed */
   LDAP_FILTER_GE         : constant := 16#A5#;
   --  Context specific + constructed */
   LDAP_FILTER_LE         : constant := 16#A6#;
   --  Context specific + constructed */
   LDAP_FILTER_PRESENT    : constant := 16#87#;
   --  Context specific + primitive   */
   LDAP_FILTER_APPROX     : constant := 16#A8#;
   --  Context specific + constructed */
   LDAP_FILTER_EXT        : constant := 16#A9#;
   --  Context specific + constructed

   --  Extended filter component types

   LDAP_FILTER_EXT_OID     : constant := 16#81#;     -- context specific
   LDAP_FILTER_EXT_TYPE    : constant := 16#82#;     -- context specific
   LDAP_FILTER_EXT_VALUE   : constant := 16#83#;     -- context specific
   LDAP_FILTER_EXT_DNATTRS : constant := 16#84#;     -- context specific

   --  Substring filter component types

   LDAP_SUBSTRING_INITIAL  : constant := 16#80#;     -- context specific
   LDAP_SUBSTRING_ANY      : constant := 16#81#;     -- context specific
   LDAP_SUBSTRING_FINAL    : constant := 16#82#;     -- context specific

   --  Search scopes

   LDAP_SCOPE_DEFAULT      : constant := -1;
   LDAP_SCOPE_BASE         : constant := 16#0000#;
   LDAP_SCOPE_ONELEVEL     : constant := 16#0001#;
   LDAP_SCOPE_SUBTREE      : constant := 16#0002#;

   --  Possible error codes we can return

   subtype Return_Code is C.int range -16#01# .. 16#61#;

   LDAP_SUCCESS                   : constant := 16#00#;
   LDAP_OPERATIONS_ERROR          : constant := 16#01#;
   LDAP_PROTOCOL_ERROR            : constant := 16#02#;
   LDAP_TIMELIMIT_EXCEEDED        : constant := 16#03#;
   LDAP_SIZELIMIT_EXCEEDED        : constant := 16#04#;
   LDAP_COMPARE_FALSE             : constant := 16#05#;
   LDAP_COMPARE_TRUE              : constant := 16#06#;
   LDAP_AUTH_METHOD_NOT_SUPPORTED : constant := 16#07#;
   LDAP_STRONG_AUTH_NOT_SUPPORTED : constant := LDAP_AUTH_METHOD_NOT_SUPPORTED;
   LDAP_STRONG_AUTH_REQUIRED      : constant := 16#08#;
   LDAP_PARTIAL_RESULTS           : constant := 16#09#; -- LDAPv2+ (not LDAPv3)

   LDAP_REFERRAL                       : constant := 16#0a#; -- LDAPv3
   LDAP_ADMINLIMIT_EXCEEDED            : constant := 16#0b#; -- LDAPv3
   LDAP_UNAVAILABLE_CRITICAL_EXTENSION : constant := 16#0c#; -- LDAPv3
   LDAP_CONFIDENTIALITY_REQUIRED       : constant := 16#0d#; -- LDAPv3
   LDAP_SASL_BIND_IN_PROGRESS          : constant := 16#0e#; -- LDAPv3

   subtype ATTR_ERROR is Return_Code range 16#10# .. 16#15#;

   function LDAP_ATTR_ERROR (n : Return_Code) return Boolean;

   LDAP_NO_SUCH_ATTRIBUTE         : constant := 16#10#;
   LDAP_UNDEFINED_TYPE            : constant := 16#11#;
   LDAP_INAPPROPRIATE_MATCHING    : constant := 16#12#;
   LDAP_CONSTRAINT_VIOLATION      : constant := 16#13#;
   LDAP_TYPE_OR_VALUE_EXISTS      : constant := 16#14#;
   LDAP_INVALID_SYNTAX            : constant := 16#15#;

   subtype NAME_ERROR is Return_Code range 16#20# .. 16#24#;

   function LDAP_NAME_ERROR (n : Return_Code) return Boolean;

   LDAP_NO_SUCH_OBJECT            : constant := 16#20#;
   LDAP_ALIAS_PROBLEM             : constant := 16#21#;
   LDAP_INVALID_DN_SYNTAX         : constant := 16#22#;
   LDAP_IS_LEAF                   : constant := 16#23#; -- not LDAPv3
   LDAP_ALIAS_DEREF_PROBLEM       : constant := 16#24#;

   subtype SECURITY_ERROR is Return_Code range 16#30# .. 16#32#;

   function LDAP_SECURITY_ERROR (n : Return_Code) return Boolean;

   LDAP_INAPPROPRIATE_AUTH        : constant := 16#30#;
   LDAP_INVALID_CREDENTIALS       : constant := 16#31#;
   LDAP_INSUFFICIENT_ACCESS       : constant := 16#32#;

   subtype SERVICE_ERROR is Return_Code range 16#33# .. 16#36#;

   function LDAP_SERVICE_ERROR (n : Return_Code) return Boolean;

   LDAP_BUSY                      : constant := 16#33#;
   LDAP_UNAVAILABLE               : constant := 16#34#;
   LDAP_UNWILLING_TO_PERFORM      : constant := 16#35#;
   LDAP_LOOP_DETECT               : constant := 16#36#;

   subtype UPDATE_ERROR is Return_Code range 16#40# .. 16#47#;

   function LDAP_UPDATE_ERROR (n : Return_Code) return Boolean;

   LDAP_NAMING_VIOLATION          : constant := 16#40#;
   LDAP_OBJECT_CLASS_VIOLATION    : constant := 16#41#;
   LDAP_NOT_ALLOWED_ON_NONLEAF    : constant := 16#42#;
   LDAP_NOT_ALLOWED_ON_RDN        : constant := 16#43#;
   LDAP_ALREADY_EXISTS            : constant := 16#44#;
   LDAP_NO_OBJECT_CLASS_MODS      : constant := 16#45#;
   LDAP_RESULTS_TOO_LARGE         : constant := 16#46#; -- CLDAP
   LDAP_AFFECTS_MULTIPLE_DSAS     : constant := 16#47#; -- LDAPv3

   LDAP_OTHER                     : constant := 16#50#;

   subtype API_ERROR is Return_Code range 16#51# .. 16#61#;

   function LDAP_API_ERROR (n : Return_Code) return Boolean;

   function LDAP_API_RESULT (n : Return_Code) return Boolean;

   LDAP_SERVER_DOWN               : constant := 16#51#;
   LDAP_LOCAL_ERROR               : constant := 16#52#;
   LDAP_ENCODING_ERROR            : constant := 16#53#;
   LDAP_DECODING_ERROR            : constant := 16#54#;
   LDAP_TIMEOUT                   : constant := 16#55#;
   LDAP_AUTH_UNKNOWN              : constant := 16#56#;
   LDAP_FILTER_ERROR              : constant := 16#57#;
   LDAP_USER_CANCELLED            : constant := 16#58#;
   LDAP_PARAM_ERROR               : constant := 16#59#;
   LDAP_NO_MEMORY                 : constant := 16#5a#;

   --  Not technically reserved for APIs
   LDAP_CONNECT_ERROR             : constant := 16#5b#;
   --  Draft-ietf-ldap-c-api-xx
   LDAP_NOT_SUPPORTED             : constant := 16#5c#;
   --  Draft-ietf-ldap-c-api-xx
   LDAP_CONTROL_NOT_FOUND         : constant := 16#5d#;
   --  Draft-ietf-ldap-c-api-xx
   LDAP_NO_RESULTS_RETURNED       : constant := 16#5e#;
   --  Draft-ietf-ldap-c-api-xx
   LDAP_MORE_RESULTS_TO_RETURN    : constant := 16#5f#;
   --  Draft-ietf-ldap-c-api-xx
   LDAP_CLIENT_LOOP               : constant := 16#60#;
   --  Draft-ietf-ldap-c-api-xx
   LDAP_REFERRAL_LIMIT_EXCEEDED   : constant := 16#61#;
   --  Draft-ietf-ldap-c-api-xx

   --  for modifications
   LDAP_MOD_ADD                   : constant := 16#0000#;
   LDAP_MOD_REPLACE               : constant := 16#0002#;
   LDAP_MOD_BVALUES               : constant := 16#0080#;

   LDAP_FILT_MAXSIZ        : constant := 1024;

   LDAP_DEREF_NEVER        : constant := 16#00#;
   LDAP_DEREF_SEARCHING    : constant := 16#01#;
   LDAP_DEREF_FINDING      : constant := 16#02#;
   LDAP_DEREF_ALWAYS       : constant := 16#03#;

   LDAP_NO_LIMIT           : constant := 0;

   --  How many messages to retrieve results for

   LDAP_MSG_ONE            : constant := 16#00#;
   LDAP_MSG_ALL            : constant := 16#01#;
   LDAP_MSG_RECEIVED       : constant := 16#02#;

   --  Types for ldap URL handling

   LDAP_URL_SUCCESS          : constant := 16#00#;
   --  Success
   LDAP_URL_ERR_MEM          : constant := 16#01#;
   --  Can't allocate memory space
   LDAP_URL_ERR_PARAM        : constant := 16#02#;
   --  Parameter is bad

   LDAP_URL_ERR_BADSCHEME    : constant := 16#03#;
   --  URL doesn't begin with "ldap[si]://"
   LDAP_URL_ERR_BADENCLOSURE : constant := 16#04#;
   --  URL is missing trailing ">"
   LDAP_URL_ERR_BADURL       : constant := 16#05#;
   --  URL is bad
   LDAP_URL_ERR_BADHOST      : constant := 16#06#;
   --  Host port is bad
   LDAP_URL_ERR_BADATTRS     : constant := 16#07#;
   --  Bad (or missing) attributes
   LDAP_URL_ERR_BADSCOPE     : constant := 16#08#;
   --  Scope string is invalid (or missing)
   LDAP_URL_ERR_BADFILTER    : constant := 16#09#;
   --  Bad or missing filter
   LDAP_URL_ERR_BADEXTS      : constant := 16#0a#;
   --  Bad or missing extensions

   --  API

   type LDAP_Type is private;
   --  An LDAP connection, this is a pointer to the C LDAP structure. Such
   --  object is returned by LDAP_Init.

   Null_LDAP_Type : constant LDAP_Type;

   --  Refer to any LDAP API documentation for a description of the routines
   --  below. Only the thick Ada binding in AWS.LDAP.Client is properly
   --  documented.

   --  Init LDAP

   function ldap_init
     (host : chars_ptr;
      port : C.int) return LDAP_Type
     with Import, Convention => C;

   function ldap_simple_bind
     (ld     : LDAP_Type;
      who    : chars_ptr;
      passwd : chars_ptr) return C.int
     with Import, Convention => C;

   function ldap_simple_bind_s
     (ld     : LDAP_Type;
      who    : chars_ptr;
      passwd : chars_ptr) return C.int
     with Import, Convention => C;

   function ldap_bind_s
     (ld         : LDAP_Type;
      who        : chars_ptr;
      passwd     : chars_ptr;
      authmethod : C.int) return C.int
     with Import, Convention => C;

   function ldap_unbind_s (ld : LDAP_Type) return C.int
     with Import, Convention => C;

   --  Search

   type LDAPMessage is private;
   --  LDAP message, this structure can contain the search result for example

   Null_LDAPMessage : constant LDAPMessage;

   type BerElement is private;

   function ldap_search_s
     (ld        : LDAP_Type;
      base      : chars_ptr;
      scope     : C.int;
      filter    : chars_ptr;
      attrs     : C.Strings.chars_ptr_array;
      attrsonly : C.int;
      res       : not null access LDAPMessage) return C.int
     with Import, Convention => C;

   function ldap_search_s
     (ld        : LDAP_Type;
      base      : chars_ptr;
      scope     : C.int;
      filter    : chars_ptr;
      attrs     : chars_ptr; -- To be able to pass a null ptr
      attrsonly : C.int;
      res       : not null access LDAPMessage) return C.int
     with Import, Convention => C;

   --  Add / Modify / Delete

   Max_Mod_Values : constant Natural := 64;
   --  Maximum amount of LDAPMod values supported

   subtype Mod_Value_Array is C.Strings.chars_ptr_array
     (C.size_t range 1 .. C.size_t (Max_Mod_Values));
   --  Storage array for modification values

   type Mod_Value_Array_Access is access Mod_Value_Array;

   type LDAPMod_Element is record
      Mod_Op     : C.int;
      Mod_Type   : chars_ptr;
      Mod_Values : Mod_Value_Array_Access;
   end record;
   type LDAPMod_Element_Access is access all LDAPMod_Element;
   --  LDAPMod element

   type LDAPMods is array (C.size_t range <>)
     of aliased LDAPMod_Element_Access;
   type LDAPMods_Access is access all LDAPMod_Element_Access
     with Convention => C;
   --  LDAPMods, array of modifications. This is the Ada representation
   --  of "LDAPMod **" on the C side. LDAPMods_Access points to the first
   --  element in the array (since we cannot pass fat-pointers to the
   --  C-API function).

   --  Add

   function ldap_add_s
     (ld    : LDAP_Type;
      dn    : chars_ptr;
      attrs : LDAPMods_Access) return C.int
     with Import, Convention => C;

   --  Modify

   function ldap_modify_s
     (ld   : LDAP_Type;
      dn   : chars_ptr;
      mods : LDAPMods_Access) return C.int
     with Import, Convention => C;

   --  Delete

   function ldap_delete_s
     (ld : LDAP_Type;
      dn : chars_ptr) return C.int
     with Import, Convention => C;

   --  Helpers

   function ldap_count_entries
     (ld    : LDAP_Type;
      chain : LDAPMessage) return C.int
     with Import, Convention => C;

   function ldap_first_entry
     (ld    : LDAP_Type;
      chain : LDAPMessage) return LDAPMessage
     with Import, Convention => C;

   function ldap_next_entry
     (ld      : LDAP_Type;
      entries : LDAPMessage) return LDAPMessage
     with Import, Convention => C;

   function ldap_get_dn
     (ld      : LDAP_Type;
      entries : LDAPMessage) return chars_ptr
     with Import, Convention => C;

   function ldap_first_attribute
     (ld      : LDAP_Type;
      entries : LDAPMessage;
      ber     : not null access BerElement) return chars_ptr
     with Import, Convention => C;

   function ldap_next_attribute
     (ld       : LDAP_Type;
      entries  : LDAPMessage;
      ber      : BerElement) return chars_ptr
     with Import, Convention => C;

   function ldap_msgfree (lm : LDAPMessage) return C.int
     with Import, Convention => C;

   function ldap_msgid (lm : LDAPMessage) return C.int
     with Import, Convention => C;

   function ldap_msgtype (lm : LDAPMessage) return C.int
     with Import, Convention => C;

   subtype Constr_Char_Array is C.char_array (C.size_t);
   type Constr_Char_Array_Access is access Constr_Char_Array;

   type Ber_Val is record
      BV_Len : C.unsigned_long;
      BV_Val : Constr_Char_Array_Access;
   end record;

   type Ber_Val_Access is access Ber_Val;
   type Ber_Val_Array is array (C.size_t range <>) of aliased Ber_Val_Access;
   subtype Constr_Ber_Val_Array is Ber_Val_Array (C.size_t);
   type Constr_Ber_Val_Array_Access is access Constr_Ber_Val_Array;
   --  This is a "struct berval **" on the C side. We emulate this with a
   --  pointer to a constrained Ber_Val_Array. Note that it is not possible to
   --  dereference a Constr_Ber_Val_Array_Access at the Ada level because this
   --  is a dummy type which is very big! To access individual element you
   --  should use Item routine below.

   function Item
     (Set   : Constr_Ber_Val_Array_Access;
      Index : C.int) return C.char_array
     with Inline;
   --  Returns item at positon Index in Set

   subtype Attribute_Set is C.Strings.chars_ptr_array (C.size_t);
   type Attribute_Set_Access is access all Attribute_Set;
   --  This is a "char **" on the C side. We emulate this with a pointer to a
   --  constrained chars_ptr_array. Note that it is not possible to
   --  dereference an Attribute_Set_Access at the Ada level because this is a
   --  dummy type which is very big! To access individual element you should
   --  use Item routine below.

   function Item
     (Set   : Attribute_Set_Access;
      Index : C.int) return chars_ptr
     with Inline;
   --  Returns item at positon Index in Set

   function ldap_get_values_len
     (ld      : LDAP_Type;
      entries : LDAPMessage;
      target  : chars_ptr) return Constr_Ber_Val_Array_Access
     with Import, Convention => C;

   function ldap_count_values (V : Attribute_Set_Access) return C.int
     with Import, Convention => C;

   function ldap_count_values_len
     (V : Constr_Ber_Val_Array_Access) return C.int
     with Import, Convention => C;

   procedure ldap_value_free_len (V : Constr_Ber_Val_Array_Access)
     with Import, Convention => C;

   procedure ldap_value_free (V : Attribute_Set_Access)
     with Import, Convention => C;

   function ldap_err2string (err : C.int) return chars_ptr
     with Import, Convention => C;

   function ldap_dn2ufn (dn : chars_ptr) return chars_ptr
     with Import, Convention => C;

   function ldap_explode_dn
     (dn      : chars_ptr;
      notypes : C.int) return Attribute_Set_Access
     with Import, Convention => C;

   procedure ber_free (BER : BerElement; fbuf : C.int)
     with Import, Convention => C;

private

   type LDAP_Type   is new System.Address;
   type LDAPMessage is new System.Address;
   type BerElement  is new System.Address;
   --  All these types are handled as opaque types. We do not need to expose
   --  the structure here. These objects created and a pointer to them is
   --  returned by routines above. We just pass this pointer around and we
   --  must make sure that we free the structure.

   Null_LDAP_Type   : constant LDAP_Type :=
                        LDAP_Type (System.Null_Address);

   Null_LDAPMessage : constant LDAPMessage :=
                        LDAPMessage (System.Null_Address);

end AWS.LDAP.Thin;
