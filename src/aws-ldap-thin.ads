------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                            Copyright (C) 2002                            --
--                               ACT-Europe                                 --
--                                                                          --
--  Authors: Dmitriy Anisimkov - Pascal Obry                                --
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

--  $Id$:

--  Thin binding to the LDAP API (derived from ldap.h)

--  This has been tested with OpenLDAP and wldap32.dll on Windows paltforms.

with System;
with Interfaces.C.Strings;

package AWS.LDAP.Thin is

   package C renames Interfaces.C;

   function NS (S : in String) return C.Strings.chars_ptr
     renames C.Strings.New_String;
   subtype chars_ptr is C.Strings.chars_ptr;

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

   LDAP_CONTROL_MANAGEDSAIT  : constant chars_ptr
     := NS ("2.16.840.1.113730.3.4.2");

   --  Experimental Controls

   LDAP_CONTROL_SORTREQUEST  : constant chars_ptr
     := NS ("1.2.840.113556.1.4.473");
   LDAP_CONTROL_SORTRESPONSE : constant chars_ptr
     := NS ("1.2.840.113556.1.4.474");
   LDAP_CONTROL_VLVREQUEST   : constant chars_ptr
     := NS ("2.16.840.1.113730.3.4.9");
   LDAP_CONTROL_VLVRESPONSE  : constant chars_ptr
     := NS ("2.16.840.1.113730.3.4.10");

   --  LDAP Unsolicited Notifications

   LDAP_NOTICE_OF_DISCONNECTION : constant chars_ptr
     := NS ("1.3.6.1.4.1.1466.20036");
   LDAP_NOTICE_DISCONNECT    : constant chars_ptr
     := LDAP_NOTICE_OF_DISCONNECTION;

   --  LDAP Extended Operations
   LDAP_EXOP_START_TLS       : constant chars_ptr
     := NS ("1.3.6.1.4.1.1466.20037");

   LDAP_EXOP_X_MODIFY_PASSWD : constant chars_ptr
     := NS ("1.3.6.1.4.1.4203.1.11.1");

   LDAP_TAG_EXOP_X_MODIFY_PASSWD_ID  : constant := 16#80#;
   LDAP_TAG_EXOP_X_MODIFY_PASSWD_OLD : constant := 16#81#;
   LDAP_TAG_EXOP_X_MODIFY_PASSWD_NEW : constant := 16#82#;
   LDAP_TAG_EXOP_X_MODIFY_PASSWD_GEN : constant := 16#80#;

   --  specific LDAP instantiations of BER types we know about

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

   --  general stuff

   LDAP_TAG_MESSAGE        : constant := 16#30#;
   --  constructed + 16
   LDAP_TAG_MSGID          : constant := 16#02#;
   --  integer
   LDAP_TAG_LDAPDN         : constant := 16#04#;
   --  octect string
   LDAP_TAG_LDAPCRED       : constant := 16#04#;
   --  octect string
   LDAP_TAG_CONTROLS       : constant := 16#A0#;
   --  context specific + constructed + 0
   LDAP_TAG_REFERRAL       : constant := 16#A3#;
   --  context specific + constructed + 3

   LDAP_TAG_NEWSUPERIOR    : constant := 16#80#;
   --  context-specific + primitive + 0

   LDAP_TAG_EXOP_REQ_OID   : constant := 16#80#;
   --  context specific + primitive
   LDAP_TAG_EXOP_REQ_VALUE : constant := 16#81#;
   --  context specific + primitive
   LDAP_TAG_EXOP_RES_OID   : constant := 16#8a#;
   --  context specific + primitive
   LDAP_TAG_EXOP_RES_VALUE : constant := 16#8b#;
   --  context specific + primitive

   LDAP_TAG_SASL_RES_CREDS : constant := 16#87#;
   --  context specific + primitive


   --  possible operations a client can invoke

   LDAP_REQ_BIND                   : constant := 16#60#;
   --  application + constructed
   LDAP_REQ_UNBIND                 : constant := 16#42#;
   --  application + primitive
   LDAP_REQ_SEARCH                 : constant := 16#63#;
   --  application + constructed
   LDAP_REQ_MODIFY                 : constant := 16#66#;
   --  application + constructed
   LDAP_REQ_ADD                    : constant := 16#68#;
   --  application + constructed
   LDAP_REQ_DELETE                 : constant := 16#4a#;
   --  application + primitive
   LDAP_REQ_MODDN                  : constant := 16#6c#;
   --  application + constructed
   LDAP_REQ_MODRDN                 : constant := LDAP_REQ_MODDN;
   LDAP_REQ_RENAME                 : constant := LDAP_REQ_MODDN;
   LDAP_REQ_COMPARE                : constant := 16#6e#;
   --  application + constructed
   LDAP_REQ_ABANDON                : constant := 16#50#;
   --  application + primitive
   LDAP_REQ_EXTENDED               : constant := 16#77#;
   --  application + constructed

   --  possible result types a server can return

   LDAP_RES_BIND                   : constant := 16#61#;
   --   application + constructed
   LDAP_RES_SEARCH_ENTRY           : constant := 16#64#;
   --  application + constructed
   LDAP_RES_SEARCH_REFERENCE       : constant := 16#73#;
   --  V3: application + constructed
   LDAP_RES_SEARCH_RESULT          : constant := 16#65#;
   --  application + constructed
   LDAP_RES_MODIFY                 : constant := 16#67#;
   --  application + constructed
   LDAP_RES_ADD                    : constant := 16#69#;
   --  application + constructed
   LDAP_RES_DELETE                 : constant := 16#6b#;
   --  application + constructed
   LDAP_RES_MODDN                  : constant := 16#6d#;
   --  application + constructed
   LDAP_RES_MODRDN                 : constant := LDAP_RES_MODDN;
   --  application + constructed
   LDAP_RES_RENAME                 : constant := LDAP_RES_MODDN;
   --  application + constructed
   LDAP_RES_COMPARE                : constant := 16#6f#;
   --  application + constructed
   LDAP_RES_EXTENDED               : constant := 16#78#;
   --  V3: application + constructed
   LDAP_RES_EXTENDED_PARTIAL       : constant := 16#79#;
   --  V3+: application + constructed

   LDAP_RES_ANY                    : constant := -1;
   LDAP_RES_UNSOLICITED            : constant := 0;

   --  sasl methods

   LDAP_SASL_SIMPLE : constant System.Address := System.Null_Address;
   LDAP_SASL_NULL   : constant chars_ptr      := NS ("");


   --  authentication methods available

   LDAP_AUTH_NONE   : constant := 16#00#; -- no authentication
   LDAP_AUTH_SIMPLE : constant := 16#80#; -- context specific + primitive
   LDAP_AUTH_SASL   : constant := 16#A3#; -- context specific + constructed
   LDAP_AUTH_KRBV4  : constant := 16#Ff#; -- means do both of the following
   LDAP_AUTH_KRBV41 : constant := 16#81#; -- context specific + primitive
   LDAP_AUTH_KRBV42 : constant := 16#82#; -- context specific + primitive


   --  filter types

   LDAP_FILTER_AND        : constant := 16#A0#;
   --  context specific + constructed */
   LDAP_FILTER_OR         : constant := 16#A1#;
   --  context specific + constructed */
   LDAP_FILTER_NOT        : constant := 16#A2#;
   --  context specific + constructed */
   LDAP_FILTER_EQUALITY   : constant := 16#A3#;
   --  context specific + constructed */
   LDAP_FILTER_SUBSTRINGS : constant := 16#A4#;
   --  context specific + constructed */
   LDAP_FILTER_GE         : constant := 16#A5#;
   --  context specific + constructed */
   LDAP_FILTER_LE         : constant := 16#A6#;
   --  context specific + constructed */
   LDAP_FILTER_PRESENT    : constant := 16#87#;
   --  context specific + primitive   */
   LDAP_FILTER_APPROX     : constant := 16#A8#;
   --  context specific + constructed */
   LDAP_FILTER_EXT        : constant := 16#A9#;
   --  context specific + constructed

   --  extended filter component types

   LDAP_FILTER_EXT_OID     : constant := 16#81#;     -- context specific
   LDAP_FILTER_EXT_TYPE    : constant := 16#82#;     -- context specific
   LDAP_FILTER_EXT_VALUE   : constant := 16#83#;     -- context specific
   LDAP_FILTER_EXT_DNATTRS : constant := 16#84#;     -- context specific

   --  substring filter component types

   LDAP_SUBSTRING_INITIAL  : constant := 16#80#;     -- context specific
   LDAP_SUBSTRING_ANY      : constant := 16#81#;     -- context specific
   LDAP_SUBSTRING_FINAL    : constant := 16#82#;     -- context specific

   --  search scopes
   LDAP_SCOPE_DEFAULT      : constant := -1;
   LDAP_SCOPE_BASE         : constant := 16#0000#;
   LDAP_SCOPE_ONELEVEL     : constant := 16#0001#;
   LDAP_SCOPE_SUBTREE      : constant := 16#0002#;

   --  possible error codes we can return

   subtype Return_Code is C.int range 16#00# .. 16#61#;

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

   function LDAP_ATTR_ERROR (n : in Return_Code) return Boolean;

   LDAP_NO_SUCH_ATTRIBUTE         : constant := 16#10#;
   LDAP_UNDEFINED_TYPE            : constant := 16#11#;
   LDAP_INAPPROPRIATE_MATCHING    : constant := 16#12#;
   LDAP_CONSTRAINT_VIOLATION      : constant := 16#13#;
   LDAP_TYPE_OR_VALUE_EXISTS      : constant := 16#14#;
   LDAP_INVALID_SYNTAX            : constant := 16#15#;

   subtype NAME_ERROR is Return_Code range 16#20# .. 16#24#;

   function LDAP_NAME_ERROR (n : in Return_Code) return Boolean;

   LDAP_NO_SUCH_OBJECT            : constant := 16#20#;
   LDAP_ALIAS_PROBLEM             : constant := 16#21#;
   LDAP_INVALID_DN_SYNTAX         : constant := 16#22#;
   LDAP_IS_LEAF                   : constant := 16#23#; -- not LDAPv3
   LDAP_ALIAS_DEREF_PROBLEM       : constant := 16#24#;

   subtype SECURITY_ERROR is Return_Code range 16#30# .. 16#32#;

   function LDAP_SECURITY_ERROR (n : in Return_Code) return Boolean;

   LDAP_INAPPROPRIATE_AUTH        : constant := 16#30#;
   LDAP_INVALID_CREDENTIALS       : constant := 16#31#;
   LDAP_INSUFFICIENT_ACCESS       : constant := 16#32#;

   subtype SERVICE_ERROR is Return_Code range 16#33# .. 16#36#;

   function LDAP_SERVICE_ERROR (n : in Return_Code) return Boolean;

   LDAP_BUSY                      : constant := 16#33#;
   LDAP_UNAVAILABLE               : constant := 16#34#;
   LDAP_UNWILLING_TO_PERFORM      : constant := 16#35#;
   LDAP_LOOP_DETECT               : constant := 16#36#;

   subtype UPDATE_ERROR is Return_Code range 16#40# .. 16#47#;

   function LDAP_UPDATE_ERROR (n : in Return_Code) return Boolean;

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

   function LDAP_API_ERROR (n : in Return_Code) return Boolean;

   function LDAP_API_RESULT (n : in Return_Code) return Boolean;

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

   --  not technically reserved for APIs
   LDAP_CONNECT_ERROR             : constant := 16#5b#;
   --  draft-ietf-ldap-c-api-xx
   LDAP_NOT_SUPPORTED             : constant := 16#5c#;
   --  draft-ietf-ldap-c-api-xx
   LDAP_CONTROL_NOT_FOUND         : constant := 16#5d#;
   --  draft-ietf-ldap-c-api-xx
   LDAP_NO_RESULTS_RETURNED       : constant := 16#5e#;
   --  draft-ietf-ldap-c-api-xx
   LDAP_MORE_RESULTS_TO_RETURN    : constant := 16#5f#;
   --  draft-ietf-ldap-c-api-xx
   LDAP_CLIENT_LOOP               : constant := 16#60#;
   --  draft-ietf-ldap-c-api-xx
   LDAP_REFERRAL_LIMIT_EXCEEDED   : constant := 16#61#;
   --  draft-ietf-ldap-c-api-xx

   LDAP_FILT_MAXSIZ : constant := 1024;

   LDAP_DEREF_NEVER        : constant := 16#00#;
   LDAP_DEREF_SEARCHING    : constant := 16#01#;
   LDAP_DEREF_FINDING      : constant := 16#02#;
   LDAP_DEREF_ALWAYS       : constant := 16#03#;

   LDAP_NO_LIMIT           : constant := 0;

   --  how many messages to retrieve results for

   LDAP_MSG_ONE            : constant := 16#00#;
   LDAP_MSG_ALL            : constant := 16#01#;
   LDAP_MSG_RECEIVED       : constant := 16#02#;

   --  types for ldap URL handling

   LDAP_URL_SUCCESS          : constant := 16#00#;
   --  Success
   LDAP_URL_ERR_MEM          : constant := 16#01#;
   --  can't allocate memory space
   LDAP_URL_ERR_PARAM        : constant := 16#02#;
   --  parameter is bad

   LDAP_URL_ERR_BADSCHEME    : constant := 16#03#;
   --  URL doesn't begin with "ldap[si]://"
   LDAP_URL_ERR_BADENCLOSURE : constant := 16#04#;
   --  URL is missing trailing ">"
   LDAP_URL_ERR_BADURL       : constant := 16#05#;
   --  URL is bad
   LDAP_URL_ERR_BADHOST      : constant := 16#06#;
   --  host port is bad
   LDAP_URL_ERR_BADATTRS     : constant := 16#07#;
   --  bad (or missing) attributes
   LDAP_URL_ERR_BADSCOPE     : constant := 16#08#;
   --  scope string is invalid (or missing)
   LDAP_URL_ERR_BADFILTER    : constant := 16#09#;
   --  bad or missing filter
   LDAP_URL_ERR_BADEXTS      : constant := 16#0a#;
   --  bad or missing extensions

   --  API

   type LDAP_Type is private;
   --  An LDAP connection, this is a pointer to the C LDAP structure. Such
   --  object is returned by LDAP_Init.
   Null_LDAP_Type : constant LDAP_Type;

   --  Init LDAP

   function ldap_init
     (host : in chars_ptr;
      port : in C.int)
      return LDAP_Type;

   function ldap_simple_bind
     (ld     : in LDAP_Type;
      who    : in chars_ptr;
      passwd : in chars_ptr)
      return C.int;

   function ldap_simple_bind_s
     (ld     : in LDAP_Type;
      who    : in chars_ptr;
      passwd : in chars_ptr)
      return C.int;

   function ldap_bind_s
     (ld         : in LDAP_Type;
      who        : in chars_ptr;
      passwd     : in chars_ptr;
      authmethod : in C.int)
      return C.int;

   function ldap_unbind_s (ld : in LDAP_Type) return C.int;

   --  Search

   type LDAPMessage is private;
   --  LDAP message, this structure can contain the search result for example.
   Null_LDAPMessage : constant LDAPMessage;

   type BerElement is private;

   function ldap_search_s
     (ld        : in LDAP_Type;
      base      : in chars_ptr;
      scope     : in C.int;
      filter    : in chars_ptr;
      attrs     : in C.Strings.chars_ptr_array;
      attrsonly : in C.int;
      res       : access LDAPMessage)
      return C.int;

   function ldap_count_entries
     (ld    : in LDAP_Type;
      chain : in LDAPMessage)
      return C.int;

   function ldap_first_entry
     (ld    : in LDAP_Type;
      chain : in LDAPMessage)
      return LDAPMessage;

   function ldap_next_entry
     (ld      : in LDAP_Type;
      entries : in LDAPMessage)
      return LDAPMessage;

   function ldap_get_dn
     (ld      : in LDAP_Type;
      entries : in LDAPMessage)
      return chars_ptr;

   function ldap_first_attribute
     (ld      : in LDAP_Type;
      entries : in LDAPMessage;
      ber     : access BerElement)
      return chars_ptr;

   function ldap_next_attribute
     (ld       : in LDAP_Type;
      entries  : in LDAPMessage;
      ber      : in BerElement)
      return chars_ptr;

   function ldap_msgfree
     (lm : in LDAPMessage)
      return C.int;

   function ldap_msgid
     (lm : in LDAPMessage)
      return C.int;

   function ldap_msgtype
     (lm : in LDAPMessage)
      return C.int;

   subtype Attribute_Set is C.Strings.chars_ptr_array (C.size_t);
   type Attribute_Set_Access is access all Attribute_Set;

   function Item
     (Set   : in Attribute_Set_Access;
      Index : in C.int)
      return chars_ptr;
   pragma Inline (Item);
   --  Returns item at positon Index in Set.

   function ldap_get_values
     (ld      : in LDAP_Type;
      entries : in LDAPMessage;
      target  : in chars_ptr)
      return Attribute_Set_Access;

   function ldap_count_values (V : in Attribute_Set_Access) return C.int;

   procedure ldap_value_free (V : in Attribute_Set_Access);

   function ldap_err2string
     (err : in C.int)
      return chars_ptr;

   function ldap_dn2ufn
     (dn : in chars_ptr)
      return chars_ptr;

   function ldap_explode_dn
     (dn      : in chars_ptr;
      notypes : in C.int)
      return Attribute_Set_Access;

   procedure ber_free
     (BER  : in BerElement;
      fbuf : in C.int);

private

   type LDAP_Type   is new System.Address;
   type LDAPMessage is new System.Address;
   type BerElement  is new System.Address;

   Null_LDAP_Type   : constant LDAP_Type
     := LDAP_Type (System.Null_Address);

   Null_LDAPMessage : constant LDAPMessage
     := LDAPMessage (System.Null_Address);

   pragma Import (C, ldap_init);
   pragma Import (C, ldap_simple_bind);
   pragma Import (C, ldap_simple_bind_s);
   pragma Import (C, ldap_bind_s);
   pragma Import (C, ldap_unbind_s);
   pragma Import (C, ldap_search_s);
   pragma Import (C, ldap_count_entries);
   pragma Import (C, ldap_first_entry);
   pragma Import (C, ldap_next_entry);
   pragma Import (C, ldap_get_dn);
   pragma Import (C, ldap_first_attribute);
   pragma Import (C, ldap_next_attribute);
   pragma Import (C, ldap_msgfree);
   pragma Import (C, ldap_msgid);
   pragma Import (C, ldap_msgtype);
   pragma Import (C, ldap_get_values);
   pragma Import (C, ldap_err2string);
   pragma Import (C, ldap_dn2ufn);
   pragma Import (C, ldap_explode_dn);
   pragma Import (C, ldap_value_free);
   pragma Import (C, ldap_count_values);
   pragma Import (C, ber_free);

end AWS.LDAP.Thin;
