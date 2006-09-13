
--  This is an LDAP routine not implemented on Windows wldap32.dll

--  aws-ldap-thin.ads

type LDAP_SORT_AD_CMP_PROC is
  access function (left, right : in chars_ptr) return C.int;

function ldap_sort_entries
  (ld    : in     LDAP_Type;
   chain : access LDAPMessage;
   attr  : in     chars_ptr;
   cmp   : in     LDAP_SORT_AD_CMP_PROC)
   return C.int;
--  This routine is not implemented on Windows LDAP wldap32.dll


--  aws-ldap-client.adb

------------------
-- Sort_Entries --
------------------

procedure Sort_Entries
  (Dir   : in     Directory;
   Chain : access LDAP_Message;
   Attr  : in     String;
   Cmp   : in     LDAP_SORT_AD_CMP_PROC)
is
   function C_Cmp (Left, Right : in chars_ptr) return C.int;
   --  C wrapper for Cmp.

   C_Attr : chars_ptr := New_String (Attr);
   Res    : C.int;

   -----------
   -- C_Cmp --
   -----------

   function C_Cmp (Left, Right : in chars_ptr) return C.int is
   begin
      if Cmp (Value (Left), Value (Right)) then
         return 1;
      else
         return 0;
      end if;
   end C_Cmp;

begin
   Res := Thin.ldap_sort_entries
     (Dir, Chain, C_Attr, C_Cmp'Unrestricted_Access);

   Free (C_Attr);
end Sort_Entries;

--  aws-ldap-client.ads

----------
-- Sort --
----------

type LDAP_SORT_AD_CMP_PROC is
  access function (Left, Right : in String) return Boolean;

procedure Sort_Entries
  (Dir   : in     Directory;
   Chain : access LDAP_Message;
   Attr  : in     String;
   Cmp   : in     LDAP_SORT_AD_CMP_PROC);
--  Sort entries in Chain on key Attr according to the Cmp function.

