with MD5;
package AWS.Digest is

   subtype Digest_String is MD5.Digest_String;

   function Create_Nonce return String;
   --  Create a Nonce value for the digest authentication
   --  see RFC-2617 3.2.1

   function Check_Nonce (Value : String) return Boolean;
   --  Check Nonce for validity and expiration.

   function Create_Digest
     (Username, Realm, Password : String;
      Nonce : String;
      Method, URI : String) return String;
   --  Creating simple MD5 Digest is for
   --  the parameters.

   function Create_Digest
     (Username, Realm, Password : String;
      Nonce, NC, CNonce, QOP : String;
      Method, URI : String) return String;
   --  Creating more complex MD5 Digest is for
   --  the parameters.

end AWS.Digest;