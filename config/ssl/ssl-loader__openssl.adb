------------------------------------------------------------------------------
--                            Secure Sockets Layer                          --
--                                                                          --
--                        Copyright (C) 2022, AdaCore                       --
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

with GNATCOLL.Plugins;
with GNATCOLL.OS.Constants;
with Interfaces.C.Strings;

package body SSL.Loader is

   use GNATCOLL.Plugins;
   use Interfaces;
   use type System.Address;

   package CS renames C.Strings;

   DLL_Ext : String renames GNATCOLL.OS.Constants.DLL_Ext;

   Crypto_Lib_Name : constant String := "libcrypto" & DLL_Ext;
   SSL_Lib_Name    : constant String := "libssl" & DLL_Ext;

   Error_Flag : Boolean := False with Thread_Local_Storage;

   function Unsupported return System.Address is
     (System.Null_Address) with Convention => C;

   function F_Load (Name : String) return Plugin;
   --  Load library with name or with Name & ".1.1" if absent

   function Error_Code return Unsigned_64 with Convention => C;
   --  Returns X509_V_ERR_INVALID_PURPOSE error code and zero sequentally

   procedure Error_String
     (Code : Unsigned_64; Buffer : CS.chars_ptr; Len : C.size_t)
     with Convention => C;

   function Get_Address (Name : String) return System.Address;

   function SSL_library_init return Integer with Convention => C;

   function SSL_CTX_get_ex_new_index
     (Args                          : Long_Integer;
      Argp                          : System.Address;
      New_Func, Dup_Func, Free_Func : System.Address) return Integer
     with Convention => C;

   ----------------
   -- Error_Code --
   ----------------

   function Error_Code return Unsigned_64 is
   begin
      Error_Flag := not Error_Flag;
      return (if Error_Flag then 26 else 0);
   end Error_Code;

   ------------------
   -- Error_String --
   ------------------

   procedure Error_String
     (Code : Unsigned_64; Buffer : CS.chars_ptr; Len : C.size_t)
   is
      pragma Unreferenced (Code);
      Error_Message : constant String :=
                        Crypto_Lib_Name & " or " & SSL_Lib_Name & " not found";
   begin
      CS.Update
        (Buffer, 0,
         Error_Message (1 .. Integer'Min (Error_Message'Last, Integer (Len))),
         Check => False);
   end Error_String;

   ------------
   -- F_Load --
   ------------

   function F_Load (Name : String) return Plugin is
      Result : constant Plugin := Load (Name);
   begin
      return (if Result = No_Plugin then Load (Name & ".1.1") else Result);
   end F_Load;

   Libcrypto : constant Plugin := F_Load (Crypto_Lib_Name);
   Libssl    : constant Plugin := F_Load (SSL_Lib_Name);

   -----------------
   -- Get_Address --
   -----------------

   function Get_Address (Name : String) return System.Address is
      Result : System.Address := System.Null_Address;
   begin
      if Libcrypto /= No_Plugin then
         Result := Routine_Address (Libcrypto, Name);
      end if;

      if Result = System.Null_Address and then Libssl /= No_Plugin then
         Result := Routine_Address (Libssl, Name);
      end if;

      if Result = System.Null_Address then
         Result := (if Name = "ERR_get_error" then Error_Code'Address
                    elsif Name = "ERR_error_string_n" then Error_String'Address
                    else Unsupported'Address);
      end if;

      return Result;
   end Get_Address;

   CRYPTO_get_ex_new_index : constant System.Address :=
                               Get_Address ("CRYPTO_get_ex_new_index");

   OPENSSL_init_ssl : constant System.Address :=
                        Get_Address ("OPENSSL_init_ssl");

   ------------------------------
   -- SSL_CTX_get_ex_new_index --
   ------------------------------

   function SSL_CTX_get_ex_new_index
     (Args                          : Long_Integer;
      Argp                          : System.Address;
      New_Func, Dup_Func, Free_Func : System.Address) return Integer
   is
      CRYPTO_EX_INDEX_SSL_CTX : constant := 1;

      function CRYPTO_get_ex_new_index
        (Class_Index                   : Integer;
         Args                          : Long_Integer;
         Argp                          : System.Address;
         New_Func, Dup_Func, Free_Func : System.Address) return Integer
        with Import, Convention => C,
             Address => Loader.CRYPTO_get_ex_new_index;

   begin
      return CRYPTO_get_ex_new_index
        (CRYPTO_EX_INDEX_SSL_CTX, Args, Argp, New_Func, Dup_Func, Free_Func);
   end SSL_CTX_get_ex_new_index;

   ----------------------
   -- SSL_library_init --
   ----------------------

   function SSL_library_init return Integer is
      --  OPENSSL_INIT_NO_LOAD_SSL_STRINGS : constant := 16#00100000#;
      OPENSSL_INIT_LOAD_SSL_STRINGS    : constant := 16#00200000#;
      OPENSSL_INIT_LOAD_CRYPTO_STRINGS : constant := 16#00000002#;
      OPENSSL_INIT_SSL_DEFAULT         : constant :=
                                           OPENSSL_INIT_LOAD_SSL_STRINGS +
                                             OPENSSL_INIT_LOAD_CRYPTO_STRINGS;

      function OPENSSL_init_ssl
        (opts     : Interfaces.Unsigned_64;
         Settings : System.Address) return Integer
        with Import, Convention => C, Address => Loader.OPENSSL_init_ssl;

   begin
      return OPENSSL_init_ssl (OPENSSL_INIT_SSL_DEFAULT, System.Null_Address);
   end SSL_library_init;

   ------------
   -- Symbol --
   ------------

   function Symbol (Name : String) return System.Address is
      AWS_Prefix : constant String := "__aws_";
      Result     : System.Address;
   begin
      if Name = "__aws_SSL_library_init" then
         return SSL_library_init'Address;

      elsif Name = "__aws_SSL_CTX_get_ex_new_index" then
         return SSL_CTX_get_ex_new_index'Address;

      elsif Name = "__aws_X509_get_notBefore" then
         return Get_Address ("X509_getm_notBefore");

      elsif Name = "__aws_X509_get_notAfter" then
         return Get_Address ("X509_getm_notAfter");

      elsif Name'Length > AWS_Prefix'Length
        and then Name (Name'First .. Name'First + AWS_Prefix'Length - 1)
                 = AWS_Prefix
      then
         Result := Get_Address
                     (Name (Name'First + AWS_Prefix'Length .. Name'Last));

         if Result /= Unsupported'Address then
            return Result;

         elsif Name = "__aws_SSL_get_peer_certificate" then
            return Get_Address ("SSL_get1_peer_certificate");

         elsif Name = "__aws_EVP_MD_size" then
            return Get_Address ("EVP_MD_get_size");

         elsif Name = "__aws_EVP_PKEY_size" then
            return Get_Address ("EVP_PKEY_get_size");
         end if;
      end if;

      return Get_Address (Name);
   end Symbol;

end SSL.Loader;
