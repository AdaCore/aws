------------------------------------------------------------------------------
--                            Secure Sockets Layer                          --
--                         Binding to OpenSSL library                       --
--                                                                          --
--                         Copyright (C) 2000-2001                          --
--                                ACT-Europe                                --
--                                                                          --
--  Authors: Dmitriy Anisimov - Pascal Obry                                 --
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

--  $Id$

with Interfaces.C.Strings;

package SSL.Thin is

   use Interfaces.C;

   package Cstr renames Interfaces.C.Strings;

   subtype Pointer is System.Address;

   subtype SSL_Method is Pointer;
   subtype SSL_Ctx is Pointer;
   subtype SSL_Handle is Pointer;
   subtype RSA is Pointer;

   subtype Error_Code is unsigned_long;

   SSL_Filetype_Pem                    : constant := 1;
   SSL_Ctrl_Need_Tmp_Rsa               : constant := 1;
   SSL_Ctrl_Set_Tmp_Rsa                : constant := 2;
   SSL_Ctrl_Set_Tmp_Dh                 : constant := 3;
   SSL_Ctrl_Set_Tmp_Rsa_Cb             : constant := 4;
   SSL_Ctrl_Set_Tmp_Dh_Cb              : constant := 5;
   SSL_Ctrl_Get_Session_Reused         : constant := 6;
   SSL_Ctrl_Get_Client_Cert_Request    : constant := 7;
   SSL_Ctrl_Get_Num_Renegotiations     : constant := 8;
   SSL_Ctrl_Clear_Num_Renegotiations   : constant := 9;
   SSL_Ctrl_Get_Total_Renegotiations   : constant := 10;
   SSL_Ctrl_Get_Flags                  : constant := 11;
   SSL_Ctrl_Extra_Chain_Cert           : constant := 12;
   SSL_Ctrl_Sess_Number                : constant := 20;
   SSL_Ctrl_Sess_Connect               : constant := 21;
   SSL_Ctrl_Sess_Connect_Good          : constant := 22;
   SSL_Ctrl_Sess_Connect_Renegotiate   : constant := 23;
   SSL_Ctrl_Sess_Accept                : constant := 24;
   SSL_Ctrl_Sess_Accept_Good           : constant := 25;
   SSL_Ctrl_Sess_Accept_Renegotiate    : constant := 26;
   SSL_Ctrl_Sess_Hit                   : constant := 27;
   SSL_Ctrl_Sess_Cb_Hit                : constant := 28;
   SSL_Ctrl_Sess_Misses                : constant := 29;
   SSL_Ctrl_Sess_Timeouts              : constant := 30;
   SSL_Ctrl_Sess_Cache_Full            : constant := 31;
   SSL_Ctrl_Options                    : constant := 16#0020#;
   SSL_Ctrl_Mode                       : constant := 33;
   SSL_Ctrl_Get_Read_Ahead             : constant := 40;
   SSL_Ctrl_Set_Read_Ahead             : constant := 41;
   SSL_Ctrl_Set_Sess_Cache_Size        : constant := 42;
   SSL_Ctrl_Get_Sess_Cache_Size        : constant := 43;
   SSL_Ctrl_Set_Sess_Cache_Mode        : constant := 44;
   SSL_Ctrl_Get_Sess_Cache_Mode        : constant := 45;

   Rsa_3    : constant := 3;
   Rsa_F4   : constant := 16#10001#;

   function SSLv3_Method         return SSL_Method;
   function SSLv3_Server_Method  return SSL_Method;
   function SSLv3_Client_Method  return SSL_Method;
   function SSLv2_Method         return SSL_Method;
   function SSLv2_Server_Method  return SSL_Method;
   function SSLv2_Client_Method  return SSL_Method;
   function SSLv23_Method        return SSL_Method;
   function SSLv23_Server_Method return SSL_Method;
   function SSLv23_Client_Method return SSL_Method;
   function Tlsv1_Method         return SSL_Method;
   function Tlsv1_Server_Method  return SSL_Method;
   function Tlsv1_Client_Method  return SSL_Method;

   function Crypto_Set_Mem_Functions
     (M : System.Address;
      R : System.Address;
      F : System.Address) return int;


   function SSL_Ctx_New (Meth : SSL_Method) return SSL_Ctx;
   procedure SSL_Ctx_Free (P1 : SSL_Ctx);
   procedure SSL_Ctx_Set_Quiet_Shutdown (Ctx : SSL_Ctx;
                                         Mode : int);
   function SSL_Ctx_Ctrl (Ctx : SSL_Ctx;
                          Cmd : int;
                          Larg : int;
                          Parg : Pointer) return int;

   procedure OpenSSL_Add_All_Algorithms;
   procedure SSL_Load_Error_Strings;
   procedure Err_Load_Crypto_Strings;
   procedure Err_Load_SSL_Strings;
   function Err_Get_Error return Error_Code;
   function Err_Error_String (Code : Error_Code;
                              Buffer : Cstr.chars_ptr) return Cstr.chars_ptr;

   function SSL_New (Ctx : SSL_Ctx) return SSL_Handle;
   procedure SSL_Free (SSL : SSL_Handle);
   function SSL_Set_Fd (S  : SSL_Handle;
                        Fd : int)
                       return int;
   procedure SSL_Set_Read_Ahead (S   : SSL_Handle;
                                 Yes : int);
   function SSL_Connect (SSL : SSL_Handle) return int;
   function SSL_Accept (SSL : SSL_Handle) return int;

   function SSL_Renegotiate (SSL : SSL_Handle) return int;
   function SSL_Do_Handshake (SSL : SSL_Handle) return int;
   function SSL_Want (S : SSL_Handle) return int;

   function SSL_Read (SSL : SSL_Handle;
                      Buf : Pointer;
                      Num : int)
                     return int;

   function SSL_Peek (SSL : SSL_Handle;
                      Buf : Pointer;
                      Num : int)
                     return int;

   function SSL_Write (SSL : SSL_Handle;
                       Buf : Pointer;
                       Num : int)
                      return int;

   function SSL_Pending (S : SSL_Handle) return int;


   type Generate_Key_Callback is access
     procedure (I1, I2 : Integer; Param : Pointer);

   pragma Convention (C, Generate_Key_Callback);


   function Rsa_Generate_Key (Bits    : int;
                              E        : unsigned;
                              Callback : Generate_Key_Callback;
                              Cb_Arg   : Pointer)
                             return RSA;

   function SSL_Use_Rsaprivatekey (SSL : SSL_Handle;
                                   Private_Key : RSA) return int;

   function SSL_Shutdown (SSL : SSL_Handle) return int;

   function SSL_Ctx_Use_Privatekey_File (Ctx : SSL_Ctx;
                                         File  : char_array;
                                         C_Type : int) return int;

   function SSL_Ctx_Use_Certificate_File (Ctx : SSL_Ctx;
                                          File  : char_array;
                                          C_Type : int) return int;

   function SSL_Ctx_Check_Private_Key (Ctx : SSL_Ctx) return int;

   procedure Rand_Seed (Buf : Pointer; Num : Integer);

private

   pragma Import (C, Rand_Seed, "RAND_seed");
   pragma Import (C, SSL_Set_Fd, "SSL_set_fd");
   pragma Import (C, SSL_Accept, "SSL_accept");
   pragma Import (C, SSL_Ctx_Use_Certificate_File,
                    "SSL_CTX_use_certificate_file");
   pragma Import (C, SSL_Ctx_Use_Privatekey_File,
                    "SSL_CTX_use_PrivateKey_file");
   pragma Import (C, SSL_Ctx_Check_Private_Key, "SSL_CTX_check_private_key");
   pragma Import (C, SSL_Read, "SSL_read");
   pragma Import (C, SSL_Write, "SSL_write");
   pragma Import (C, SSL_Peek, "SSL_peek");
   pragma Import (C, SSL_Connect, "SSL_connect");
   pragma Import (C, SSL_Ctx_Set_Quiet_Shutdown, "SSL_CTX_set_quiet_shutdown");
   pragma Import (C, SSL_Ctx_Ctrl, "SSL_CTX_ctrl");
   pragma Import (C, SSL_Pending, "SSL_pending");
   pragma Import (C, SSL_Shutdown, "SSL_shutdown");
   pragma Import (C, SSL_Do_Handshake, "SSL_do_handshake");
   pragma Import (C, SSL_Renegotiate, "SSL_renegotiate");
   pragma Import (C, SSL_Want, "SSL_want");
   pragma Import (C, SSL_Set_Read_Ahead, "SSL_set_read_ahead");

   pragma Import (C, Rsa_Generate_Key, "RSA_generate_key");
   pragma Import (C, SSL_Use_Rsaprivatekey, "SSL_use_RSAPrivateKey");
   pragma Import (C, OpenSSL_Add_All_Algorithms, "OpenSSL_add_all_algorithms");
   pragma Import (C, SSL_Load_Error_Strings, "SSL_load_error_strings");
   pragma Import (C, Err_Load_Crypto_Strings, "ERR_load_crypto_strings");
   pragma Import (C, Err_Load_SSL_Strings, "ERR_load_SSL_strings");

   pragma Import (C, Err_Get_Error, "ERR_get_error");
   pragma Import (C, Err_Error_String, "ERR_error_string");
   pragma Import (C, Crypto_Set_Mem_Functions, "CRYPTO_set_mem_functions");

   pragma Import (C, SSL_Ctx_New, "SSL_CTX_new");
   pragma Import (C, SSL_Ctx_Free, "SSL_CTX_free");

   pragma Import (C, SSLv3_Method, "SSLv3_method");
   pragma Import (C, SSLv3_Server_Method, "SSLv3_server_method");
   pragma Import (C, SSLv3_Client_Method, "SSLv3_client_method");
   pragma Import (C, SSLv2_Method, "SSLv2_method");
   pragma Import (C, SSLv2_Server_Method, "SSLv2_server_method");
   pragma Import (C, SSLv2_Client_Method, "SSLv2_client_method");
   pragma Import (C, SSLv23_Method, "SSLv23_method");
   pragma Import (C, SSLv23_Server_Method, "SSLv23_server_method");
   pragma Import (C, SSLv23_Client_Method, "SSLv23_client_method");
   pragma Import (C, Tlsv1_Method, "TLSv1_method");
   pragma Import (C, Tlsv1_Server_Method, "TLSv1_server_method");
   pragma Import (C, Tlsv1_Client_Method, "TLSv1_client_method");

   pragma Import (C, SSL_New, "SSL_new");
   pragma Import (C, SSL_Free, "SSL_free");

end SSL.Thin;
