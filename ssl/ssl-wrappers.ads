------------------------------------------------------------------------------
--                            Secure Sockets Layer                          --
--                     Dummy Binding for non SSL support                    --
--                                                                          --
--                            Copyright (C) 2002                            --
--                                ACT-Europe                                --
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

package SSL.Wrappers is

   --  The routines below must not raise Program_Error as they are called
   --  during the SSL initialization.

   procedure SSL_library_init;
   pragma Export (C, SSL_library_init, "SSL_library_init");

   procedure SSL_load_error_strings;
   pragma Export (C, SSL_load_error_strings, "SSL_load_error_strings");

   procedure RAND_seed;
   pragma Export (C, RAND_seed, "RAND_seed");

   --  All routines below raise Program_Error

   procedure SSL_set_shutdown;
   pragma Export (C, SSL_set_shutdown, "SSL_set_shutdown");

   procedure SSL_set_read_ahead;
   pragma Export (C, SSL_set_read_ahead, "SSL_set_read_ahead");

   procedure SSL_new;
   pragma Export (C, SSL_new, "SSL_new");

   procedure SSL_write;
   pragma Export (C, SSL_write, "SSL_write");

   procedure SSL_set_fd;
   pragma Export (C, SSL_set_fd, "SSL_set_fd");

   procedure SSL_read;
   pragma Export (C, SSL_read, "SSL_read");

   procedure RSA_generate_key;
   pragma Export (C, RSA_generate_key, "RSA_generate_key");

   procedure SSL_CTX_new;
   pragma Export (C, SSL_CTX_new, "SSL_CTX_new");

   procedure SSL_CTX_set_quiet_shutdown;
   pragma Export (C, SSL_CTX_set_quiet_shutdown, "SSL_CTX_set_quiet_shutdown");

   procedure SSL_CTX_ctrl;
   pragma Export (C, SSL_CTX_ctrl, "SSL_CTX_ctrl");

   procedure SSL_CTX_check_private_key;
   pragma Export (C, SSL_CTX_check_private_key, "SSL_CTX_check_private_key");

   procedure SSL_CTX_use_certificate_file;
   pragma Export
     (C, SSL_CTX_use_certificate_file, "SSL_CTX_use_certificate_file");

   procedure SSL_CTX_use_PrivateKey_file;
   pragma Export
     (C, SSL_CTX_use_PrivateKey_file, "SSL_CTX_use_PrivateKey_file");

   procedure SSLv3_client_method;
   pragma Export (C, SSLv3_client_method, "SSLv3_client_method");

   procedure SSLv3_method;
   pragma Export (C, SSLv3_method, "SSLv3_method");

   procedure SSLv3_server_method;
   pragma Export (C, SSLv3_server_method, "SSLv3_server_method");

   procedure SSL_accept;
   pragma Export (C, SSL_accept, "SSL_accept");

   procedure SSL_set_accept_state;
   pragma Export (C, SSL_set_accept_state, "SSL_set_accept_state");

   procedure SSL_connect;
   pragma Export (C, SSL_connect, "SSL_connect");

   procedure SSL_set_connect_state;
   pragma Export (C, SSL_set_connect_state, "SSL_set_connect_state");

   procedure TLSv1_client_method;
   pragma Export (C, TLSv1_client_method, "TLSv1_client_method");

   procedure TLSv1_server_method;
   pragma Export (C, TLSv1_server_method, "TLSv1_server_method");

   procedure TLSv1_method;
   pragma Export (C, TLSv1_method, "TLSv1_method");

   procedure SSLv23_client_method;
   pragma Export (C, SSLv23_client_method, "SSLv23_client_method");

   procedure SSLv23_server_method;
   pragma Export (C, SSLv23_server_method, "SSLv23_server_method");

   procedure SSLv23_method;
   pragma Export (C, SSLv23_method, "SSLv23_method");

   procedure SSLv2_client_method;
   pragma Export (C, SSLv2_client_method, "SSLv2_client_method");

   procedure SSLv2_server_method;
   pragma Export (C, SSLv2_server_method, "SSLv2_server_method");

   procedure SSLv2_method;
   pragma Export (C, SSLv2_method, "SSLv2_method");

   procedure SSL_free;
   pragma Export (C, SSL_free, "SSL_free");

   procedure SSL_CTX_free;
   pragma Export (C, SSL_CTX_free, "SSL_CTX_free");

   procedure ERR_error_string_n;
   pragma Export (C, ERR_error_string_n, "ERR_error_string_n");

   procedure ERR_get_error;
   pragma Export (C, ERR_get_error, "ERR_get_error");

end SSL.Wrappers;
