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

with Ada.Exceptions;

package body SSL.Wrappers is

   procedure Raise_Error;
   --  Raises program error with a clear message that no-SSL support has been
   --  configured.

   ------------------------
   -- ERR_error_string_n --
   ------------------------

   procedure ERR_error_string_n is
   begin
      Raise_Error;
   end ERR_error_string_n;

   -------------------
   -- ERR_get_error --
   -------------------

   procedure ERR_get_error is
   begin
      Raise_Error;
   end ERR_get_error;

   -----------------
   -- Raise_Error --
   -----------------

   procedure Raise_Error is
   begin
      Ada.Exceptions.Raise_Exception
        (Program_Error'Identity,
         "AWS has been configured without SSL support.");
   end Raise_Error;

   ---------------
   -- RAND_seed --
   ---------------

   procedure RAND_seed is
   begin
      null;
   end RAND_seed;

   ----------------------
   -- RSA_generate_key --
   ----------------------

   procedure RSA_generate_key is
   begin
      Raise_Error;
   end RSA_generate_key;

   ----------------
   -- SSL_accept --
   ----------------

   procedure SSL_accept is
   begin
      Raise_Error;
   end SSL_accept;

   -----------------
   -- SSL_connect --
   -----------------

   procedure SSL_connect is
   begin
      Raise_Error;
   end SSL_connect;

   -------------------------------
   -- SSL_CTX_check_private_key --
   -------------------------------

   procedure SSL_CTX_check_private_key is
   begin
      Raise_Error;
   end SSL_CTX_check_private_key;

   ------------------
   -- SSL_CTX_ctrl --
   ------------------

   procedure SSL_CTX_ctrl is
   begin
      Raise_Error;
   end SSL_CTX_ctrl;

   ------------------
   -- SSL_CTX_free --
   ------------------

   procedure SSL_CTX_free is
   begin
      Raise_Error;
   end SSL_CTX_free;

   -----------------
   -- SSL_CTX_new --
   -----------------

   procedure SSL_CTX_new is
   begin
      Raise_Error;
   end SSL_CTX_new;

   --------------------------------
   -- SSL_CTX_set_quiet_shutdown --
   --------------------------------

   procedure SSL_CTX_set_quiet_shutdown is
   begin
      Raise_Error;
   end SSL_CTX_set_quiet_shutdown;

   ----------------------------------
   -- SSL_CTX_use_certificate_file --
   ----------------------------------

   procedure SSL_CTX_use_certificate_file is
   begin
      Raise_Error;
   end SSL_CTX_use_certificate_file;

   ---------------------------------
   -- SSL_CTX_use_PrivateKey_file --
   ---------------------------------

   procedure SSL_CTX_use_PrivateKey_file is
   begin
      Raise_Error;
   end SSL_CTX_use_PrivateKey_file;

   --------------
   -- SSL_free --
   --------------

   procedure SSL_free is
   begin
      Raise_Error;
   end SSL_free;

   ----------------------
   -- SSL_Library_Init --
   ----------------------

   procedure SSL_library_init is
   begin
      null;
   end SSL_library_init;

   ----------------------------
   -- SSL_load_error_strings --
   ----------------------------

   procedure SSL_load_error_strings is
   begin
      null;
   end SSL_load_error_strings;

   -------------
   -- SSL_new --
   -------------

   procedure SSL_new is
   begin
      Raise_Error;
   end SSL_new;

   --------------
   -- SSL_read --
   --------------

   procedure SSL_read is
   begin
      Raise_Error;
   end SSL_read;

   --------------------------
   -- SSL_set_accept_state --
   --------------------------

   procedure SSL_set_accept_state is
   begin
      Raise_Error;
   end SSL_set_accept_state;

   ---------------------------
   -- SSL_set_connect_state --
   ---------------------------

   procedure SSL_set_connect_state is
   begin
      Raise_Error;
   end SSL_set_connect_state;

   ----------------
   -- SSL_set_fd --
   ----------------

   procedure SSL_set_fd is
   begin
      Raise_Error;
   end SSL_set_fd;

   ------------------------
   -- SSL_set_read_ahead --
   ------------------------

   procedure SSL_set_read_ahead is
   begin
      Raise_Error;
   end SSL_set_read_ahead;

   ----------------------
   -- SSL_Set_Shutdown --
   ----------------------

   procedure SSL_set_shutdown is
   begin
      Raise_Error;
   end SSL_set_shutdown;

   ---------------
   -- SSL_write --
   ---------------

   procedure SSL_write is
   begin
      Raise_Error;
   end SSL_write;

   --------------------------
   -- SSLv23_client_method --
   --------------------------

   procedure SSLv23_client_method is
   begin
      Raise_Error;
   end SSLv23_client_method;

   -------------------
   -- SSLv23_method --
   -------------------

   procedure SSLv23_method is
   begin
      Raise_Error;
   end SSLv23_method;

   --------------------------
   -- SSLv23_server_method --
   --------------------------

   procedure SSLv23_server_method is
   begin
      Raise_Error;
   end SSLv23_server_method;

   -------------------------
   -- SSLv2_Client_Method --
   -------------------------

   procedure SSLv2_client_method is
   begin
      Raise_Error;
   end SSLv2_client_method;

   ------------------
   -- SSLv2_method --
   ------------------

   procedure SSLv2_method is
   begin
      Raise_Error;
   end SSLv2_method;

   -------------------------
   -- SSLv2_server_method --
   -------------------------

   procedure SSLv2_server_method is
   begin
      Raise_Error;
   end SSLv2_server_method;

   -------------------------
   -- SSLv3_client_method --
   -------------------------

   procedure SSLv3_client_method is
   begin
      Raise_Error;
   end SSLv3_client_method;

   ------------------
   -- SSLv3_method --
   ------------------

   procedure SSLv3_method is
   begin
      Raise_Error;
   end SSLv3_method;

   -------------------------
   -- SSLv3_server_method --
   -------------------------

   procedure SSLv3_server_method is
   begin
      Raise_Error;
   end SSLv3_server_method;

   -------------------------
   -- TLSv1_client_method --
   -------------------------

   procedure TLSv1_client_method is
   begin
      Raise_Error;
   end TLSv1_client_method;

   ------------------
   -- TLSv1_method --
   ------------------

   procedure TLSv1_method is
   begin
      Raise_Error;
   end TLSv1_method;

   -------------------------
   -- TLSv1_server_method --
   -------------------------

   procedure TLSv1_server_method is
   begin
      Raise_Error;
   end TLSv1_server_method;

end SSL.Wrappers;
