------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2002                          --
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

--  $Id$

--  Routines here are wrappers around standard sockets and SSL.
--
--  IMPORTANT: The default certificate used for the SSL connection is
--  "cert.pem" (in the working directory) if it exists. If this file does not
--  exists it is required to initialize the SSL layer certificate with
--  AWS.Server.Set_Security.

with Ada.Calendar;
with Ada.Exceptions;
with Interfaces.C;
with System.Storage_Elements;

with AWS.Net.Std;
with SSL.Thin;

package body AWS.Net.SSL is

   pragma Linker_Options ("-lsslaws");
   --  This is the library used to link without SSL support. The symbols there
   --  will be used only if the application is not linked with the real SSL
   --  libraries.

   use type Interfaces.C.int;
   use type System.Address;

   subtype NSST is Net.Std.Socket_Type;

   procedure Error_If (Error  : in Boolean);
   pragma Inline (Error_If);
   --  Raises Socket_Error if Error is true. Attach the SSL error message

   procedure Set_Read_Ahead (Socket : in Socket_Type; Value : in Boolean);
   --  ???

   function Error_Str (Code : in TSSL.Error_Code) return String;
   --  Returns the SSL error message for error Code

   procedure Init_Random;
   --  Initialize the SSL library with a random number

   ------------------------------
   -- Thread safe SSL handling --
   ------------------------------

   protected TS_SSL is

      procedure Set_FD (Socket : in out Socket_Type);
      --  Bind the SSL socket handle with the socket

      procedure Initialize
        (Certificate_Filename : in String;
         Security_Mode        : in Method := SSLv23;
         Key_Filename         : in String := "");

      procedure Finalize;

   private
      Initialized : Boolean      := False;
      Private_Key : TSSL.RSA     := Null_Ptr;
      Context     : TSSL.SSL_CTX := Null_Ptr;
   end TS_SSL;

   -------------------
   -- Accept_Socket --
   -------------------

   procedure Accept_Socket
     (Socket     : in     Socket_Type;
      New_Socket :    out Net.Socket_Type'Class) is
   begin
      loop
         Net.Std.Accept_Socket
           (NSST (Socket), NSST (Socket_Type (New_Socket)));

         TS_SSL.Set_FD (Socket_Type (New_Socket));

         TSSL.SSL_set_accept_state (Socket_Type (New_Socket).SSL);

         exit when TSSL.SSL_accept (Socket_Type (New_Socket).SSL) > 0;

         Shutdown (New_Socket);
      end loop;

      Set_Read_Ahead (Socket_Type (New_Socket), True);
   end Accept_Socket;

   ------------
   -- Assign --
   ------------

   procedure Assign
     (Left  : in out Socket_Type;
      Right : in     Net.Socket_Type'Class) is
   begin
      Net.Std.Assign (NSST (Left), Right);
      Left.SSL := Socket_Type (Right).SSL;
   end Assign;

   -------------
   -- Connect --
   -------------

   procedure Connect
     (Socket   : in Socket_Type;
      Host     : in String;
      Port     : in Positive) is
   begin
      Net.Std.Connect (NSST (Socket), Host, Port);

      TSSL.SSL_set_connect_state (Socket.SSL);

      Error_If (TSSL.SSL_connect (Socket.SSL) = -1);
   end Connect;

   --------------
   -- Error_If --
   --------------

   procedure Error_If (Error : in Boolean) is
      use Ada;
   begin
      if Error then
         Exceptions.Raise_Exception
           (Socket_Error'Identity, Error_Str (TSSL.ERR_get_error));
      end if;
   end Error_If;

   ---------------
   -- Error_Str --
   ---------------

   function Error_Str (Code : in TSSL.Error_Code) return String is
      use Interfaces;
      Buffer : C.char_array := (0 .. 511 => Interfaces.C.nul);
   begin
      TSSL.ERR_error_string_n (Code, Buffer, Buffer'Length);
      return C.To_Ada (Buffer);
   end Error_Str;

   ----------
   -- Free --
   ----------

   procedure Free (Socket : in out Socket_Type) is
   begin
      if Socket.SSL /= Null_Ptr then
         TSSL.SSL_free (Socket.SSL);
         Socket.SSL := Null_Ptr;
      end if;

      Net.Std.Free (NSST (Socket));
      Release_Cache (Socket);
   end Free;

   -----------------
   -- Init_Random --
   -----------------

   procedure Init_Random is
      use Ada.Calendar;
      use System.Storage_Elements;

      Buf : String
        := Duration'Image
        (Clock - Time_Of (Year  => Year_Number'First,
                          Month => Month_Number'First,
                          Day   => Day_Number'First))
        & Integer_Address'Image (To_Integer (Init_Random'Address));
   begin
      TSSL.RAND_seed (Buf'Address, Buf'Length);
   end Init_Random;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Certificate_Filename : in String;
      Security_Mode        : in Method := SSLv23;
      Key_Filename         : in String := "") is
   begin
      TS_SSL.Initialize (Certificate_Filename, Security_Mode, Key_Filename);
   end Initialize;

   -------------
   -- Receive --
   -------------

   function Receive
     (Socket : in Socket_Type;
      Max    : in Stream_Element_Count := 4096)
      return Stream_Element_Array
   is
      Buffer : Stream_Element_Array (0 .. Max - 1);
      Len    : Interfaces.C.int;
   begin
      Len := TSSL.SSL_read (Socket.SSL, Buffer'Address, Buffer'Length);
      Error_If (Len <= 0);

      return Buffer
        (Buffer'First
           .. Buffer'First - 1 + Stream_Element_Count (Len));
   end Receive;

   ----------
   -- Send --
   ----------

   procedure Send
     (Socket : in Socket_Type;
      Data   : in Ada.Streams.Stream_Element_Array) is
   begin
      Error_If (TSSL.SSL_write (Socket.SSL, Data'Address, Data'Length) = -1);
   end Send;

   --------------------
   -- Set_Read_Ahead --
   --------------------

   procedure Set_Read_Ahead (Socket : in Socket_Type; Value : in Boolean)  is
   begin
      TSSL.SSL_set_read_ahead (S => Socket.SSL, Yes => Boolean'Pos (Value));
   end Set_Read_Ahead;

   --------------
   -- Shutdown --
   --------------

   procedure Shutdown (Socket : in Socket_Type) is
   begin
      TSSL.SSL_set_shutdown
        (Socket.SSL, TSSL.SSL_SENT_SHUTDOWN + TSSL.SSL_RECEIVED_SHUTDOWN);
      Net.Std.Shutdown (NSST (Socket));
   end Shutdown;

   ------------
   -- Socket --
   ------------

   function Socket return Socket_Access is
      S    : Socket_Access;
      Sock : Socket_Access;
   begin
      S    := Net.Std.Socket;
      Sock := new Socket_Type;

      Net.Std.Assign (NSST (Sock.all), S.all);

      TS_SSL.Set_FD (Socket_Type (Sock.all));

      return Sock;
   end Socket;

   ------------
   -- TS_SSL --
   ------------

   protected body TS_SSL is

      --------------
      -- Finalize --
      --------------

      procedure Finalize is
      begin
         TSSL.SSL_CTX_free (Context);
         Context := Null_Ptr;
      end Finalize;

      ----------------
      -- Initialize --
      ----------------

      procedure Initialize
        (Certificate_Filename : in String;
         Security_Mode        : in Method := SSLv23;
         Key_Filename         : in String := "")
      is

         type Meth_Func is access function return TSSL.SSL_Method;
         pragma Convention (C, Meth_Func);

         procedure Set_Quiet_Shutdown (Value : in Boolean := True);

         procedure Set_Sess_Cache_Size (Value : in Natural);

         procedure Set_Certificate
           (Cert_Filename : in String;
            Key_Filename  : in String := "");

         Methods : constant array (Method) of Meth_Func
           := (SSLv2          => TSSL.SSLv2_method'Access,
               SSLv2_Server   => TSSL.SSLv2_server_method'Access,
               SSLv2_Client   => TSSL.SSLv2_client_method'Access,
               SSLv23         => TSSL.SSLv23_method'Access,
               SSLv23_Server  => TSSL.SSLv23_server_method'Access,
               SSLv23_Client  => TSSL.SSLv23_client_method'Access,
               TLSv1          => TSSL.TLSv1_method'Access,
               TLSv1_Server   => TSSL.TLSv1_server_method'Access,
               TLSv1_Client   => TSSL.TLSv1_client_method'Access,
               SSLv3          => TSSL.SSLv3_method'Access,
               SSLv3_Server   => TSSL.SSLv3_server_method'Access,
               SSLv3_Client   => TSSL.SSLv3_client_method'Access);

         ---------------------
         -- Set_Certificate --
         ---------------------

         procedure Set_Certificate
           (Cert_Filename : in String;
            Key_Filename  : in String := "")
         is

            function Key_File_Name return String;
            --  Returns the key file (Key_Filename) if it is defined and the
            --  certificate filename (Cert_Filename) otherwise.

            -------------------
            -- Key_File_Name --
            -------------------

            function Key_File_Name return String is
            begin
               if Key_Filename = "" then
                  return Cert_Filename;
               else
                  return Key_Filename;
               end if;
            end Key_File_Name;

            use Interfaces.C;

         begin
            Error_If
              (TSSL.SSL_CTX_use_PrivateKey_file
                 (Ctx    => Context,
                  File   => To_C (Key_File_Name),
                  C_Type => TSSL.SSL_Filetype_Pem) = -1);

            Error_If
              (TSSL.SSL_CTX_use_certificate_file
                 (Ctx    => Context,
                  File   => To_C (Cert_Filename),
                  C_Type => TSSL.SSL_Filetype_Pem) = -1);

            Error_If
              (TSSL.SSL_CTX_check_private_key (Ctx => Context) = -1);

            if TSSL.SSL_CTX_ctrl
              (Ctx  => Context,
               Cmd  => TSSL.SSL_Ctrl_Need_Tmp_RSA,
               Larg => 0,
               Parg => Null_Ptr) /= 0
            then
               Error_If
                 (TSSL.SSL_CTX_ctrl
                    (Ctx  => Context,
                     Cmd  => TSSL.SSL_Ctrl_Set_Tmp_RSA,
                     Larg => 0,
                     Parg => Private_Key) = -1);
            end if;
         end Set_Certificate;

         ------------------------
         -- Set_Quiet_Shutdown --
         ------------------------

         procedure Set_Quiet_Shutdown (Value : in Boolean := True) is
         begin
            TSSL.SSL_CTX_set_quiet_shutdown
              (Ctx  => Context,
               Mode => Boolean'Pos (Value));
         end Set_Quiet_Shutdown;

         -------------------------
         -- Set_Sess_Cache_Size --
         -------------------------

         procedure Set_Sess_Cache_Size (Value : in Natural) is
         begin
            Error_If
              (TSSL.SSL_CTX_ctrl
                 (Ctx  => Context,
                  Cmd  => TSSL.SSL_Ctrl_Set_Sess_Cache_Size,
                  Larg => Interfaces.C.int (Value),
                  Parg => Null_Ptr) = -1);
         end Set_Sess_Cache_Size;

      begin
         if not Initialized then
            if Context /= Null_Ptr then
               Finalize;
            end if;

            --  Initialize context

            Context := TSSL.SSL_CTX_new (Methods (Security_Mode).all);
            Error_If (Context = Null_Ptr);

            --  Initialize private key

            Private_Key :=
              TSSL.RSA_generate_key (Bits     => 512,
                                     E        => TSSL.Rsa_F4,
                                     Callback => null,
                                     Cb_Arg   => Null_Ptr);
            Error_If (Private_Key = Null_Ptr);

            Set_Certificate (Certificate_Filename, Key_Filename);

            Set_Quiet_Shutdown;
            Set_Sess_Cache_Size (16);

            Initialized := True;
         end if;
      end Initialize;

      ------------
      -- Set_FD --
      ------------

      procedure Set_FD (Socket : in out Socket_Type) is
      begin
         if Socket.SSL = Null_Ptr then
            Socket.SSL := TSSL.SSL_new (Context);
            Error_If (Socket.SSL = Null_Ptr);
         end if;

         Error_If
           (TSSL.SSL_set_fd
              (Socket.SSL,
               Interfaces.C.int (Get_FD (Socket))) = -1);
      end Set_FD;

   end TS_SSL;

begin
   TSSL.SSL_load_error_strings;
   TSSL.SSL_library_init;
   Init_Random;
end AWS.Net.SSL;
