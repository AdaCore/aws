------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2004                          --
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
--  "cert.pem" (in the working directory) if it exists. If this file does
--  not exists it is required to initialize the SSL layer certificate with
--  AWS.Server.Set_Security.

with Ada.Calendar;
with Ada.Exceptions;
with Ada.Unchecked_Deallocation;

with AWS.Config;
with AWS.Net.Std;
with AWS.Utils;

with Interfaces.C.Strings;
with System.Storage_Elements;
with System;

package body AWS.Net.SSL is

   use Ada;

   pragma Linker_Options ("-laws_nossl");
   --  This is the library used to link without SSL support. The symbols there
   --  will be used only if the application is not linked with the real SSL
   --  libraries.

   use type Interfaces.C.int;
   use type System.Address;

   subtype NSST is Net.Std.Socket_Type;

   Default_Config : constant Config := new TS_SSL;

   procedure Error_If (Error : in Boolean);
   pragma Inline (Error_If);
   --  Raises Socket_Error if Error is true. Attach the SSL error message

   procedure Do_Handshake (Socket : in out Socket_Type; Success : out Boolean);
   --  Perform SSL handshake.

   function Error_Stack return String;
   --  Returns error stack of the last SSL error in multiple lines.

   function Error_Str (Code : in TSSL.Error_Code) return String;
   --  Returns the SSL error message for error Code

   procedure Init_Random;
   --  Initialize the SSL library with a random number

   procedure Initialize_Default_Config;
   --  Initializes default config. It could be called more then once, because
   --  secondary initialization is ignored.

   function Verify_Callback
     (preverify_ok : in Integer;
      ctx          : in System.Address)
      return Integer;
   --  Dummy verify procedure that always return ok. This is needed to be able
   --  to retreive the client's certificate.

   protected Private_Key_Holder is
      procedure Get (Key : out TSSL.RSA);
   private
      Private_Key : TSSL.RSA := TSSL.Null_Pointer;
   end Private_Key_Holder;
   --  The private key to use by all SSL servers

   procedure Secure
     (Source : in     Net.Socket_Type'Class;
      Target :    out Socket_Type;
      Config : in     SSL.Config);
   --  Common code for Secure_Server and Secure_Client routines.

   -------------------
   -- Accept_Socket --
   -------------------

   procedure Accept_Socket
     (Socket     : in     Net.Socket_Type'Class;
      New_Socket : in out Socket_Type)
   is
      Success : Boolean;
   begin
      if New_Socket.Config = null then
         Initialize_Default_Config;
         New_Socket.Config := Default_Config;
      end if;

      SSL_Accept : loop
         Net.Std.Accept_Socket (Socket, NSST (New_Socket));

         New_Socket.Config.Set_FD (New_Socket);

         TSSL.SSL_set_accept_state (New_Socket.SSL);

         Do_Handshake (New_Socket, Success);

         exit SSL_Accept when Success;

         Shutdown (New_Socket);

         --  We cannot reuse allocated SSL handle, Free it before the next use

         TSSL.SSL_free (New_Socket.SSL);
         New_Socket.SSL := TSSL.Null_Pointer;
      end loop SSL_Accept;

   exception
      when others =>
         Free (New_Socket);
         raise;
   end Accept_Socket;

   -------------
   -- Connect --
   -------------

   procedure Connect
     (Socket   : in out Socket_Type;
      Host     : in     String;
      Port     : in     Positive)
   is
      Success : Boolean;
   begin
      Net.Std.Connect (NSST (Socket), Host, Port);

      if Socket.Config = null then
         Initialize_Default_Config;
         Socket.Config := Default_Config;
      end if;

      Socket.Config.Set_FD (Socket);

      TSSL.SSL_set_connect_state (Socket.SSL);

      Do_Handshake (Socket, Success);

      if not Success then
         Net.Std.Shutdown (NSST (Socket));
         Free (Socket);
         Ada.Exceptions.Raise_Exception (Socket_Error'Identity, Error_Stack);
      end if;
   end Connect;

   ------------------
   -- Do_Handshake --
   ------------------

   procedure Do_Handshake (Socket : in out Socket_Type; Success : out Boolean)
   is
      Res : Interfaces.C.int;
   begin
      loop
         Res := TSSL.SSL_do_handshake (Socket.SSL);

         Success := Res = 1;

         exit when Success;

         case TSSL.SSL_get_error (Socket.SSL, Res) is
            when TSSL.SSL_ERROR_WANT_READ  => Wait_For (Input, Socket);
            when TSSL.SSL_ERROR_WANT_WRITE => Wait_For (Output, Socket);
            when others => exit;
         end case;
      end loop;
   end Do_Handshake;

   procedure Do_Handshake (Socket : in out Socket_Type) is
      Success : Boolean;
   begin
      Do_Handshake (Socket, Success);

      if not Success then
         Ada.Exceptions.Raise_Exception (Socket_Error'Identity, Error_Stack);
      end if;
   end Do_Handshake;

   --------------
   -- Error_If --
   --------------

   procedure Error_If (Error : in Boolean) is
   begin
      if Error then
         Ada.Exceptions.Raise_Exception (Socket_Error'Identity, Error_Stack);
      end if;
   end Error_If;

   -----------------
   -- Error_Stack --
   -----------------

   function Error_Stack return String is
      use type TSSL.Error_Code;
      Error_Code : constant TSSL.Error_Code := TSSL.ERR_get_error;
   begin
      if Error_Code = 0 then
         return "";

      else
         declare
            Error_Text : constant String := Error_Str (Error_Code);
            Trim_Start : constant String := "error:";
            First      : Positive := Error_Text'First;
         begin
            if Error_Text'Length > Trim_Start'Length
              and then Error_Text (First .. Trim_Start'Last) = Trim_Start
            then
               First := Error_Text'First + Trim_Start'Length;
            end if;

            return Error_Text (First .. Error_Text'Last)
                   & ASCII.LF & Error_Stack;
         end;
      end if;
   end Error_Stack;

   ---------------
   -- Error_Str --
   ---------------

   function Error_Str (Code : in TSSL.Error_Code) return String is
      use Interfaces;
      use type TSSL.Error_Code;
      Buffer : aliased C.char_array := (0 .. 511 => Interfaces.C.nul);
   begin
      if Code = 0 then
         return "Not an error";
      else
         TSSL.ERR_error_string_n
           (Code,
            C.Strings.To_Chars_Ptr (Buffer'Unchecked_Access),
            Buffer'Length);

         return C.To_Ada (Buffer);
      end if;
   end Error_Str;

   ----------
   -- Free --
   ----------

   procedure Free (Socket : in out Socket_Type) is
   begin
      if Socket.SSL /= TSSL.Null_Pointer then
         TSSL.SSL_free (Socket.SSL);
         Socket.SSL := TSSL.Null_Pointer;
      end if;

      Net.Std.Free (NSST (Socket));
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
     (Config               : in out SSL.Config;
      Certificate_Filename : in     String;
      Security_Mode        : in     Method     := SSLv23;
      Key_Filename         : in     String     := "";
      Exchange_Certificate : in     Boolean    := False) is
   begin
      if Config = null then
         Config := new TS_SSL;
      end if;

      Config.Initialize
        (Certificate_Filename, Security_Mode, Key_Filename,
         Exchange_Certificate);
   end Initialize;

   -------------------------------
   -- Initialize_Default_Config --
   -------------------------------

   procedure Initialize_Default_Config is
      package CNF renames AWS.Config;
      Default : CNF.Object renames CNF.Default_Config;
   begin
      Default_Config.Initialize
        (Certificate_Filename => CNF.Certificate (Default),
         Security_Mode        => Method'Value (CNF.Security_Mode (Default)),
         Key_Filename         => CNF.Key (Default),
         Exchange_Certificate => CNF.Exchange_Certificate (Default));
   end Initialize_Default_Config;

   -------------
   -- Pending --
   -------------

   function Pending (Socket : in Socket_Type) return Stream_Element_Count is
      Res : constant Interfaces.C.int := TSSL.SSL_pending (Socket.SSL);
   begin
      Error_If (Res < 0);
      return Stream_Element_Count (Res);
   end Pending;

   ------------------------
   -- Private_Key_Holder --
   ------------------------

   protected body Private_Key_Holder is

      ---------
      -- Get --
      ---------

      procedure Get (Key : out TSSL.RSA) is
      begin
         if Private_Key = TSSL.Null_Pointer then
            --  Initialize private key

            Private_Key := TSSL.RSA_generate_key
              (Bits     => 512,
               E        => TSSL.RSA_F4,
               Callback => null,
               Cb_Arg   => TSSL.Null_Pointer);

            Error_If (Private_Key = TSSL.Null_Pointer);
         end if;

         Key := Private_Key;
      end Get;

   end Private_Key_Holder;

   -------------
   -- Receive --
   -------------

   procedure Receive
     (Socket : in     Socket_Type;
      Data   :    out Stream_Element_Array;
      Last   :    out Stream_Element_Offset)
   is
      use Interfaces;

      Len    : C.int;
   begin
      loop
         Len := TSSL.SSL_read (Socket.SSL, Data'Address, Data'Length);

         exit when Len > 0;

         declare
            Error_Code : constant C.int
              := TSSL.SSL_get_error (Socket.SSL, Len);
         begin
            case Error_Code is
               when TSSL.SSL_ERROR_WANT_READ  => Wait_For (Input, Socket);
               when TSSL.SSL_ERROR_WANT_WRITE => Wait_For (Output, Socket);
               when others => Error_If (True);
            end case;
         end;
      end loop;

      Last := Data'First + Stream_Element_Offset (Len) - 1;
   end Receive;

   -------------
   -- Release --
   -------------

   procedure Release (Config : in out SSL.Config) is
      procedure Free is new Ada.Unchecked_Deallocation (TS_SSL, SSL.Config);
   begin
      if Config /= null then
         Config.Finalize;
         Free (Config);
      end if;
   end Release;

   ------------
   -- Secure --
   ------------

   procedure Secure
     (Source : in     Net.Socket_Type'Class;
      Target :    out Socket_Type;
      Config : in     SSL.Config) is
   begin
      Std.Socket_Type (Target) := Std.Socket_Type (Source);

      if Config = null then
         Initialize_Default_Config;
         Target.Config := Default_Config;
      else
         Target.Config := Config;
      end if;

      Target.Config.Set_FD (Target);
   end Secure;

   -------------------
   -- Secure_Client --
   -------------------

   function Secure_Client
     (Socket : in Net.Socket_Type'Class;
      Config : in SSL.Config := Null_Config) return Socket_Type
   is
      Result : Socket_Type;
   begin
      Secure (Socket, Result, Config);
      TSSL.SSL_set_connect_state (Result.SSL);
      return Result;
   end Secure_Client;

   -------------------
   -- Secure_Server --
   -------------------

   function Secure_Server
     (Socket : in Net.Socket_Type'Class;
      Config : in SSL.Config := Null_Config) return Socket_Type
   is
      Result : Socket_Type;
   begin
      Secure (Socket, Result, Config);
      TSSL.SSL_set_accept_state (Result.SSL);
      return Result;
   end Secure_Server;

   ----------
   -- Send --
   ----------

   procedure Send
     (Socket : in     Socket_Type;
      Data   : in     Stream_Element_Array;
      Last   :    out Stream_Element_Offset)
   is
      use Interfaces;

      RC : C.int;
   begin
      loop
         RC := TSSL.SSL_write (Socket.SSL, Data'Address, Data'Length);

         if RC > 0 then
            Last  := Data'First - 1 + Stream_Element_Offset (RC);

            return;

         else
            declare
               Error_Code : constant C.int
                 := TSSL.SSL_get_error (Socket.SSL, RC);

               Err_Code : TSSL.Error_Code;

               use type TSSL.Error_Code;
            begin
               case Error_Code is
                  when TSSL.SSL_ERROR_WANT_READ  =>
                     Wait_For (Input, Socket);

                  when TSSL.SSL_ERROR_WANT_WRITE =>
                     Last := Data'First - 1;
                     return;

                  when others =>
                     Err_Code := TSSL.ERR_get_error;

                     if Err_Code = 0 then
                        Ada.Exceptions.Raise_Exception
                          (Socket_Error'Identity,
                           "Error (" & Utils.Image (Integer (Error_Code))
                           & ") on SSL send");
                     else
                        Ada.Exceptions.Raise_Exception
                          (Socket_Error'Identity, Error_Str (Err_Code));
                     end if;
               end case;
            end;
         end if;
      end loop;
   end Send;

   ----------------
   -- Set_Config --
   ----------------

   procedure Set_Config
     (Socket : in out Socket_Type;
      Config : in     SSL.Config) is
   begin
      Socket.Config := Config;
   end Set_Config;

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
   -- TS_SSL --
   ------------

   protected body TS_SSL is

      --------------
      -- Finalize --
      --------------

      procedure Finalize is
      begin
         TSSL.SSL_CTX_free (Context);
         Context := TSSL.Null_Pointer;
      end Finalize;

      ----------------
      -- Initialize --
      ----------------

      procedure Initialize
        (Certificate_Filename : in String;
         Security_Mode        : in Method;
         Key_Filename         : in String;
         Exchange_Certificate : in Boolean)
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
            use Interfaces.C;

            procedure File_Error (Prefix, Name : in String);
            --  Prefix is the type of file key or certificate that was expected
            pragma No_Return (File_Error);

            -------------------
            -- Could_Not_Use --
            -------------------

            procedure File_Error (Prefix, Name : in String) is
            begin
               Ada.Exceptions.Raise_Exception
                 (Socket_Error'Identity,
                  Prefix & " file """ & Name & """ error." & ASCII.LF
                  & Error_Stack);
            end File_Error;

         begin
            if Key_Filename = "" then
               --  Get certificate and private key from the same file.
               --  We could not use certificates chain this way.

               if TSSL.SSL_CTX_use_certificate_file
                    (Ctx    => Context,
                     File   => To_C (Cert_Filename),
                     C_Type => TSSL.SSL_FILETYPE_PEM) /= 1
               then
                  File_Error ("Certificate", Cert_Filename);
               end if;

               if TSSL.SSL_CTX_use_PrivateKey_file
                    (Ctx    => Context,
                     File   => To_C (Cert_Filename),
                     C_Type => TSSL.SSL_FILETYPE_PEM) /= 1
               then
                  File_Error ("Key", Cert_Filename);
               end if;

            else
               --  Get the single certificate or certificate chain from
               --  the file Cert_Filename.

               if TSSL.SSL_CTX_use_certificate_chain_file
                    (Ctx    => Context,
                     File   => To_C (Cert_Filename)) /= 1
               then
                  File_Error ("Certificate", Cert_Filename);
               end if;

               if TSSL.SSL_CTX_use_PrivateKey_file
                    (Ctx    => Context,
                     File   => To_C (Key_Filename),
                     C_Type => TSSL.SSL_FILETYPE_PEM) /= 1
               then
                  File_Error ("Key", Key_Filename);
               end if;
            end if;

            Error_If
              (TSSL.SSL_CTX_check_private_key (Ctx => Context) /= 1);

            if TSSL.SSL_CTX_ctrl
              (Ctx  => Context,
               Cmd  => TSSL.SSL_CTRL_NEED_TMP_RSA,
               Larg => 0,
               Parg => TSSL.Null_Pointer) /= 0
            then
               declare
                  Private_Key : TSSL.RSA;
               begin
                  Private_Key_Holder.Get (Private_Key);

                  Error_If
                    (TSSL.SSL_CTX_ctrl
                       (Ctx  => Context,
                        Cmd  => TSSL.SSL_CTRL_SET_TMP_RSA,
                        Larg => 0,
                        Parg => Private_Key) = -1);
               end;
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
                  Cmd  => TSSL.SSL_CTRL_SET_SESS_CACHE_SIZE,
                  Larg => Interfaces.C.int (Value),
                  Parg => TSSL.Null_Pointer) = -1);
         end Set_Sess_Cache_Size;

      begin
         if Context = TSSL.Null_Pointer then
            --  Initialize context

            Context := TSSL.SSL_CTX_new (Methods (Security_Mode).all);
            Error_If (Context = TSSL.Null_Pointer);

            Error_If
              (TSSL.SSL_CTX_ctrl
                 (Ctx  => Context,
                  Cmd  => TSSL.SSL_CTRL_MODE,
                  Larg => TSSL.SSL_MODE_ENABLE_PARTIAL_WRITE
                          + TSSL.SSL_MODE_ACCEPT_MOVING_WRITE_BUFFER,
                  Parg => TSSL.Null_Pointer) = 0);

            if Exchange_Certificate then
               --  Client is requested to send its certificate once

               TSSL.SSL_CTX_set_verify
                 (Context,
                  TSSL.SSL_VERIFY_PEER + TSSL.SSL_VERIFY_CLIENT_ONCE,
                  Verify_Callback'Address);
            end if;

            Set_Certificate (Certificate_Filename, Key_Filename);

            Set_Quiet_Shutdown;
            Set_Sess_Cache_Size (16);
         end if;
      end Initialize;

      ------------
      -- Set_FD --
      ------------

      procedure Set_FD (Socket : in out Socket_Type) is
      begin
         if Socket.SSL = TSSL.Null_Pointer then
            Socket.SSL := TSSL.SSL_new (Context);
            Error_If (Socket.SSL = TSSL.Null_Pointer);

            TSSL.SSL_set_read_ahead (S => Socket.SSL, Yes => 1);

         else
            Error_If (TSSL.SSL_clear (Socket.SSL) /= 1);
         end if;

         Error_If
           (TSSL.SSL_set_fd
              (Socket.SSL,
               Interfaces.C.int (Get_FD (Socket))) = -1);
      end Set_FD;

   end TS_SSL;

   ---------------------
   -- Verify_Callback --
   ---------------------

   function Verify_Callback
     (preverify_ok : in Integer;
      ctx          : in System.Address)
      return Integer
   is
      pragma Unreferenced (preverify_ok, ctx);
   begin
      return 1;
   end Verify_Callback;

begin
   TSSL.SSL_load_error_strings;
   TSSL.SSL_library_init;
   Init_Random;
end AWS.Net.SSL;
