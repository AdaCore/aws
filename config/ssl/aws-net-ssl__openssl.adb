------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2014, AdaCore                     --
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

--  Routines here are wrappers around standard sockets and SSL.
--
--  IMPORTANT: The default certificate used for the SSL connection is
--  "cert.pem" (in the working directory) if it exists. If this file does
--  not exists it is required to initialize the SSL layer certificate with
--  AWS.Server.Set_Security.

with Ada.Calendar;
with Ada.Command_Line;
with Ada.Directories;
with Ada.Task_Attributes;
with Ada.Task_Identification;
with Ada.Task_Termination;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with Interfaces.C.Strings;

with System.Memory;
with System.Storage_Elements;

with AWS.Config;
with AWS.Net.Log;
with AWS.Net.SSL.Certificate.Impl;
with AWS.Net.SSL.RSA_DH_Generators;
with AWS.OS_Lib;
with AWS.Translator;
with AWS.Utils;

package body AWS.Net.SSL is

   use Interfaces;
   use type C.int;
   use type System.Address;

   subtype NSST is Net.Std.Socket_Type;

   package Locking is

      procedure Initialize;
      --  Initialize OpenSSL locking callbacks, this makes OpenSSL
      --  implementation thread safe.

   end Locking;

   function Lib_Realloc
     (Ptr  : System.Address;
      Size : System.Memory.size_t) return System.Address
     with Convention => C;
   --  C library could use null pointer as input parameter for realloc, but
   --  gnatmem does not care about it and logging Free of the null pointer.

   protected type TS_SSL is

      procedure Set_IO (Socket : in out Socket_Type);
      --  Bind the SSL handle with the BIO pair

      procedure Initialize
        (Certificate_Filename : String;
         Security_Mode        : Method;
         Priorities           : String;
         Ticket_Support       : Boolean;
         Key_Filename         : String;
         Exchange_Certificate : Boolean;
         Certificate_Required : Boolean;
         Trusted_CA_Filename  : String;
         CRL_Filename         : String;
         Session_Cache_Size   : Positive);

      procedure Finalize;

      procedure Clear_Session_Cache;

      procedure Set_Session_Cache_Size (Size : Natural);

      function Session_Cache_Number return Natural;

      procedure Set_Verify_Callback (Callback : System.Address);

      procedure Check_CRL;
      --  Check Certificate Revocation List, if this file has changed reload it

   private
      Context        : TSSL.SSL_CTX := TSSL.Null_CTX;
      CRL_File       : C.Strings.chars_ptr := C.Strings.Null_Ptr;
      CRL_Time_Stamp : Calendar.Time := Utils.AWS_Epoch;
   end TS_SSL;

   type Memory_Access is access all
     Stream_Element_Array (1 .. Stream_Element_Offset'Last);

   Default_Config : constant Config := new TS_SSL;

   Data_Index     : C.int;
   --  Application specific data's index

   Max_Overhead : Stream_Element_Count := 78 with Atomic;

   Debug_Level  : Natural := 0 with Atomic;

   DH_Params  : array (0 .. 1) of aliased TSSL.DH :=
                  (others => TSSL.Null_Pointer) with Atomic_Components;
   RSA_Params : array (0 .. 1) of aliased TSSL.RSA :=
                  (others => TSSL.Null_Pointer) with Atomic_Components;
   --  0 element for current use, 1 element for remain usage after creation new
   --  0 element.

   DH_Length  : constant C.int := 2048;
   RSA_Length : constant C.int := 2048;

   function DH_Generate_cb
     (a, b : C.int; cb : access TSSL.BN_GENCB) return C.int
     with Convention => C;

   DH_Generate_Callback : aliased TSSL.BN_GENCB :=
     (cb => DH_Generate_cb'Access, others => <>);

   procedure Socket_Read (Socket : Socket_Type);
   --  Read encripted data from socket if necessary

   procedure Socket_Write (Socket : Socket_Type; Gone : C.int := 0);
   --  Write encripted data to socket if availabe

   procedure Error_If (Error : Boolean) with Inline;
   --  Raises Socket_Error if Error is true. Attach the SSL error message

   procedure Error_If (Socket : Socket_Type; Error : Boolean) with Inline;
   --  Raises and log Socket_Error if Error is true.
   --  Attach the SSL error message.

   procedure File_Error (Prefix, Name : String) with No_Return;
   --  Prefix is the type of file key or certificate that was expected

   procedure Do_Handshake (Socket : in out Socket_Type; Success : out Boolean);
   --  Perform SSL handshake

   function Error_Stack return String;
   --  Returns error stack of the last SSL error in multiple lines

   function Error_Str (Code : TSSL.Error_Code) return String;
   --  Returns the SSL error message for error Code

   procedure Init_Random;
   --  Initialize the SSL library with a random number

   procedure Initialize_Default_Config;
   --  Initializes default config. It could be called more then once, because
   --  secondary initialization is ignored.

   function Verify_Callback
     (preverify_ok : C.int; ctx : TSSL.X509_STORE_CTX) return C.int
     with Convention => C;
   --  This routine is needed to be able to retreive the client's certificate
   --  and validate it thought the user's verification routine if provided.

   function Tmp_RSA_Callback
     (SSL : SSL_Handle; Is_Export : C.int; Keylength : C.int) return TSSL.RSA
     with Convention => C;

   function Tmp_DH_Callback
     (SSL : SSL_Handle; Is_Export : C.int; Keylength : C.int) return TSSL.DH
     with Convention => C;

   procedure Secure
     (Source : Net.Socket_Type'Class;
      Target : out Socket_Type;
      Config : SSL.Config);
   --  Common code for Secure_Server and Secure_Client routines

   procedure Set_Accept_State (Socket : Socket_Type);
   --  Server session initialization

   -------------------------
   -- Abort_DH_Generation --
   -------------------------

   procedure Abort_DH_Generation is
   begin
      Abort_DH_Flag := True;
   end Abort_DH_Generation;

   -------------------
   -- Accept_Socket --
   -------------------

   overriding procedure Accept_Socket
     (Socket : Net.Socket_Type'Class; New_Socket : in out Socket_Type)
   is
      Success : Boolean;
   begin
      if New_Socket.Config = null then
         Initialize_Default_Config;
         New_Socket.Config := Default_Config;
      end if;

      SSL_Accept : loop
         Net.Std.Accept_Socket (Socket, NSST (New_Socket));

         New_Socket.Config.Set_IO (New_Socket);

         Set_Accept_State (New_Socket);

         Do_Handshake (New_Socket, Success);

         exit SSL_Accept when Success;

         Shutdown (New_Socket);
      end loop SSL_Accept;
   end Accept_Socket;

   ------------------------
   -- Cipher_Description --
   ------------------------

   overriding function Cipher_Description (Socket : Socket_Type) return String
   is
      Buffer : aliased C.char_array := (1 .. 256 => <>);
      Result : constant String :=
                 C.Strings.Value
                   (TSSL.SSL_CIPHER_description
                      (TSSL.SSL_get_current_cipher (Socket.SSL).all,
                       Buffer'Unchecked_Access, Buffer'Length));
   begin
      if Result'Length > 0 and then Result (Result'Last) = ASCII.LF then
         return Result (Result'First .. Result'Last - 1);
      end if;

      return Result;
   end Cipher_Description;

   -------------
   -- Ciphers --
   -------------

   procedure Ciphers (Cipher : access procedure (Name : String)) is
      use type C.Strings.chars_ptr;
      Name : C.Strings.chars_ptr;
      Ctx  : constant TSSL.SSL_CTX := TSSL.SSL_CTX_new (TSSL.SSLv23_method);
      SSL  : SSL_Handle;
   begin
      Error_If (Ctx = TSSL.Null_Pointer);

      SSL := TSSL.SSL_new (Ctx);
      Error_If (SSL = TSSL.Null_Pointer);

      for J in 0 .. C.int'Last loop
         Name := TSSL.SSL_get_cipher_list (SSL, J);
         exit when Name = C.Strings.Null_Ptr;
         Cipher (C.Strings.Value (Name));
      end loop;

      TSSL.SSL_free (SSL);
      TSSL.SSL_CTX_free (Ctx);
   end Ciphers;

   -------------------------
   -- Clear_Session_Cache --
   -------------------------

   procedure Clear_Session_Cache (Config : SSL.Config := Null_Config) is
   begin
      if Config = Null_Config then
         Default_Config.Clear_Session_Cache;
      else
         Config.Clear_Session_Cache;
      end if;
   end Clear_Session_Cache;

   -------------
   -- Connect --
   -------------

   overriding procedure Connect
     (Socket : in out Socket_Type;
      Host   : String;
      Port   : Positive;
      Wait   : Boolean     := True;
      Family : Family_Type := Family_Unspec)
   is
      Success : Boolean;
   begin
      Net.Std.Connect (NSST (Socket), Host, Port, Wait, Family);

      if Socket.Config = null then
         Initialize_Default_Config;
         Socket.Config := Default_Config;
      end if;

      Socket.Config.Set_IO (Socket);

      if Socket.Sessn /= null then
         Set_Session_Data (Socket, Socket.Sessn);
         Socket.Sessn := null;
      end if;

      TSSL.SSL_set_connect_state (Socket.SSL);

      if Wait then
         --  Do handshake only in case of wait connection completion

         Do_Handshake (Socket, Success);

         if not Success then
            declare
               Error_Text : constant String := Error_Stack;
            begin
               Net.Log.Error (Socket, Error_Text);

               Net.Std.Shutdown (NSST (Socket));

               raise Socket_Error with Error_Text;
            end;
         end if;
      end if;
   end Connect;

   --------------------
   -- DH_Generate_cb --
   --------------------

   function DH_Generate_cb
     (a, b : C.int; cb : access TSSL.BN_GENCB) return C.int
   is
      pragma Unreferenced (a, b, cb);
   begin
      return Boolean'Pos (not Abort_DH_Flag);
   end DH_Generate_cb;

   ------------------
   -- Do_Handshake --
   ------------------

   procedure Do_Handshake
     (Socket : in out Socket_Type; Success : out Boolean)
   is
      use TSSL;
      Res : C.int;
   begin
      loop
         Res := TSSL.SSL_do_handshake (Socket.SSL);

         Success := Res = 1;

         exit when Success;

         case TSSL.SSL_get_error (Socket.SSL, Res) is
            when TSSL.SSL_ERROR_WANT_READ  => Socket_Read (Socket);
            when TSSL.SSL_ERROR_WANT_WRITE => Socket_Write (Socket);
            when others => exit;
         end case;
      end loop;

      Socket_Write (Socket);
   end Do_Handshake;

   procedure Do_Handshake (Socket : in out Socket_Type) is
      Success : Boolean;
   begin
      Do_Handshake (Socket, Success);

      if not Success then
         Raise_Socket_Error (Socket, Error_Stack);
      end if;
   end Do_Handshake;

   --------------
   -- Error_If --
   --------------

   procedure Error_If (Error : Boolean) is
   begin
      if Error then
         Raise_Socket_Error
           (Socket_Type'(Std.Socket_Type with others => <>), Error_Stack);
      end if;
   end Error_If;

   procedure Error_If (Socket : Socket_Type; Error : Boolean) is
   begin
      if Error then
         Raise_Socket_Error (Socket, Error_Stack);
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
            Error_Rest : constant String := Error_Stack;
         begin
            if Utils.Match (Error_Text, Trim_Start) then
               First := Error_Text'First + Trim_Start'Length;
            end if;

            return Error_Text (First .. Error_Text'Last)
              & (if Error_Rest = "" then "" else ASCII.LF & Error_Rest);
         end;
      end if;
   end Error_Stack;

   ---------------
   -- Error_Str --
   ---------------

   function Error_Str (Code : TSSL.Error_Code) return String is
      use type TSSL.Error_Code;
      Buffer : aliased C.char_array := (0 .. 511 => C.nul);
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

   ----------------
   -- File_Error --
   ----------------

   procedure File_Error (Prefix, Name : String) is
   begin
      raise Socket_Error with
         Prefix & " file """ & Name & """ error." & ASCII.LF & Error_Stack;
   end File_Error;

   ----------
   -- Free --
   ----------

   overriding procedure Free (Socket : in out Socket_Type) is
   begin
      if Socket.SSL /= TSSL.Null_Pointer then
         TSSL.SSL_free (Socket.SSL);
         TSSL.BIO_free (Socket.IO);
         Socket.SSL := TSSL.Null_Pointer;
      end if;

      NSST (Socket).Free;
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Session : in out Session_Type) is
   begin
      if Session /= null then
         TSSL.SSL_SESSION_free (TSSL.SSL_Session (Session));
         Session := null;
      end if;
   end Free;

   procedure Free (Key : in out Private_Key) is
   begin
      if Key /= Private_Key (TSSL.Null_Pointer) then
         TSSL.RSA_free (TSSL.Private_Key (Key));
         Key := Private_Key (TSSL.Null_Pointer);
      end if;
   end Free;

   -----------------
   -- Generate_DH --
   -----------------

   procedure Generate_DH is
      use type TSSL.BIO_Access;
      use type TSSL.DH;
      OK   : Boolean;
      DH   : aliased TSSL.DH;
      Bits : constant C.int := DH_Length;

      function Loaded return Boolean;

      procedure Save;

      ------------
      -- Loaded --
      ------------

      function Loaded return Boolean is
         Filename : constant String :=
                      RSA_DH_Generators.Parameters_Filename
                        ("dh-" & Utils.Image (Integer (Bits)), Exist => True);
         C_Name : aliased C.char_array := C.To_C (Filename);
         IO : TSSL.BIO_Access;
      begin
         if Filename = "" then
            return False;
         end if;

         IO := TSSL.BIO_new (TSSL.BIO_s_file);

         Error_If (IO = null);
         Error_If
           (TSSL.BIO_read_filename
              (IO, C.Strings.To_Chars_Ptr (C_Name'Unchecked_Access)) = 0);

         Error_If
           (TSSL.PEM_read_bio_DHparams (IO, DH'Access, null, TSSL.Null_Pointer)
            /= DH);

         DH_Time (DH_Time_Idx + 1) :=
           Ada.Directories.Modification_Time (Filename);
         DH_Time_Idx := DH_Time_Idx + 1;

         TSSL.BIO_free (IO);

         return True;
      end Loaded;

      ----------
      -- Save --
      ----------

      procedure Save is
         Filename : constant String :=
                      RSA_DH_Generators.Parameters_Filename
                        ("dh-" & Utils.Image (Integer (Bits)), Exist => False);
         C_Name : aliased C.char_array := C.To_C (Filename);
         BIO : TSSL.BIO_Access;
      begin
         if Filename = "" then
            return;
         end if;

         BIO := TSSL.BIO_new (TSSL.BIO_s_file);

         Error_If (BIO = null);
         Error_If
           (TSSL.BIO_write_filename
              (BIO, C.Strings.To_Chars_Ptr (C_Name'Unchecked_Access)) = 0);

         Error_If (TSSL.PEM_write_bio_DHparams (BIO, DH) = 0);

         TSSL.BIO_free (BIO);
      end Save;

   begin
      DH_Lock.Try_Lock (OK);

      if not OK then
         return;
      end if;

      DH := TSSL.DH_new;

      Error_If (DH = TSSL.Null_Pointer);

      if DH_Params (0) /= TSSL.Null_Pointer or else not Loaded then
         if TSSL.DH_generate_parameters_ex
              (params    => DH,
               prime_len => DH_Length,
               generator => (if DH_Time_Idx = 0 then 2 else 5),
               cb        => DH_Generate_Callback'Access) = 0
         then
            Error_If (not Abort_DH_Flag);
         else
            DH_Time (DH_Time_Idx + 1) := Ada.Calendar.Clock;
            DH_Time_Idx := DH_Time_Idx + 1;

            Save;
         end if;
      end if;

      if DH_Params (1) /= TSSL.Null_Pointer then
         TSSL.DH_free (DH_Params (1));
      end if;

      DH_Params (1) := DH_Params (0);
      DH_Params (0) := DH;

      DH_Lock.Unlock;
   end Generate_DH;

   ------------------
   -- Generate_RSA --
   ------------------

   procedure Generate_RSA is
      use type TSSL.RSA;
      OK  : Boolean;
      BN  : TSSL.BIGNUM;
      RSA : TSSL.RSA;
   begin
      RSA_Lock.Try_Lock (OK);

      if not OK then
         return;
      end if;

      BN := TSSL.BN_new;

      Error_If (TSSL."=" (BN, TSSL.BIGNUM (TSSL.Null_Pointer)));
      Error_If (TSSL.BN_set_word (BN, TSSL.RSA_F4) = 0);

      RSA := TSSL.RSA_new;

      Error_If (RSA = TSSL.Null_Pointer);
      Error_If
        (TSSL.RSA_generate_key_ex
           (RSA, RSA_Length, BN, TSSL.Null_Pointer) = 0);

      TSSL.BN_free (BN);

      if RSA_Params (1) /= TSSL.Null_Pointer then
         TSSL.RSA_free (RSA_Params (1));
      end if;

      RSA_Params (1) := RSA_Params (0);
      RSA_Params (0) := RSA;

      RSA_Time (RSA_Time_Idx + 1) := Ada.Calendar.Clock;
      RSA_Time_Idx := RSA_Time_Idx + 1;

      RSA_Lock.Unlock;
   end Generate_RSA;

   -----------------
   -- Init_Random --
   -----------------

   procedure Init_Random is
      use Ada.Calendar;
      use System.Storage_Elements;

      Buf : String :=
              Duration'Image
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
      Certificate_Filename : String;
      Security_Mode        : Method     := SSLv23;
      Priorities           : String     := "";
      Ticket_Support       : Boolean    := False;
      Key_Filename         : String     := "";
      Exchange_Certificate : Boolean    := False;
      Certificate_Required : Boolean    := False;
      Trusted_CA_Filename  : String     := "";
      CRL_Filename         : String     := "";
      Session_Cache_Size   : Positive   := 16#4000#) is
   begin
      if Config = null then
         Config := new TS_SSL;
      end if;

      Config.Initialize
        (Certificate_Filename, Security_Mode, Priorities, Ticket_Support,
         Key_Filename, Exchange_Certificate, Certificate_Required,
         Trusted_CA_Filename, CRL_Filename, Session_Cache_Size);
   end Initialize;

   -------------------------------
   -- Initialize_Default_Config --
   -------------------------------

   procedure Initialize_Default_Config
     (Certificate_Filename : String;
      Security_Mode        : Method   := SSLv23;
      Priorities           : String   := "";
      Ticket_Support       : Boolean  := False;
      Key_Filename         : String   := "";
      Exchange_Certificate : Boolean  := False;
      Certificate_Required : Boolean  := False;
      Trusted_CA_Filename  : String   := "";
      CRL_Filename         : String   := "";
      Session_Cache_Size   : Positive := 16#4000#) is
   begin
      Default_Config.Initialize
        (Certificate_Filename, Security_Mode, Priorities, Ticket_Support,
         Key_Filename, Exchange_Certificate, Certificate_Required,
         Trusted_CA_Filename, CRL_Filename, Session_Cache_Size);
   end Initialize_Default_Config;

   procedure Initialize_Default_Config is
      package CNF renames AWS.Config;
      Default : CNF.Object renames CNF.Default_Config;
   begin
      Default_Config.Initialize
        (Certificate_Filename => CNF.Certificate (Default),
         Security_Mode        => Method'Value (CNF.Security_Mode (Default)),
         Priorities           => CNF.Cipher_Priorities (Default),
         Ticket_Support       => CNF.TLS_Ticket_Support (Default),
         Key_Filename         => CNF.Key (Default),
         Exchange_Certificate => CNF.Exchange_Certificate (Default),
         Certificate_Required => CNF.Certificate_Required (Default),
         Trusted_CA_Filename  => CNF.Trusted_CA (Default),
         CRL_Filename         => CNF.CRL_File (Default),
         Session_Cache_Size   => 16#4000#);
   end Initialize_Default_Config;

   -----------------
   -- Lib_Realloc --
   -----------------

   function Lib_Realloc
     (Ptr  : System.Address;
      Size : System.Memory.size_t) return System.Address is
   begin
      if Ptr = System.Null_Address then
         return System.Memory.Alloc (Size);
      else
         return System.Memory.Realloc (Ptr, Size);
      end if;
   end Lib_Realloc;

   ----------
   -- Load --
   ----------

   function Load (Filename : String) return Private_Key is
      use type TSSL.Private_Key;
      Key  : aliased TSSL.Private_Key := TSSL.RSA_new;
      IO   : constant TSSL.BIO_Access := TSSL.BIO_new (TSSL.BIO_s_file);
      Name : aliased C.char_array := C.To_C (Filename);
   begin
      if TSSL.BIO_read_filename
           (IO, C.Strings.To_Chars_Ptr (Name'Unchecked_Access)) = 0
        or else TSSL.PEM_read_bio_RSAPrivateKey
                  (IO, Key'Access, null, TSSL.Null_Pointer) /= Key
      then
         TSSL.BIO_free (IO);
         TSSL.RSA_free (Key);
         File_Error ("Key", Filename);
      end if;

      TSSL.BIO_free (IO);

      return Private_Key (Key);
   end Load;

   ---------------
   -- Log_Error --
   ---------------

   procedure Log_Error (Text : String) is
   begin
      Log.Error (Socket_Type'(Net.Std.Socket_Type with others => <>), Text);
   end Log_Error;

   -------------
   -- Pending --
   -------------

   overriding function Pending
     (Socket : Socket_Type) return Stream_Element_Count
   is
      Res : constant C.int := TSSL.SSL_pending (Socket.SSL);
   begin
      Error_If (Socket, Res < 0);
      return Stream_Element_Count (Res);
   end Pending;

   -------------
   -- Receive --
   -------------

   overriding procedure Receive
     (Socket : Socket_Type;
      Data   : out Stream_Element_Array;
      Last   : out Stream_Element_Offset)
   is
      Len   : C.int;
      First : Stream_Element_Offset := Data'First;
   begin
      loop
         Len := TSSL.SSL_read
                  (Socket.SSL, Data (First)'Address,
                   Data'Length - C.int (First - Data'First));

         if Len > 0 then
            First := First + Stream_Element_Offset (Len);
            Last  := First - 1;

            exit when Last = Data'Last;

         else
            exit when First > Data'First and then NSST (Socket).Pending = 0;

            case TSSL.SSL_get_error (Socket.SSL, Len) is
               when TSSL.SSL_ERROR_WANT_READ  => Socket_Read (Socket);
               when TSSL.SSL_ERROR_WANT_WRITE => Socket_Write (Socket);
               when TSSL.SSL_ERROR_SYSCALL =>
                  Raise_Socket_Error
                    (Socket,
                     "System error ("
                     & Utils.Image (Integer (OS_Lib.Socket_Errno))
                     & ") on SSL receive");

               when TSSL.SSL_ERROR_ZERO_RETURN =>
                  if Len = 0 then
                     raise Socket_Error with Peer_Closed_Message;
                  else
                     Raise_Socket_Error (Socket, Error_Stack);
                  end if;

               when others => Raise_Socket_Error (Socket, Error_Stack);
            end case;
         end if;
      end loop;
   end Receive;

   -------------
   -- Release --
   -------------

   procedure Release (Config : in out SSL.Config) is
      procedure Free is new Unchecked_Deallocation (TS_SSL, SSL.Config);
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
     (Source : Net.Socket_Type'Class;
      Target : out Socket_Type;
      Config : SSL.Config) is
   begin
      Std.Socket_Type (Target) := Std.Socket_Type (Source);

      if Config = null then
         Initialize_Default_Config;
         Target.Config := Default_Config;
      else
         Target.Config := Config;
      end if;

      Target.Config.Set_IO (Target);
   end Secure;

   -------------------
   -- Secure_Client --
   -------------------

   function Secure_Client
     (Socket : Net.Socket_Type'Class;
      Config : SSL.Config := Null_Config) return Socket_Type
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
     (Socket : Net.Socket_Type'Class;
      Config : SSL.Config := Null_Config) return Socket_Type
   is
      Result : Socket_Type;
   begin
      Secure (Socket, Result, Config);
      Set_Accept_State (Result);
      return Result;
   end Secure_Server;

   ----------
   -- Send --
   ----------

   overriding procedure Send
     (Socket : Socket_Type;
      Data   : Stream_Element_Array;
      Last   : out Stream_Element_Offset)
   is
      RC        : C.int;
      RW        : constant RW_Data_Access := Net.Socket_Type (Socket).C;
      Pack_Size : Stream_Element_Count :=
                    Stream_Element_Count'Min (RW.Pack_Size, Data'Length);
   begin
      if not Check (Socket, (Input => False, Output => True)) (Output) then
         Last := Last_Index (Data'First, 0);
         return;
      end if;

      if not RW.Can_Wait then
         declare
            Free : constant Stream_Element_Offset := Socket.Output_Space;
         begin
            if Free > 0
              and then Pack_Size + Max_Overhead > Free
              and then Socket.Output_Busy > 0
            then
               if Free <= Max_Overhead then
                  Last := Last_Index (Data'First, 0);
                  return;
               else
                  Pack_Size := Free - Max_Overhead;
               end if;
            end if;
         end;
      end if;

      loop
         RC := TSSL.SSL_write (Socket.SSL, Data'Address, C.int (Pack_Size));

         if RC > 0 then
            if RC < C.int (Pack_Size) then
               --  Pack_Size initialization. This condition would be true only
               --  once per SSL socket.

               RW.Pack_Size := Stream_Element_Count (RC);
            end if;

            Socket_Write (Socket, RC);

            Last := Data'First + Stream_Element_Offset (RC) - 1;

            return;

         else
            declare
               Error_Code : constant C.int :=
                              TSSL.SSL_get_error (Socket.SSL, RC);

               Err_Code   : TSSL.Error_Code;
               Err_No     : Integer;

               use type TSSL.Error_Code;
            begin
               case Error_Code is
                  when TSSL.SSL_ERROR_WANT_READ =>
                     Socket_Read (Socket);

                  when TSSL.SSL_ERROR_WANT_WRITE =>
                     Socket_Write (Socket);

                  when TSSL.SSL_ERROR_SYSCALL =>
                     Err_No := OS_Lib.Socket_Errno;

                     if Err_No = 0 then
                        Socket_Write (Socket);
                        Last := Data'First - 1;

                        return;

                     else
                        Raise_Socket_Error
                          (Socket,
                           "System error (" & Utils.Image (Err_No)
                           & ") on SSL send");
                     end if;

                  when others =>
                     Err_Code := TSSL.ERR_get_error;

                     if Err_Code = 0 then
                        Raise_Socket_Error
                          (Socket,
                           "Error ("
                           & Utils.Image (Integer (Error_Code))
                           & ") on SSL send");
                     else
                        Raise_Socket_Error (Socket, Error_Str (Err_Code));
                     end if;
               end case;
            end;
         end if;
      end loop;
   end Send;

   --------------------------
   -- Session_Cache_Number --
   --------------------------

   function Session_Cache_Number
     (Config : SSL.Config := Null_Config) return Natural
   is
      Cfg : constant SSL.Config :=
        (if Config = Null_Config then Default_Config else Config);
   begin
      return Cfg.Session_Cache_Number;
   end Session_Cache_Number;

   ------------------
   -- Session_Data --
   ------------------

   function Session_Data (Socket : Socket_Type) return Session_Type is
   begin
      return Session_Type (TSSL.SSL_get1_session (Socket.SSL));
   end Session_Data;

   ----------------------
   -- Session_Id_Image --
   ----------------------

   function Session_Id_Image (Session : Session_Type) return String is
      use TSSL;

      subtype Binary_Array is Stream_Element_Array (1 .. 1024);
      type Binary_Access is access all Binary_Array;

      function To_Array is
        new Ada.Unchecked_Conversion (Pointer, Binary_Access);

      Len : aliased C.unsigned;
      Id  : Binary_Access;

   begin
      if Session = null then
         return "";
      end if;

      Id := To_Array (SSL_SESSION_get_id (SSL_Session (Session), Len'Access));

      return Translator.Base64_Encode (Id (1 .. Stream_Element_Offset (Len)));
   end Session_Id_Image;

   function Session_Id_Image (Socket : Socket_Type) return String is
   begin
      return Session_Id_Image
               (Session_Type (TSSL.SSL_get_session (Socket.SSL)));
   end Session_Id_Image;

   --------------------
   -- Session_Reused --
   --------------------

   function Session_Reused (Socket : Socket_Type) return Boolean is
      use TSSL;
      use type C.long;
      Rc : constant C.long :=
             SSL_ctrl -- SSL_session_reused is macro in OpenSSL sources
               (Socket.SSL, SSL_CTRL_GET_SESSION_REUSED, 0, Null_Pointer);
   begin
      if Rc = 0 then
         return False;
      end if;

      Error_If (Socket, Rc /= 1);

      return True;
   end Session_Reused;

   ----------------------
   -- Set_Accept_State --
   ----------------------

   procedure Set_Accept_State (Socket : Socket_Type) is
   begin
      Socket.Config.Check_CRL;
      TSSL.SSL_set_accept_state (Socket.SSL);

      if RSA_Params (0) = TSSL.Null_Pointer
        and then DH_Params (0) = TSSL.Null_Pointer
      then
         if not RSA_Lock.Locked and then not DH_Lock.Locked then
            Start_Parameters_Generation (DH => True);
         end if;

      else
         if RSA_Params (0) /= TSSL.Null_Pointer then
            TSSL.SSL_set_tmp_rsa_callback
              (SSL => Socket.SSL, RSA_CB => Tmp_RSA_Callback'Access);
         end if;

         if DH_Params (0) /= TSSL.Null_Pointer then
            TSSL.SSL_set_tmp_dh_callback
              (SSL => Socket.SSL, DH_CB => Tmp_DH_Callback'Access);
         end if;
      end if;
   end Set_Accept_State;

   ----------------
   -- Set_Config --
   ----------------

   procedure Set_Config
     (Socket : in out Socket_Type; Config : SSL.Config) is
   begin
      Socket.Config := Config;
   end Set_Config;

   ---------------
   -- Set_Debug --
   ---------------

   procedure Set_Debug (Level : Natural) is
   begin
      Debug_Level := Level;
   end Set_Debug;

   ----------------------------
   -- Set_Session_Cache_Size --
   ----------------------------

   procedure Set_Session_Cache_Size
     (Size : Natural; Config : SSL.Config := Null_Config) is
   begin
      if Config = Null_Config then
         Initialize_Default_Config;
         Default_Config.Set_Session_Cache_Size (Size);
      else
         Config.Set_Session_Cache_Size (Size);
      end if;
   end Set_Session_Cache_Size;

   ----------------------
   -- Set_Session_Data --
   ----------------------

   procedure Set_Session_Data
     (Socket : in out Socket_Type; Data : Session_Type) is
   begin
      if Socket.SSL = TSSL.Null_Pointer or else Socket.Get_FD = No_Socket then
         Socket.Sessn := Data;
      else
         Error_If
           (Socket,
            TSSL.SSL_set_session (Socket.SSL, TSSL.SSL_Session (Data)) = 0);
      end if;
   end Set_Session_Data;

   -------------------------
   -- Set_Verify_Callback --
   -------------------------

   procedure Set_Verify_Callback
     (Config : in out SSL.Config; Callback : System.Address) is
   begin
      Config.Set_Verify_Callback (Callback);
   end Set_Verify_Callback;

   --------------
   -- Shutdown --
   --------------

   overriding procedure Shutdown
     (Socket : Socket_Type; How : Shutmode_Type := Shut_Read_Write)
   is
      RC   : C.int;
      Both : Boolean := False;

      function Error_Processing return Boolean;
      --  Exit from shutdown loop on True result

      ----------------------
      -- Error_Processing --
      ----------------------

      function Error_Processing return Boolean is
         Error_Code : constant C.int := TSSL.SSL_get_error (Socket.SSL, RC);
         Err_Code   : TSSL.Error_Code;

         use type TSSL.Error_Code;

      begin
         case Error_Code is
            when TSSL.SSL_ERROR_WANT_READ =>
               if Net.Socket_Type (Socket).Timeout > Shutdown_Read_Timeout then
                  Socket_Write (Socket);
                  Wait_For (Input, NSST (Socket), Shutdown_Read_Timeout);
               end if;

               Socket_Read (Socket);

            when TSSL.SSL_ERROR_WANT_WRITE =>
               Socket_Write (Socket);

            when TSSL.SSL_ERROR_SYSCALL =>
               Net.Log.Error
                 (Socket,
                  "System error (" & Utils.Image (OS_Lib.Socket_Errno)
                  & ") on SSL shutdown");

               return True;

            when others =>
               Err_Code := TSSL.ERR_get_error;

               if Err_Code = 0 then
                  Net.Log.Error
                    (Socket,
                     "Error (" & Utils.Image (Integer (Error_Code))
                     & ") on SSL shutdown");
               else
                  Net.Log.Error (Socket, Error_Str (Err_Code));
               end if;

               return True;
         end case;

         return False;

      exception
         when Socket_Error =>
            return True;
      end Error_Processing;

   begin
      if Socket.SSL /= TSSL.Null_Handle then
         loop
            RC := TSSL.SSL_shutdown (Socket.SSL);

            exit when RC > 0;

            if RC = 0 then
               --  First part of bidirectional shutdown done

               if How = Shut_Write then
                  exit;
               end if;

               if Both then
                  exit when Error_Processing;
               else
                  Both := True;
               end if;

            else
               exit when Error_Processing;
            end if;
         end loop;
      end if;

      Net.Std.Shutdown (NSST (Socket), How);
   end Shutdown;

   ---------------
   -- Signature --
   ---------------

   function Signature
     (Ptr  : System.Address;
      Size : C.size_t;
      Key  : Private_Key;
      Hash : Hash_Method) return Stream_Element_Array
   is
      use TSSL;
      use type C.unsigned;
      To_EVP_MD : constant array (Hash_Method) of EVP_MD :=
                    (MD5    => EVP_md5,
                     SHA1   => EVP_sha1,
                     SHA224 => EVP_sha224,
                     SHA256 => EVP_sha256,
                     SHA384 => EVP_sha384,
                     SHA512 => EVP_sha512);

      To_NID : constant array (Hash_Method) of C.int :=
                 (MD5    => NID_md5,
                  SHA1   => NID_sha1,
                  SHA224 => NID_sha224,
                  SHA256 => NID_sha256,
                  SHA384 => NID_sha384,
                  SHA512 => NID_sha512);

      Md  : constant EVP_MD := To_EVP_MD (Hash);
      Ctx : constant EVP_MD_CTX := EVP_MD_CTX_create;
      Dig : Stream_Element_Array
              (1 .. Stream_Element_Offset (EVP_MD_size (Md)));
      Res : Stream_Element_Array
              (1 .. Stream_Element_Offset (RSA_size (RSA (Key))));
      Len : aliased C.unsigned := Dig'Length;
   begin
      Error_If (EVP_DigestInit (Ctx, Md) = 0);
      Error_If (EVP_DigestUpdate (Ctx, Ptr, Size) = 0);
      Error_If (EVP_DigestFinal (Ctx, Dig'Address, Len'Access) = 0);

      if Len /= Dig'Length then
         raise Program_Error with "Digest length error";
      end if;

      EVP_MD_CTX_destroy (Ctx);

      Len := Res'Length;
      Error_If
        (RSA_sign
           (To_NID (Hash), Dig'Address, Dig'Length, Res'Address, Len'Access,
            RSA (Key)) /= 1);

      return Res;
   end Signature;

   -----------------
   -- Socket_Pair --
   -----------------

   overriding procedure Socket_Pair (S1, S2 : out Socket_Type) is
      ST1, ST2 : Std.Socket_Type;
   begin
      Std.Socket_Pair (ST1, ST2);
      S1 := Secure_Server (ST1);
      S2 := Secure_Client (ST2);
   end Socket_Pair;

   -----------------
   -- Socket_Read --
   -----------------

   procedure Socket_Read (Socket : Socket_Type) is
      use TSSL;

      Data : aliased Memory_Access;
      Len  : Stream_Element_Offset;
      Last : Stream_Element_Offset;
   begin
      Socket_Write (Socket);

      Len := Stream_Element_Offset (BIO_nwrite0 (Socket.IO, Data'Address));

      Net.Std.Receive (NSST (Socket), Data (1 .. Len), Last);

      if TSSL.BIO_nwrite (Socket.IO, Data'Address, C.int (Last))
         /= C.int (Last)
      then
         raise Socket_Error with "Not enought memory.";
      end if;
   end Socket_Read;

   ------------------
   -- Socket_Write --
   ------------------

   procedure Socket_Write (Socket : Socket_Type; Gone : C.int := 0) is
      use TSSL;

      Data  : aliased Memory_Access;
      Cnt   : constant C.int := BIO_nread0 (Socket.IO, Data'Address);
      Last  : Stream_Element_Offset;
      Plain : constant Net.Std.Socket_Type := NSST (Socket);
   begin
      if Cnt <= 0 then
         return;
      end if;

      if Gone > 0 and then Cnt - Gone > C.int (Max_Overhead) then
         --  Looks like Max_Overhead is not enought

         Max_Overhead := Stream_Element_Offset (Cnt - Gone);

         Log.Error
           (Socket,
            "Increase Max_Overhead to" & Max_Overhead'Img
            & " in the aws-net-ssl_openssl.adb to avoid send locking");
      end if;

      Plain.Send (Data (1 .. Stream_Element_Offset (Cnt)), Last);

      if Last < Stream_Element_Offset (Cnt) then
         if not Net.Socket_Type (Socket).C.Can_Wait then
            Log.Error (Socket, "Unexpected blocking send");
         end if;

         --  Most likely Max_Overhead value is not enought, send rest data
         --  locking.

         Plain.Send (Data (Last + 1 .. Stream_Element_Offset (Cnt)));
      end if;

      if BIO_nread (Socket.IO, Data'Address, Cnt) /= Cnt then
         raise Program_Error;
      end if;
   end Socket_Write;

   -------------
   -- Locking --
   -------------

   package body Locking is

      type Task_Identifier is new C.unsigned_long;
      type Lock_Index is new C.int;
      type Mode_Type is mod 2 ** C.int'Size;

      type Task_Data is record
         TID : Task_Identifier;
      end record;

      subtype Filename_Type is C.Strings.chars_ptr;
      subtype Line_Number is C.int;

      Finalized : Boolean := False;
      --  Need to avoid access to finalized protected locking objects

      package Task_Identifiers is new Task_Attributes (Task_Data, (TID => 0));

      procedure Finalize;

      protected Task_Id_Generator is

         procedure Get_Task_Id (Id : out Task_Identifier) with
           Post => Id > 0;
         --  Return a uniq Id starting from 1 and incrementing one by one

         procedure Finalize_Task
           (Cause : Task_Termination.Cause_Of_Termination;
            T     : Task_Identification.Task_Id;
            X     : Exceptions.Exception_Occurrence);

      private
         Id_Counter : Task_Identifier := 0;
      end Task_Id_Generator;

      subtype RW_Mutex is AWS.Utils.RW_Semaphore (1);

      type RW_Mutex_Access is access all RW_Mutex;

      Locks : array (1 .. Lock_Index (TSSL.CRYPTO_num_locks)) of RW_Mutex;

      F : Utils.Finalizer (Finalize'Access) with Unreferenced;

      procedure Lock (Mode : Mode_Type; Locker : in out RW_Mutex) with Inline;

      procedure Locking_Function
        (Mode : Mode_Type;
         N    : Lock_Index;
         File : Filename_Type;
         Line : Line_Number)
        with Convention => C;

      function Dyn_Create
        (File : Filename_Type; Line : Line_Number) return RW_Mutex_Access
        with Convention => C;

      procedure Dyn_Lock
        (Mode   : Mode_Type;
         Locker : RW_Mutex_Access;
         File   : Filename_Type;
         Line   : Line_Number)
        with Convention => C;

      procedure Dyn_Destroy
        (Locker : RW_Mutex_Access;
         File   : Filename_Type;
         Line   : Line_Number)
        with Convention => C;

      function Get_Task_Identifier return Task_Identifier with Convention => C;

      ----------------
      -- Dyn_Create --
      ----------------

      function Dyn_Create
        (File : Filename_Type; Line : Line_Number) return RW_Mutex_Access
      is
         pragma Unreferenced (File, Line);
      begin
         return new RW_Mutex;
      end Dyn_Create;

      -----------------
      -- Dyn_Destroy --
      -----------------

      procedure Dyn_Destroy
        (Locker : RW_Mutex_Access;
         File   : Filename_Type;
         Line   : Line_Number)
      is
         pragma Unreferenced (File, Line);

         Temp : RW_Mutex_Access := Locker;

         procedure Free is
            new Unchecked_Deallocation (RW_Mutex, RW_Mutex_Access);
      begin
         Free (Temp);
      end Dyn_Destroy;

      --------------
      -- Dyn_Lock --
      --------------

      procedure Dyn_Lock
        (Mode   : Mode_Type;
         Locker : RW_Mutex_Access;
         File   : Filename_Type;
         Line   : Line_Number)
      is
         pragma Unreferenced (File, Line);
      begin
         Lock (Mode, Locker.all);
      end Dyn_Lock;

      --------------
      -- Finalize --
      --------------

      procedure Finalize is
      begin
         for J in DH_Params'Range loop
            if DH_Params (J) /= TSSL.Null_Pointer then
               TSSL.DH_free (DH_Params (J));
               DH_Params (J) := TSSL.Null_Pointer;
            end if;
         end loop;

         Finalized := True;
      end Finalize;

      -------------------------
      -- Get_Task_Identifier --
      -------------------------

      function Get_Task_Identifier return Task_Identifier is
         TA : constant Task_Identifiers.Attribute_Handle :=
                Task_Identifiers.Reference;
      begin
         if TA.TID = 0 then
            Task_Id_Generator.Get_Task_Id (TA.TID);

            Task_Termination.Set_Specific_Handler
              (Task_Identification.Current_Task,
               Task_Id_Generator.Finalize_Task'Access);
         end if;

         return TA.TID;
      end Get_Task_Identifier;

      ----------------
      -- Initialize --
      ----------------

      procedure Initialize is
      begin
         --  We do not have to install thread id_callback on MS Windows and
         --  on platforms where getpid returns different id for each thread.
         --  But it is easier to install it for all platforms.

         TSSL.CRYPTO_set_id_callback (Get_Task_Identifier'Address);
         TSSL.CRYPTO_set_locking_callback (Locking_Function'Address);

         TSSL.CRYPTO_set_dynlock_create_callback  (Dyn_Create'Address);
         TSSL.CRYPTO_set_dynlock_lock_callback    (Dyn_Lock'Address);
         TSSL.CRYPTO_set_dynlock_destroy_callback (Dyn_Destroy'Address);
      end Initialize;

      ----------
      -- Lock --
      ----------

      procedure Lock (Mode : Mode_Type; Locker : in out RW_Mutex) is
      begin
         if Finalized then
            return;
         end if;

         case Mode is
            when TSSL.CRYPTO_LOCK   or TSSL.CRYPTO_WRITE =>
               Locker.Write;
            when TSSL.CRYPTO_LOCK   or TSSL.CRYPTO_READ  =>
               Locker.Read;
            when TSSL.CRYPTO_UNLOCK or TSSL.CRYPTO_WRITE =>
               Locker.Release_Write;
            when TSSL.CRYPTO_UNLOCK or TSSL.CRYPTO_READ  =>
               Locker.Release_Read;
            when others => null;
         end case;
      end Lock;

      ----------------------
      -- Locking_Function --
      ----------------------

      procedure Locking_Function
        (Mode : Mode_Type;
         N    : Lock_Index;
         File : Filename_Type;
         Line : Line_Number)
      is
         pragma Unreferenced (File, Line);
      begin
         Lock (Mode, Locks (N));
      end Locking_Function;

      -----------------------
      -- Task_Id_Generator --
      -----------------------

      protected body Task_Id_Generator is

         -------------------
         -- Finalize_Task --
         -------------------

         procedure Finalize_Task
           (Cause : Task_Termination.Cause_Of_Termination;
            T     : Task_Identification.Task_Id;
            X     : Exceptions.Exception_Occurrence)
         is
            pragma Unreferenced (Cause, X);
         begin
            TSSL.ERR_remove_state (C.int (Task_Identifiers.Reference (T).TID));
         end Finalize_Task;

         -----------------
         -- Get_Task_Id --
         -----------------

         procedure Get_Task_Id (Id : out Task_Identifier) is
         begin
            Id_Counter := Id_Counter + 1;
            Id := Id_Counter;
         end Get_Task_Id;

      end Task_Id_Generator;

   end Locking;

   ---------------------------------
   -- Start_Parameters_Generation --
   ---------------------------------

   procedure Start_Parameters_Generation
     (DH : Boolean; Logging : access procedure (Text : String) := null) is
   begin
      RSA_DH_Generators.Start_Parameters_Generation (DH, Logging);
   end Start_Parameters_Generation;

   ---------------------
   -- Tmp_DH_Callback --
   ---------------------

   function Tmp_DH_Callback
     (SSL : SSL_Handle; Is_Export : C.int; Keylength : C.int) return TSSL.DH
   is
      pragma Unreferenced (SSL, Is_Export, Keylength);
      --  Ignore export restrictions
   begin
      return DH_Params (0);
   end Tmp_DH_Callback;

   ----------------------
   -- Tmp_RSA_Callback --
   ----------------------

   function Tmp_RSA_Callback
     (SSL : SSL_Handle; Is_Export : C.int; Keylength : C.int) return TSSL.RSA
   is
      pragma Unreferenced (SSL, Is_Export, Keylength);
      --  Ignore export restrictions
   begin
      return RSA_Params (0);
   end Tmp_RSA_Callback;

   ------------
   -- TS_SSL --
   ------------

   protected body TS_SSL is

      ---------------
      -- Check_CRL --
      ---------------

      procedure Check_CRL is
         use type Ada.Calendar.Time;
         use type C.Strings.chars_ptr;
      begin
         --  We need to load the CRL file if CRL_Time_Stamp is AWS_Epoch (it
         --  has never been loaded) or the time stamp has changed on disk.

         if CRL_File /= C.Strings.Null_Ptr then
            declare
               TS : constant Calendar.Time :=
                      Utils.File_Time_Stamp (C.Strings.Value (CRL_File));
            begin
               if CRL_Time_Stamp = Utils.AWS_Epoch
                 or else CRL_Time_Stamp /= TS
               then
                  CRL_Time_Stamp := TS;

                  declare
                     Store  : constant TSSL.X509_STORE :=
                                TSSL.SSL_CTX_get_cert_store (Context);
                     Lookup : constant TSSL.X509_LOOKUP :=
                                TSSL.X509_STORE_add_lookup
                                  (Store, TSSL.X509_LOOKUP_file);
                     Param  : constant TSSL.X509_VERIFY_PARAM :=
                                TSSL.X509_VERIFY_PARAM_new;
                  begin
                     --  Load CRL

                     Error_If
                       (TSSL.X509_load_crl_file
                          (Lookup, CRL_File, TSSL.X509_FILETYPE_PEM) /= 1);

                     --  Set up certificate store to check CRL

                     Error_If
                       (TSSL.X509_VERIFY_PARAM_set_flags
                          (Param, TSSL.X509_V_FLAG_CRL_CHECK) < 0);
                     Error_If (TSSL.X509_STORE_set1_param (Store, Param) < 0);
                     TSSL.X509_VERIFY_PARAM_free (Param);
                  end;
               end if;
            end;
         end if;
      end Check_CRL;

      -------------------------
      -- Clear_Session_Cache --
      -------------------------

      procedure Clear_Session_Cache is
      begin
         TSSL.SSL_CTX_flush_sessions (Context, C.long'Last);
      end Clear_Session_Cache;

      ----------------
      -- File_Error --
      ----------------

      procedure File_Error (Prefix, Name : String) is
      begin
         raise Socket_Error
            with Prefix & " file """ & Name & """ error."
               & ASCII.LF & Error_Stack;
      end File_Error;

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
        (Certificate_Filename : String;
         Security_Mode        : Method;
         Priorities           : String;
         Ticket_Support       : Boolean;
         Key_Filename         : String;
         Exchange_Certificate : Boolean;
         Certificate_Required : Boolean;
         Trusted_CA_Filename  : String;
         CRL_Filename         : String;
         Session_Cache_Size   : Positive)
      is
         type Meth_Func is access function return TSSL.SSL_Method
           with Convention => C;

         procedure Set_Certificate
           (Cert_Filename : String; Key_Filename : String);

         procedure Set_Priorities;

         Methods : constant array (Method) of Meth_Func :=
                     (SSLv23        => TSSL.SSLv23_method'Access,
                      SSLv23_Server => TSSL.SSLv23_server_method'Access,
                      SSLv23_Client => TSSL.SSLv23_client_method'Access,
                      TLSv1         => TSSL.TLSv1_method'Access,
                      TLSv1_Server  => TSSL.TLSv1_server_method'Access,
                      TLSv1_Client  => TSSL.TLSv1_client_method'Access,
                      SSLv3         => TSSL.SSLv3_method'Access,
                      SSLv3_Server  => TSSL.SSLv3_server_method'Access,
                      SSLv3_Client  => TSSL.SSLv3_client_method'Access);

         ---------------------
         -- Set_Certificate --
         ---------------------

         procedure Set_Certificate
           (Cert_Filename : String; Key_Filename : String)
         is
            use Interfaces.C;

            PK : constant Private_Key :=
                   Load ((if Key_Filename = ""
                          then Cert_Filename else Key_Filename));

         begin
            --  Get the single certificate or certificate chain from
            --  the file Cert_Filename.

            if TSSL.SSL_CTX_use_certificate_chain_file
                 (Ctx => Context, File => To_C (Cert_Filename)) /= 1
            then
               File_Error ("Certificate", Cert_Filename);
            end if;

            Error_If
              (TSSL.SSL_CTX_use_RSAPrivateKey (Context, TSSL.RSA (PK)) /= 1);
            Error_If (TSSL.SSL_CTX_check_private_key (Ctx => Context) /= 1);

            if Exchange_Certificate then
               declare
                  Data : aliased constant Stream_Element_Array :=
                           Signature (Command_Line.Command_Name, PK, SHA224);
               begin
                  Error_If
                    (TSSL.SSL_CTX_set_session_id_context
                       (Context, Data'Address,
                        C.unsigned'Min
                          (TSSL.SSL_MAX_SSL_SESSION_ID_LENGTH, Data'Length))
                     = 0);
               end;
            end if;

            --  Set Trusted Certificate Authority if any

            if Trusted_CA_Filename /= "" then
               declare
                  use C.Strings;
                  CAfile : aliased C.char_array :=
                             C.To_C (Trusted_CA_Filename);
                  CA_ptr : constant chars_ptr :=
                             To_Chars_Ptr (CAfile'Unchecked_Access);
               begin
                  Error_If
                    (TSSL.SSL_CTX_load_verify_locations
                       (Context, CA_ptr, Null_Ptr) /= 1);

                  if Exchange_Certificate then
                     --  Let server send to client CA authority names it trust

                     TSSL.SSL_CTX_set_client_CA_list
                       (Context, TSSL.SSL_load_client_CA_file (CA_ptr));
                  end if;
               end;
            end if;

            --  Set Certificate Revocation List if any

            if CRL_Filename /= "" then
               CRL_File := C.Strings.New_String (CRL_Filename);
               Check_CRL;
            end if;
         end Set_Certificate;

         --------------------
         -- Set_Priorities --
         --------------------

         procedure Set_Priorities is
            Pr : aliased C.char_array := C.To_C (Priorities);
         begin
            if TSSL.SSL_CTX_set_cipher_list
                 (Context, C.Strings.To_Chars_Ptr (Pr'Unchecked_Access)) = 0
            then
               Log_Error (Error_Stack);
            end if;
         end Set_Priorities;

      begin
         if Context = TSSL.Null_Pointer then
            --  Initialize context

            Context := TSSL.SSL_CTX_new (Methods (Security_Mode).all);
            Error_If (Context = TSSL.Null_Pointer);

            if not Ticket_Support then
               Error_If
                 (TSSL.SSL_CTX_ctrl
                    (Ctx  => Context,
                     Cmd  => TSSL.SSL_CTRL_OPTIONS,
                     Larg => TSSL.SSL_OP_NO_TICKET,
                     Parg => TSSL.Null_Pointer) = 0);
            end if;

            Error_If
              (TSSL.SSL_CTX_ctrl
                 (Ctx  => Context,
                  Cmd  => TSSL.SSL_CTRL_MODE,
                  Larg => TSSL.SSL_MODE_ENABLE_PARTIAL_WRITE
                          + TSSL.SSL_MODE_ACCEPT_MOVING_WRITE_BUFFER,
                  Parg => TSSL.Null_Pointer) = 0);

            if Exchange_Certificate then
               --  Client is requested to send its certificate once

               Error_If
                 (TSSL.SSL_CTX_set_ex_data
                    (Context, Data_Index, TSSL.Null_Pointer) = -1);

               declare
                  Mode : C.int :=
                           TSSL.SSL_VERIFY_PEER + TSSL.SSL_VERIFY_CLIENT_ONCE;
               begin
                  if Certificate_Required then
                     Mode := Mode + TSSL.SSL_VERIFY_FAIL_IF_NO_PEER_CERT;
                  end if;

                  TSSL.SSL_CTX_set_verify
                    (Context, Mode, Verify_Callback'Address);
               end;
            end if;

            if Certificate_Filename /= "" then
               Set_Certificate (Certificate_Filename, Key_Filename);
            end if;

            if Priorities /= "" then
               Set_Priorities;
            end if;

            TS_SSL.Set_Session_Cache_Size (Session_Cache_Size);
         end if;
      end Initialize;

      --------------------------
      -- Session_Cache_Number --
      --------------------------

      function Session_Cache_Number return Natural is
         use TSSL;
      begin
         return Natural (SSL_CTX_ctrl
                           (Context, SSL_CTRL_SESS_NUMBER, 0, Null_Pointer));
      end Session_Cache_Number;

      ------------
      -- Set_IO --
      ------------

      procedure Set_IO (Socket : in out Socket_Type) is
         use TSSL;
         use type C.long;
         Inside_IO, Net_IO : aliased BIO_Access;
      begin
         Socket.SSL := SSL_new (Context);
         Error_If (Socket, Socket.SSL = Null_Pointer);

         Error_If
           (BIO_new_bio_pair (Inside_IO'Access, 0, Net_IO'Access, 0) = 0);

         case Debug_Level is
            when 0 => null;
            when 1 => BIO_set_callback (Inside_IO, BIO_debug_callback'Access);
            when 2 => BIO_set_callback (Net_IO, BIO_debug_callback'Access);
            when others =>
               BIO_set_callback (Inside_IO, BIO_debug_callback'Access);
               BIO_set_callback (Net_IO, BIO_debug_callback'Access);
         end case;

         Socket.IO := Net_IO;

         SSL_set_bio (Socket.SSL, Inside_IO, Inside_IO);
      end Set_IO;

      ----------------------------
      -- Set_Session_Cache_Size --
      ----------------------------

      procedure Set_Session_Cache_Size (Size : Natural) is
      begin
         Error_If
           (TSSL.SSL_CTX_ctrl
              (Ctx  => Context,
               Cmd  => TSSL.SSL_CTRL_SET_SESS_CACHE_SIZE,
               Larg => C.int (Size),
               Parg => TSSL.Null_Pointer) = -1);
      end Set_Session_Cache_Size;

      -------------------------
      -- Set_Verify_Callback --
      -------------------------

      procedure Set_Verify_Callback (Callback : System.Address) is
      begin
         Error_If
           (TSSL.SSL_CTX_set_ex_data
              (Context, Data_Index, Callback) = -1);
      end Set_Verify_Callback;

   end TS_SSL;

   ---------------------
   -- Verify_Callback --
   ---------------------

   function Verify_Callback
     (preverify_ok : C.int; ctx : TSSL.X509_STORE_CTX) return C.int
   is
      use type C.unsigned;
      use type Net.SSL.Certificate.Verify_Callback;

      function To_Callback is new Unchecked_Conversion
        (System.Address, Net.SSL.Certificate.Verify_Callback);

      CB      : aliased Net.SSL.Certificate.Verify_Callback;
      SSL     : SSL_Handle;
      SSL_CTX : TSSL.SSL_CTX;
      Cert    : TSSL.X509;
      Res     : C.int := preverify_ok;
      Mode    : C.unsigned;
      Status  : C.int;
   begin
      --  The SSL structure

      SSL := TSSL.X509_STORE_CTX_get_ex_data
        (ctx, TSSL.SSL_get_ex_data_X509_STORE_CTX_idx);

      --  The SSL context, this is the one we are looking for as it contains
      --  the register callback.

      SSL_CTX := TSSL.SSL_get_SSL_CTX (SSL);

      --  Get the current verification mode

      Mode := TSSL.SSL_get_verify_mode (SSL);

      --  Get the certificate as stored into the context

      Cert := TSSL.X509_STORE_CTX_get_current_cert (ctx);

      --  Get current error status, this will be stored into the certificate
      --  by the read routine below. It will be available in the user's verify
      --  callback if needed.

      Status := TSSL.X509_STORE_CTX_get_error (ctx);

      --  Get the user's callback stored at the Data_Index

      CB := To_Callback (TSSL.SSL_CTX_get_ex_data (SSL_CTX, Data_Index));

      if CB /= null then
         if CB (Net.SSL.Certificate.Impl.Read (Status, Cert)) then
            Res := 1;
         else
            Res := 0;
         end if;
      end if;

      --  If we did not ask to fail if no peer cert was received just
      --  unconditionally returns 1 (OK).

      if (Mode and TSSL.SSL_VERIFY_FAIL_IF_NO_PEER_CERT) = 0 then
         return 1;
      else
         return Res;
      end if;
   end Verify_Callback;

   -------------
   -- Version --
   -------------

   function Version (Build_Info : Boolean := False) return String is
      use TSSL;
      Result : constant String :=
                 C.Strings.Value (SSLeay_version_info (SSLEAY_VERSION));
   begin
      if Build_Info then
         return Result & ASCII.LF
           & C.Strings.Value (SSLeay_version_info (SSLEAY_CFLAGS)) & ASCII.LF
           & C.Strings.Value (SSLeay_version_info (SSLEAY_BUILT_ON)) & ASCII.LF
           & C.Strings.Value (SSLeay_version_info (SSLEAY_PLATFORM)) & ASCII.LF
           & C.Strings.Value (SSLeay_version_info (SSLEAY_DIR));
      else
         return Result;
      end if;
   end Version;

begin
   --  Set the RTL memory allocation routines is necessary only to be able
   --  gnatmem control memory leak allocated inside of OpenSSL library.

   if TSSL.CRYPTO_set_mem_functions
        (M => System.Memory.Alloc'Address,
         R => Lib_Realloc'Address,
         F => System.Memory.Free'Address) = 0
   then
      --  In "OpenSSL FIPS Object Module" based on OpenSSL version 1.0.1e
      --  we are unable to setup our own memory allocation routines. GNATMEM
      --  would not be aware of memory allocations inside of OpenSSL in this
      --  case, but AWS is able to run anyway.
      null;
   end if;

   TSSL.SSL_load_error_strings;
   TSSL.SSL_library_init;
   Locking.Initialize;
   Init_Random;

   Data_Index :=
     TSSL.SSL_CTX_get_ex_new_index
       (0, TSSL.Null_Pointer, TSSL.Null_Pointer,
        TSSL.Null_Pointer, TSSL.Null_Pointer);
end AWS.Net.SSL;
