------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2008, AdaCore                     --
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

--  Routines here are wrappers around standard sockets and SSL.
--
--  IMPORTANT: The default certificate used for the SSL connection is
--  "cert.pem" (in the working directory) if it exists. If this file does
--  not exists it is required to initialize the SSL layer certificate with
--  AWS.Server.Set_Security.

with Ada.Calendar;
with Ada.Task_Attributes;
with Ada.Task_Identification;
with Ada.Task_Termination;
with Ada.Unchecked_Deallocation;
with Interfaces.C.Strings;
with System.Memory;
with System.Storage_Elements;

with AWS.Config;
with AWS.Net.Log;
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

   protected type TS_SSL is

      procedure Set_IO (Socket : in out Socket_Type);
      --  Bind the SSL handle with the BIO pair

      procedure Initialize
        (Certificate_Filename : in String;
         Security_Mode        : in Method;
         Key_Filename         : in String;
         Exchange_Certificate : in Boolean;
         Session_Cache_Size   : in Positive);

      procedure Finalize;

      procedure Clear_Session_Cache;

      procedure Set_Session_Cache_Size (Size : in Natural);

   private
      Context : TSSL.SSL_CTX := TSSL.Null_CTX;
   end TS_SSL;

   Default_Config : constant Config := new TS_SSL;

   procedure Socket_Read (Socket : in Socket_Type);
   --  Read encripted data from socket if necessary

   procedure Socket_Write (Socket : in Socket_Type);
   --  Write encripted data to socket if availabe

   procedure Error_If (Error : in Boolean);
   pragma Inline (Error_If);
   --  Raises Socket_Error if Error is true. Attach the SSL error message

   procedure Error_If (Socket : in Socket_Type; Error : in Boolean);
   pragma Inline (Error_If);
   --  Raises and log Socket_Error if Error is true.
   --  Attach the SSL error message.

   procedure Do_Handshake (Socket : in out Socket_Type; Success : out Boolean);
   --  Perform SSL handshake

   function Error_Stack return String;
   --  Returns error stack of the last SSL error in multiple lines

   function Error_Str (Code : in TSSL.Error_Code) return String;
   --  Returns the SSL error message for error Code

   procedure Init_Random;
   --  Initialize the SSL library with a random number

   procedure Initialize_Default_Config;
   --  Initializes default config. It could be called more then once, because
   --  secondary initialization is ignored.

   function Verify_Callback
     (preverify_ok : in Integer; ctx : in System.Address) return Integer;
   --  Dummy verify procedure that always return ok. This is needed to be able
   --  to retreive the client's certificate.

   procedure Secure
     (Source : in     Net.Socket_Type'Class;
      Target :    out Socket_Type;
      Config : in     SSL.Config);
   --  Common code for Secure_Server and Secure_Client routines

   -------------------
   -- Accept_Socket --
   -------------------

   overriding procedure Accept_Socket
     (Socket : in Net.Socket_Type'Class; New_Socket : in out Socket_Type)
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

         TSSL.SSL_set_accept_state (New_Socket.SSL);

         Do_Handshake (New_Socket, Success);

         exit SSL_Accept when Success;

         Shutdown (New_Socket);
      end loop SSL_Accept;
   end Accept_Socket;

   -------------------------
   -- Clear_Session_Cache --
   -------------------------

   procedure Clear_Session_Cache (Config : in SSL.Config := Null_Config) is
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
     (Socket   : in out Socket_Type;
      Host     : in     String;
      Port     : in     Positive;
      Wait     : in     Boolean := True)
   is
      Success : Boolean;
   begin
      Net.Std.Connect (NSST (Socket), Host, Port, Wait);

      if Socket.Config = null then
         Initialize_Default_Config;
         Socket.Config := Default_Config;
      end if;

      Socket.Config.Set_IO (Socket);

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

   procedure Error_If (Error : in Boolean) is
   begin
      if Error then
         raise Socket_Error with Error_Stack;
      end if;
   end Error_If;

   procedure Error_If (Socket : in Socket_Type; Error : in Boolean) is
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
         begin
            if Error_Text'Length > Trim_Start'Length
              and then Error_Text (First .. Trim_Start'Last) = Trim_Start
            then
               First := Error_Text'First + Trim_Start'Length;
            end if;

            return Error_Text (First .. Error_Text'Last) &
              ASCII.LF & Error_Stack;
         end;
      end if;
   end Error_Stack;

   ---------------
   -- Error_Str --
   ---------------

   function Error_Str (Code : in TSSL.Error_Code) return String is
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

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Socket : in out Socket_Type) is
   begin
      Std.Finalize (Std.Socket_Type (Socket));
   end Finalize;

   ----------
   -- Free --
   ----------

   procedure Free (Socket : in out Socket_Type) is
   begin
      if Socket.SSL /= TSSL.Null_Pointer then
         TSSL.SSL_free (Socket.SSL);
         TSSL.BIO_free (Socket.IO);
         Socket.SSL := TSSL.Null_Pointer;
      end if;

      NSST (Socket).Free;
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
      Exchange_Certificate : in     Boolean    := False;
      Session_Cache_Size   : in     Positive   := 16#4000#) is
   begin
      if Config = null then
         Config := new TS_SSL;
      end if;

      Config.Initialize
        (Certificate_Filename, Security_Mode, Key_Filename,
         Exchange_Certificate, Session_Cache_Size);
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
         Exchange_Certificate => CNF.Exchange_Certificate (Default),
         Session_Cache_Size   => 16#4000#);
   end Initialize_Default_Config;

   -------------
   -- Pending --
   -------------

   overriding function Pending
     (Socket : in Socket_Type) return Stream_Element_Count
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
     (Socket : in     Socket_Type;
      Data   :    out Stream_Element_Array;
      Last   :    out Stream_Element_Offset)
   is
      Len : C.int;
   begin
      loop
         Len := TSSL.SSL_read (Socket.SSL, Data'Address, Data'Length);

         exit when Len > 0;

         declare
            Error_Code : constant C.int
              := TSSL.SSL_get_error (Socket.SSL, Len);
         begin
            case Error_Code is
               when TSSL.SSL_ERROR_WANT_READ  => Socket_Read (Socket);
               when TSSL.SSL_ERROR_WANT_WRITE => Socket_Write (Socket);
               when TSSL.SSL_ERROR_SYSCALL =>
                  Raise_Socket_Error
                    (Socket,
                     "System error (" & Utils.Image (Integer (Errno))
                      & ") on SSL receive");

               when others => Raise_Socket_Error (Socket, Error_Stack);
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

      Target.Config.Set_IO (Target);
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

   overriding procedure Send
     (Socket : in     Socket_Type;
      Data   : in     Stream_Element_Array;
      Last   :    out Stream_Element_Offset)
   is
      RC     : C.int;

      procedure Socket_Write;
      --  Non blocking write from IO to socket

      ------------------
      -- Socket_Write --
      ------------------

      procedure Socket_Write is
         use TSSL;

         type Memory_Access is access all
           Stream_Element_Array (1 .. Stream_Element_Offset'Last);

         S_Last : Stream_Element_Offset;
         Data   : aliased Memory_Access;
         Len    : constant Stream_Element_Offset
           := Stream_Element_Offset (BIO_nread0 (Socket.IO, Data'Address));
      begin
         if Len <= 0 then
            return;
         end if;

         Net.Std.Send (NSST (Socket), Data (1 .. Len), S_Last);

         if S_Last /= Len then
            Raise_Socket_Error
              (Socket, "Socket buffer too small" & S_Last'Img & Len'Img);
         end if;

         if S_Last > 0
           and then BIO_nread (Socket.IO, Data'Address, C.int (S_Last))
                    /= C.int (S_Last)
         then
            Raise_Socket_Error (Socket, "Socket write IO error");
         end if;
      end Socket_Write;

   begin
      if not Check (Socket, (Input => False, Output => True)) (Output) then
         if Data'First = Stream_Element_Offset'First then
            Last := Stream_Element_Offset'Last;
         else
            Last := Data'First - 1;
         end if;

         return;
      end if;

      loop
         RC := TSSL.SSL_write (Socket.SSL, Data'Address, Data'Length);
         Socket_Write;

         if RC > 0 then
            Last  := Data'First + Stream_Element_Offset (RC) - 1;

            return;

         else
            declare
               Error_Code : constant C.int
                 := TSSL.SSL_get_error (Socket.SSL, RC);

               Err_Code : TSSL.Error_Code;

               use type TSSL.Error_Code;
            begin
               case Error_Code is
                  when TSSL.SSL_ERROR_WANT_READ =>
                     Socket_Read (Socket);

                  when TSSL.SSL_ERROR_SYSCALL =>
                     Raise_Socket_Error
                       (Socket,
                        "System error (" & Utils.Image (Integer (Errno))
                         & ") on SSL send");

                  when others =>
                     Err_Code := TSSL.ERR_get_error;

                     if Err_Code = 0 then
                        Raise_Socket_Error
                          (Socket,
                           "Error (" & Utils.Image (Integer (Error_Code))
                           & ") on SSL send");
                     else
                        Raise_Socket_Error (Socket, Error_Str (Err_Code));
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
     (Socket : in out Socket_Type; Config : in SSL.Config) is
   begin
      Socket.Config := Config;
   end Set_Config;

   ----------------------------
   -- Set_Session_Cache_Size --
   ----------------------------

   procedure Set_Session_Cache_Size
     (Size : in Natural; Config : in SSL.Config := Null_Config) is
   begin
      if Config = Null_Config then
         Initialize_Default_Config;
         Default_Config.Set_Session_Cache_Size (Size);
      else
         Config.Set_Session_Cache_Size (Size);
      end if;
   end Set_Session_Cache_Size;

   -----------------
   -- Set_Timeout --
   -----------------

   overriding procedure Set_Timeout
     (Socket  : in out Socket_Type; Timeout : in Duration) is
   begin
      Set_Timeout (Net.Socket_Type (Socket), Timeout);
   end Set_Timeout;

   --------------
   -- Shutdown --
   --------------

   overriding procedure Shutdown
     (Socket : in Socket_Type; How : in Shutmode_Type := Shut_Read_Write)
   is
      To_C : constant array (Shutmode_Type) of C.int :=
               (Shut_Read_Write => TSSL.SSL_SENT_SHUTDOWN
                                   + TSSL.SSL_RECEIVED_SHUTDOWN,
                Shut_Read       => TSSL.SSL_RECEIVED_SHUTDOWN,
                Shut_Write      => TSSL.SSL_SENT_SHUTDOWN);
   begin
      TSSL.SSL_set_shutdown (Socket.SSL, To_C (How));
      Net.Std.Shutdown (NSST (Socket), How);
   end Shutdown;

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

   procedure Socket_Read (Socket : in Socket_Type) is
      use TSSL;

      type Memory_Access is access all
        Stream_Element_Array (1 .. Stream_Element_Offset'Last);

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

   procedure Socket_Write (Socket : in Socket_Type) is
      use TSSL;
      type Memory_Access is access all
        Stream_Element_Array (1 .. Stream_Element_Offset'Last);

      Data : aliased Memory_Access;
      Last : constant Stream_Element_Offset
        := Stream_Element_Offset
             (BIO_nread (Socket.IO, Data'Address, C.int'Last));
      Plain : constant NSST := NSST (Socket);
      --  ??? Looks like direct type convertion lead to wrong dispatch
   begin
      if Last <= 0 then
         return;
      end if;

      Plain.Send (Data (1 .. Last));
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

      package Task_Identifiers is new Ada.Task_Attributes
        (Task_Data, (TID => 0));

      procedure Finalize;

      protected Task_Id_Generator is
         procedure Get_Task_Id (Id : out Task_Identifier);
         procedure Finalize_Task
           (Cause : in Ada.Task_Termination.Cause_Of_Termination;
            T     : in Ada.Task_Identification.Task_Id;
            X     : in Ada.Exceptions.Exception_Occurrence);
      private
         Id_Counter : Task_Identifier := 0;
      end Task_Id_Generator;

      subtype RW_Mutex is AWS.Utils.RW_Semaphore (1);

      type RW_Mutex_Access is access all RW_Mutex;

      Locks : array (1 .. Lock_Index (TSSL.CRYPTO_num_locks)) of RW_Mutex;

      F : Utils.Finalizer (Finalize'Access);
      pragma Unreferenced (F);

      procedure Lock (Mode : in Mode_Type; Locker : in out RW_Mutex);
      pragma Inline (Lock);

      procedure Locking_Function
        (Mode : in Mode_Type;
         N    : in Lock_Index;
         File : in Filename_Type;
         Line : in Line_Number);
      pragma Convention (C, Locking_Function);

      function Dyn_Create
        (File : in Filename_Type; Line : in Line_Number)
         return RW_Mutex_Access;
      pragma Convention (C, Dyn_Create);

      procedure Dyn_Lock
        (Mode   : in Mode_Type;
         Locker : in RW_Mutex_Access;
         File   : in Filename_Type;
         Line   : in Line_Number);
      pragma Convention (C, Dyn_Lock);

      procedure Dyn_Destroy
        (Locker : in RW_Mutex_Access;
         File   : in Filename_Type;
         Line   : in Line_Number);
      pragma Convention (C, Dyn_Destroy);

      function Get_Task_Identifier return Task_Identifier;
      pragma Convention (C, Get_Task_Identifier);

      ----------------
      -- Dyn_Create --
      ----------------

      function Dyn_Create
        (File : in Filename_Type; Line : in Line_Number)
         return RW_Mutex_Access
      is
         pragma Unreferenced (File, Line);
      begin
         return new RW_Mutex;
      end Dyn_Create;

      -----------------
      -- Dyn_Destroy --
      -----------------

      procedure Dyn_Destroy
        (Locker : in RW_Mutex_Access;
         File   : in Filename_Type;
         Line   : in Line_Number)
      is
         pragma Unreferenced (File, Line);

         Temp : RW_Mutex_Access := Locker;

         procedure Free is
            new Ada.Unchecked_Deallocation (RW_Mutex, RW_Mutex_Access);
      begin
         Free (Temp);
      end Dyn_Destroy;

      --------------
      -- Dyn_Lock --
      --------------

      procedure Dyn_Lock
        (Mode   : in Mode_Type;
         Locker : in RW_Mutex_Access;
         File   : in Filename_Type;
         Line   : in Line_Number)
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
         Finalized := True;
      end Finalize;

      -------------------------
      -- Get_Task_Identifier --
      -------------------------

      function Get_Task_Identifier return Task_Identifier is
         TA : constant Task_Identifiers.Attribute_Handle
           := Task_Identifiers.Reference;
      begin
         if TA.TID = 0 then
            Task_Id_Generator.Get_Task_Id (TA.TID);

            Ada.Task_Termination.Set_Specific_Handler
              (Ada.Task_Identification.Current_Task,
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

      procedure Lock (Mode : in Mode_Type; Locker : in out RW_Mutex) is
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
        (Mode : in Mode_Type;
         N    : in Lock_Index;
         File : in Filename_Type;
         Line : in Line_Number)
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
           (Cause : in Ada.Task_Termination.Cause_Of_Termination;
            T     : in Ada.Task_Identification.Task_Id;
            X     : in Ada.Exceptions.Exception_Occurrence)
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

   ------------
   -- TS_SSL --
   ------------

   protected body TS_SSL is

      -------------------------
      -- Clear_Session_Cache --
      -------------------------

      procedure Clear_Session_Cache is
      begin
         TSSL.SSL_CTX_flush_sessions (Context, C.long'Last);
      end Clear_Session_Cache;

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
         Exchange_Certificate : in Boolean;
         Session_Cache_Size   : in Positive)
      is
         type Meth_Func is access function return TSSL.SSL_Method;
         pragma Convention (C, Meth_Func);

         procedure Set_Quiet_Shutdown (Value : in Boolean := True);

         procedure Set_Certificate
           (Cert_Filename : in String; Key_Filename : in String);

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
           (Cert_Filename : in String; Key_Filename : in String)
         is
            use Interfaces.C;

            procedure File_Error (Prefix, Name : in String);
            --  Prefix is the type of file key or certificate that was expected
            pragma No_Return (File_Error);

            ----------------
            -- File_Error --
            ----------------

            procedure File_Error (Prefix, Name : in String) is
            begin
               raise Socket_Error with
                 Prefix & " file """ & Name & """ error." &
                 ASCII.LF & Error_Stack;
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

            if Certificate_Filename /= "" then
               Set_Certificate (Certificate_Filename, Key_Filename);
            end if;

            Set_Quiet_Shutdown;
            TS_SSL.Set_Session_Cache_Size (Session_Cache_Size);
         end if;
      end Initialize;

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

         Socket.IO := Net_IO;

         SSL_set_bio (Socket.SSL, Inside_IO, Inside_IO);
      end Set_IO;

      ----------------------------
      -- Set_Session_Cache_Size --
      ----------------------------

      procedure Set_Session_Cache_Size (Size : in Natural) is
      begin
         Error_If
           (TSSL.SSL_CTX_ctrl
              (Ctx  => Context,
               Cmd  => TSSL.SSL_CTRL_SET_SESS_CACHE_SIZE,
               Larg => C.int (Size),
               Parg => TSSL.Null_Pointer) = -1);
      end Set_Session_Cache_Size;

   end TS_SSL;

   ---------------------
   -- Verify_Callback --
   ---------------------

   function Verify_Callback
     (preverify_ok : in Integer; ctx : in System.Address) return Integer
   is
      pragma Unreferenced (preverify_ok, ctx);
   begin
      return 1;
   end Verify_Callback;

   -------------
   -- Version --
   -------------

   function Version (Build_Info : in Boolean := False) return String is
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
         R => System.Memory.Realloc'Address,
         F => System.Memory.Free'Address) = 0
   then
      raise Program_Error with "Could not set memory functions.";
   end if;

   TSSL.SSL_load_error_strings;
   TSSL.SSL_library_init;
   Locking.Initialize;
   Init_Random;
end AWS.Net.SSL;
