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
--  SSL.Set_Certificate.

with Ada.Calendar;
with Ada.Exceptions;
with Interfaces.C;
with System.Storage_Elements;

with AWS.Net.Std;
with SSL.Thin;

package body AWS.Net.SSL is

   use type Interfaces.C.int;
   use type System.Address;

   subtype NSST is Net.Std.Socket_Type;

   Private_Key : TSSL.RSA     := Null_Ptr;
   Context     : TSSL.SSL_Ctx := Null_Ptr;

   SSL_Initialized : Boolean := False;
   pragma Atomic (SSL_Initialized);

   procedure Error_If (Error  : in Boolean);
   pragma Inline (Error_If);
   --  Raises Socket_Error if Error is true. Attach the SSL error message

   procedure Set_FD (Socket : in out Socket_Type);
   --  Bind the SSL socket handle with the socket

   procedure Set_Read_Ahead (Socket : in Socket_Type; Value : in Boolean);

   function Error_Str (Code : in TSSL.Error_Code) return String;

   procedure Finalize;

   procedure Init_Random;

   -------------------
   -- Accept_Socket --
   -------------------

   procedure Accept_Socket
     (Socket     : in     Socket_Type;
      New_Socket :    out Net.Socket_Type'Class) is
   begin
      loop
         Net.Std.Accept_Socket
           (NSST (Socket.S.all), NSST (Socket_Type (New_Socket).S.all));

         Set_FD (Socket_Type (New_Socket));

         TSSL.SSL_Set_Accept_State (Socket_Type (New_Socket).SSL);

         exit when TSSL.SSL_Accept (Socket_Type (New_Socket).SSL) > 0;

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
      Free (Left.S);
      Left.S := Socket_Type (Right).S;
   end Assign;

   ----------
   -- Bind --
   ----------

   procedure Bind
     (Socket : in Socket_Type;
      Port   : in Natural;
      Host   : in String := "") is
   begin
      Net.Std.Bind (NSST (Socket.S.all), Port, Host);
   end Bind;

   -------------
   -- Connect --
   -------------

   procedure Connect
     (Socket   : in Socket_Type;
      Host     : in String;
      Port     : in Positive) is
   begin
      Net.Std.Connect (NSST (Socket.S.all), Host, Port);

      TSSL.SSL_Set_Connect_State (Socket.SSL);

      Error_If (TSSL.SSL_Connect (Socket.SSL) = -1);
   end Connect;

   --------------
   -- Error_If --
   --------------

   procedure Error_If (Error : in Boolean) is
      use Ada;
   begin
      if Error then
         Exceptions.Raise_Exception
           (Socket_Error'Identity, Error_Str (TSSL.Err_Get_Error));
      end if;
   end Error_If;

   ---------------
   -- Error_Str --
   ---------------

   function Error_Str (Code : in TSSL.Error_Code) return String is
      use Interfaces;
      Buffer : C.char_array := (0 .. 511 => Interfaces.C.nul);
   begin
      TSSL.Err_Error_String_N (Code, Buffer, Buffer'Length);
      return C.To_Ada (Buffer);
   end Error_Str;

   --------------
   -- Finalise --
   --------------

   procedure Finalize is
   begin
      TSSL.SSL_Ctx_Free (Context);
      Context := Null_Ptr;
   end Finalize;

   ----------
   -- Free --
   ----------

   procedure Free (Socket : in out Socket_Type) is
   begin
      if Socket.SSL /= Null_Ptr then
         TSSL.SSL_Free (Socket.SSL);
         Socket.SSL := Null_Ptr;
      end if;

      Net.Std.Free (NSST (Socket.S.all));
   end Free;

   ------------
   -- Get_FD --
   ------------

   function Get_FD (Socket : in Socket_Type) return Integer is
   begin
      return Net.Std.Get_FD (NSST (Socket.S.all));
   end Get_FD;

   ---------------
   -- Host_Name --
   ---------------

   function Host_Name return String is
   begin
      return Net.Std.Host_Name;
   end Host_Name;

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
      TSSL.Rand_Seed (Buf'Address, Buf'Length);
   end Init_Random;

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
        := (SSLv2          => TSSL.SSLv2_Method'Access,
            SSLv2_Server   => TSSL.SSLv2_Server_Method'Access,
            SSLv2_Client   => TSSL.SSLv2_Client_Method'Access,
            SSLv23         => TSSL.SSLv23_Method'Access,
            SSLv23_Server  => TSSL.SSLv23_Server_Method'Access,
            SSLv23_Client  => TSSL.SSLv23_Client_Method'Access,
            Tlsv1          => TSSL.Tlsv1_Method'Access,
            Tlsv1_Server   => TSSL.Tlsv1_Server_Method'Access,
            Tlsv1_Client   => TSSL.Tlsv1_Client_Method'Access,
            SSLv3          => TSSL.SSLv3_Method'Access,
            SSLv3_Server   => TSSL.SSLv3_Server_Method'Access,
            SSLv3_Client   => TSSL.SSLv3_Client_Method'Access);

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
           (TSSL.SSL_Ctx_Use_Privatekey_File
              (Ctx    => Context,
               File   => To_C (Key_File_Name),
               C_Type => TSSL.SSL_Filetype_Pem) = -1);
         --  ??? To_C memory leak

         Error_If
           (TSSL.SSL_Ctx_Use_Certificate_File
              (Ctx    => Context,
               File   => To_C (Cert_Filename),
               C_Type => TSSL.SSL_Filetype_Pem) = -1);
         --  ??? To_C memory leak

         Error_If
           (TSSL.SSL_Ctx_Check_Private_Key (Ctx => Context) = -1);

         if TSSL.SSL_Ctx_Ctrl
           (Ctx  => Context,
            Cmd  => TSSL.SSL_Ctrl_Need_Tmp_Rsa,
            Larg => 0,
            Parg => Null_Ptr) /= 0
         then
            Error_If
              (TSSL.SSL_Ctx_Ctrl
                 (Ctx  => Context,
                  Cmd  => TSSL.SSL_Ctrl_Set_Tmp_Rsa,
                  Larg => 0,
                  Parg => Private_Key) = -1);
         end if;
      end Set_Certificate;

      ------------------------
      -- Set_Quiet_Shutdown --
      ------------------------

      procedure Set_Quiet_Shutdown (Value : in Boolean := True) is
      begin
         TSSL.SSL_Ctx_Set_Quiet_Shutdown
           (Ctx  => Context,
            Mode => Boolean'Pos (Value));
      end Set_Quiet_Shutdown;

      -------------------------
      -- Set_Sess_Cache_Size --
      -------------------------

      procedure Set_Sess_Cache_Size (Value : in Natural) is
      begin
         Error_If
           (TSSL.SSL_Ctx_Ctrl
              (Ctx  => Context,
               Cmd  => TSSL.SSL_Ctrl_Set_Sess_Cache_Size,
               Larg => Interfaces.C.int (Value),
               Parg => Null_Ptr) = -1);
      end Set_Sess_Cache_Size;

   begin
      if not SSL_Initialized then

         if Context /= Null_Ptr then
            Finalize;
         end if;

         --  Initialize context

         Context := TSSL.SSL_Ctx_New (Methods (Security_Mode).all);
         Error_If (Context = Null_Ptr);

         --  Initialize private key

         Private_Key :=
           TSSL.Rsa_Generate_Key (Bits     => 512,
                                  E        => TSSL.Rsa_F4,
                                  Callback => null,
                                  Cb_Arg   => Null_Ptr);
         Error_If (Private_Key = Null_Ptr);

         Set_Certificate (Certificate_Filename, Key_Filename);

         Set_Quiet_Shutdown;
         Set_Sess_Cache_Size (16);

         SSL_Initialized := True;
      end if;
   end Initialize;

   ------------
   -- Listen --
   ------------

   procedure Listen
     (Socket     : in Socket_Type;
      Queue_Size : in Positive := 5) is
   begin
      Net.Std.Listen (NSST (Socket.S.all), Queue_Size);
   end Listen;

   ---------------
   -- Peer_Addr --
   ---------------

   function Peer_Addr (Socket : in Socket_Type) return String is
   begin
      return Net.Std.Peer_Addr (NSST (Socket.S.all));
   end Peer_Addr;

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
      Len := TSSL.SSL_Read (Socket.SSL, Buffer'Address, Buffer'Length);
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
      Error_If (TSSL.SSL_Write (Socket.SSL, Data'Address, Data'Length) = -1);
   end Send;

   ------------
   -- Set_FD --
   ------------

   procedure Set_FD (Socket : in out Socket_Type) is
   begin
      if Socket.SSL = Null_Ptr then
         Socket.SSL := TSSL.SSL_New (Context);
         Error_If (Socket.SSL = Null_Ptr);
      end if;

      Error_If
        (TSSL.SSL_Set_Fd
           (Socket.SSL,
            Interfaces.C.int (Get_FD (Socket))) = -1);
   end Set_FD;

   --------------------
   -- Set_Read_Ahead --
   --------------------

   procedure Set_Read_Ahead (Socket : in Socket_Type; Value : in Boolean)  is
   begin
      TSSL.SSL_Set_Read_Ahead (S => Socket.SSL, Yes => Boolean'Pos (Value));
   end Set_Read_Ahead;

   --------------
   -- Shutdown --
   --------------

   procedure Shutdown (Socket : in Socket_Type) is
   begin
      TSSL.SSL_Set_Shutdown
        (Socket.SSL, TSSL.SSL_SENT_SHUTDOWN + TSSL.SSL_RECEIVED_SHUTDOWN);
      Net.Std.Shutdown (NSST (Socket.S.all));
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
      Socket_Type (Sock.all).S := S;

      Set_FD (Socket_Type (Sock.all));

      return Sock;
   end Socket;

begin
   TSSL.SSL_Load_Error_Strings;
   TSSL.SSL_Library_Init;
   Init_Random;
end AWS.Net.SSL;
