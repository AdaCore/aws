------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2006-2018, AdaCore                     --
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

--  This is a dummy implementation used when AWS is built without SSL
--  support. Every use of this interface will raise the Program_Error
--  exception.

package body AWS.Net.SSL is

   type TS_SSL is new System.Address;

   Error_Message : constant String := "SSL not supported.";

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
     (Socket : Net.Socket_Type'Class; New_Socket : in out Socket_Type) is
   begin
      raise Program_Error with Error_Message;
   end Accept_Socket;

   --------------------------
   -- Add_Host_Certificate --
   --------------------------

   procedure Add_Host_Certificate
     (Config               : SSL.Config;
      Host                 : String;
      Certificate_Filename : String;
      Key_Filename         : String := "") is
   begin
      null;
   end Add_Host_Certificate;

   ------------------------
   -- Cipher_Description --
   ------------------------

   overriding function Cipher_Description
     (Socket : Socket_Type) return String is
   begin
      raise Program_Error with Error_Message;
      return "";
   end Cipher_Description;

   -------------
   -- Ciphers --
   -------------

   procedure Ciphers
     (Cipher : not null access procedure (Name : String)) is null;

   -------------------------
   -- Clear_Session_Cache --
   -------------------------

   procedure Clear_Session_Cache (Config : SSL.Config := Null_Config) is null;

   -------------
   -- Connect --
   -------------

   overriding procedure Connect
     (Socket : in out Socket_Type;
      Host   : String;
      Port   : Positive;
      Wait   : Boolean     := True;
      Family : Family_Type := Family_Unspec) is
   begin
      raise Program_Error with Error_Message;
   end Connect;

   ------------------
   -- Do_Handshake --
   ------------------

   procedure Do_Handshake (Socket : in out Socket_Type) is
   begin
      raise Program_Error with Error_Message;
   end Do_Handshake;

   ----------
   -- Free --
   ----------

   procedure Free (Key : in out Private_Key) is null;

   procedure Free (Session : in out Session_Type) is null;

   overriding procedure Free (Socket : in out Socket_Type) is
   begin
      raise Program_Error with Error_Message;
   end Free;

   -----------------
   -- Generate_DH --
   -----------------

   procedure Generate_DH is null;

   ------------------
   -- Generate_RSA --
   ------------------

   procedure Generate_RSA is null;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Config               : in out SSL.Config;
      Certificate_Filename : String;
      Security_Mode        : Method     := TLS;
      Priorities           : String     := "";
      Ticket_Support       : Boolean    := False;
      Key_Filename         : String     := "";
      Exchange_Certificate : Boolean    := False;
      Certificate_Required : Boolean    := False;
      Trusted_CA_Filename  : String     := "";
      CRL_Filename         : String     := "";
      Session_Cache_Size   : Natural    := 16#4000#) is
   begin
      raise Program_Error with Error_Message;
   end Initialize;

   -------------------------------
   -- Initialize_Default_Config --
   -------------------------------

   procedure Initialize_Default_Config
     (Certificate_Filename : String;
      Security_Mode        : Method     := TLS;
      Priorities           : String     := "";
      Ticket_Support       : Boolean    := False;
      Key_Filename         : String     := "";
      Exchange_Certificate : Boolean    := False;
      Certificate_Required : Boolean    := False;
      Trusted_CA_Filename  : String     := "";
      CRL_Filename         : String     := "";
      Session_Cache_Size   : Natural    := 16#4000#) is
   begin
      raise Program_Error with Error_Message;
   end Initialize_Default_Config;

   ----------
   -- Load --
   ----------

   function Load (Filename : String) return Private_Key is
   begin
      raise Program_Error with Error_Message;
      return (null record);
   end Load;

   ---------------
   -- Log_Error --
   ---------------

   procedure Log_Error (Text : String) is null;

   -------------
   -- Pending --
   -------------

   overriding function Pending
     (Socket : Socket_Type) return Stream_Element_Count is
   begin
      raise Program_Error with Error_Message;
      return 0;
   end Pending;

   -------------
   -- Receive --
   -------------

   overriding procedure Receive
     (Socket : Socket_Type;
      Data   : out Stream_Element_Array;
      Last   : out Stream_Element_Offset) is
   begin
      raise Program_Error with Error_Message;
   end Receive;

   -------------
   -- Release --
   -------------

   procedure Release (Config : in out SSL.Config) is
   begin
      raise Program_Error with Error_Message;
   end Release;

   -------------------
   -- Secure_Client --
   -------------------

   function Secure_Client
     (Socket : Net.Socket_Type'Class;
      Config : SSL.Config := Null_Config;
      Host   : String     := "") return Socket_Type
   is
      pragma Unreferenced (Socket, Config, Host);
      S : Socket_Type;
   begin
      raise Program_Error with Error_Message;
      return S;
   end Secure_Client;

   -------------------
   -- Secure_Server --
   -------------------

   function Secure_Server
     (Socket : Net.Socket_Type'Class;
      Config : SSL.Config := Null_Config) return Socket_Type
   is
      pragma Unreferenced (Socket, Config);
      S : Socket_Type;
   begin
      raise Program_Error with Error_Message;
      return S;
   end Secure_Server;

   ----------
   -- Send --
   ----------

   overriding procedure Send
     (Socket : Socket_Type;
      Data   : Stream_Element_Array;
      Last   : out Stream_Element_Offset) is
   begin
      raise Program_Error with Error_Message;
   end Send;

   --------------------------
   -- Session_Cache_Number --
   --------------------------

   function Session_Cache_Number
     (Config : SSL.Config := Null_Config) return Natural
   is
      pragma Unreferenced (Config);
   begin
      return 0;
   end Session_Cache_Number;

   ------------------
   -- Session_Data --
   ------------------

   function Session_Data (Socket : Socket_Type) return Session_Type is
      pragma Unreferenced (Socket);
   begin
      return Null_Session;
   end Session_Data;

   ----------------------
   -- Session_Id_Image --
   ----------------------

   function Session_Id_Image (Session : Session_Type) return String is
      pragma Unreferenced (Session);
   begin
      return "";
   end Session_Id_Image;

   function Session_Id_Image (Socket : Socket_Type) return String is
      pragma Unreferenced (Socket);
   begin
      return "";
   end Session_Id_Image;

   --------------------
   -- Session_Reused --
   --------------------

   function Session_Reused (Socket : Socket_Type) return Boolean is
      pragma Unreferenced (Socket);
   begin
      return False;
   end Session_Reused;

   ----------------
   -- Set_Config --
   ----------------

   procedure Set_Config
     (Socket : in out Socket_Type; Config : SSL.Config) is
   begin
      raise Program_Error with Error_Message;
   end Set_Config;

   ---------------
   -- Set_Debug --
   ---------------

   procedure Set_Debug
     (Level : Natural; Output : Debug_Output_Procedure := null) is null;

   ----------------------------
   -- Set_Session_Cache_Size --
   ----------------------------

   procedure Set_Session_Cache_Size
     (Size : Natural; Config : SSL.Config := Null_Config) is null;

   ----------------------
   -- Set_Session_Data --
   ----------------------

   procedure Set_Session_Data
     (Socket : in out Socket_Type; Data : Session_Type) is null;

   -------------------------
   -- Set_Verify_Callback --
   -------------------------

   procedure Set_Verify_Callback
     (Config : in out SSL.Config; Callback : System.Address) is
   begin
      raise Program_Error with Error_Message;
   end Set_Verify_Callback;

   --------------
   -- Shutdown --
   --------------

   overriding procedure Shutdown
     (Socket : Socket_Type; How : Shutmode_Type := Shut_Read_Write) is
   begin
      raise Program_Error with Error_Message;
   end Shutdown;

   ---------------
   -- Signature --
   ---------------

   function Signature
     (Ptr  : System.Address;
      Size : Interfaces.C.size_t;
      Key  : Private_Key;
      Hash : Hash_Method) return Stream_Element_Array
   is
      pragma Unreferenced (Ptr, Size, Key, Hash);
   begin
      return (1 .. 0 => <>);
   end Signature;

   -----------------
   -- Socket_Pair --
   -----------------

   overriding procedure Socket_Pair (S1, S2 : out Socket_Type) is
   begin
      raise Program_Error with Error_Message;
   end Socket_Pair;

   ---------------------------------
   -- Start_Parameters_Generation --
   ---------------------------------

   procedure Start_Parameters_Generation
     (DH      : Boolean;
      Logging : access procedure (Text : String) := null) is null;

   -------------
   -- Version --
   -------------

   function Version (Build_Info : Boolean := False) return String is
      pragma Unreferenced (Build_Info);
   begin
      return Error_Message;
   end Version;

end AWS.Net.SSL;
