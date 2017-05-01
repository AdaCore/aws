------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2012-2017, AdaCore                     --
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

with Ada.Calendar.Conversions;
with Ada.Calendar.Time_Zones;
with Ada.Unchecked_Conversion;

with Interfaces.C.Strings;

with AWS.Net.Log;
with AWS.Resources;

with SSL.Thin;

package body AWS.Net.SSL.Certificate.Impl is

   procedure Check_Error_Code
     (Code : C.int; Socket : access constant Socket_Type'Class);

   function Read
     (Socket : access constant Socket_Type'Class;
      Status : C.unsigned;
      X509   : TSSL.gnutls_x509_crt_t) return Object;
   --  Read certificate data

   ----------------------
   -- Check_Error_Code --
   ----------------------

   procedure Check_Error_Code
     (Code : C.int; Socket : access constant Socket_Type'Class)
   is
      use type C.int;
   begin
      if Code /= 0 then
         declare
            Error : constant String :=
                      C.Strings.Value (TSSL.gnutls_strerror (Code));
         begin
            if Socket = null then
               Log_Error (Error);
            else
               Net.Log.Error (Socket.all, Error);
            end if;

            raise Socket_Error with Error;
         end;
      end if;
   end Check_Error_Code;

   ---------
   -- Get --
   ---------

   function Get (Socket : Socket_Type) return Object is
      use type C.unsigned;
      use type System.Address;
      use type TSSL.a_gnutls_datum_t;

      Status    : aliased C.unsigned;
      List_Size : aliased C.unsigned;
      Datum     : constant TSSL.a_gnutls_datum_t :=
                    TSSL.gnutls_certificate_get_peers
                      (Socket.SSL, List_Size'Access);
      Cert      : aliased TSSL.gnutls_x509_crt_t;
      Result    : Object;
   begin
      if List_Size = 0 or else Datum = null then
         return Undefined;
      end if;

      Check_Error_Code
        (TSSL.gnutls_certificate_verify_peers2 (Socket.SSL, Status'Access),
         Socket'Access);

      Check_Error_Code
        (TSSL.gnutls_x509_crt_init (Cert'Access), Socket'Access);

      Check_Error_Code
        (TSSL.gnutls_x509_crt_import
           (Cert, Datum, TSSL.GNUTLS_X509_FMT_DER),
         Socket'Access);

      Result := Read (Socket'Access, Status, Cert);

      TSSL.gnutls_x509_crt_deinit (Cert);

      return Result;
   end Get;

   ----------
   -- Load --
   ----------

   function Load (Filename : String) return Object is
      Data : Datum_Type := Load_File (Filename);
      Cert : aliased TSSL.gnutls_x509_crt_t;
      Res  : Object;
   begin
      Check_Error_Code (TSSL.gnutls_x509_crt_init (Cert'Access), null);

      Check_Error_Code
        (TSSL.gnutls_x509_crt_import
           (Cert, Data.Datum'Unchecked_Access, TSSL.GNUTLS_X509_FMT_PEM),
         null);

      Utils.Unchecked_Free (Data.Data);

      Res := Read (0, Cert);

      TSSL.gnutls_x509_crt_deinit (Cert);

      return Res;
   end Load;

   ---------------
   -- Load_File --
   ---------------

   function Load_File (Filename : String) return Datum_Type is
      use AWS.Resources;

      Result : Datum_Type;
      Last   : Stream_Element_Offset;
      File   : File_Type;
   begin
      Open (File, Name => Filename);

      Result.Data := new Stream_Element_Array
                           (1 .. Stream_Element_Offset (File_Size (Filename)));

      Read (File, Result.Data.all, Last);

      if not End_Of_File (File) then
         Close (File);
         raise Program_Error with "not end of file";
      end if;

      Close (File);

      if Last < Result.Data'Last then
         raise Program_Error with Last'Img & Result.Data'Last'Img;
      end if;

      Result.Datum.size := Result.Data'Length;
      Result.Datum.data := Result.Data.all'Address;

      return Result;
   end Load_File;

   ----------
   -- Read --
   ----------

   function Read
     (Socket : access constant Socket_Type'Class;
      Status : C.unsigned;
      X509   : TSSL.gnutls_x509_crt_t) return Object
   is
      use type Ada.Calendar.Time;
      use type C.int;
      use type C.size_t;
      use type C.unsigned;

      function To_Time (tv_sec : TSSL.time_t) return Calendar.Time with Inline;
      --  Convert a time_t to an Ada duration

      function To_Hex
        (Bin : Stream_Element_Array; Len : C.size_t) return Unbounded_String;
      --  Convert Bin to an hex string

      function To_Ada
        (Item : C.char_array; Length : C.size_t) return Unbounded_String
        with Inline;

      function To_DER return Binary_Holders.Holder;

      ------------
      -- To_Ada --
      ------------

      function To_Ada
        (Item : C.char_array; Length : C.size_t) return Unbounded_String is
      begin
         return To_Unbounded_String (C.To_Ada (Item (1 .. Length), False));
      end To_Ada;

      ------------
      -- To_DER --
      ------------

      function To_DER return Binary_Holders.Holder is
         Datum  : aliased TSSL.gnutls_datum_t;
         Result : Binary_Holders.Holder;
      begin
         Check_Error_Code
           (TSSL.gnutls_x509_crt_export2
              (X509, TSSL.GNUTLS_X509_FMT_DER, Datum'Unchecked_Access),
            Socket);

         declare
            type Array_Access is
               access all Stream_Element_Array
                            (1 .. Stream_Element_Offset (Datum.size));
            function To_Array is
               new Ada.Unchecked_Conversion
                     (TSSL.a_unsigned_char_t, Array_Access);
         begin
            Result := Binary_Holders.To_Holder (To_Array (Datum.data).all);
            TSSL.gnutls_free (Datum.data);
         end;

         return Result;
      end To_DER;

      ------------
      -- To_Hex --
      ------------

      function To_Hex
        (Bin : Stream_Element_Array; Len : C.size_t) return Unbounded_String
      is
         R : Unbounded_String;
      begin
         for K in Bin'First .. Bin'First + Stream_Element_Offset (Len) - 1 loop
            declare
               P : constant Natural := Natural (Bin (K));
            begin
               --  Skip leading zero

               if P = 0 and then K = Bin'First then
                  if Len = 1 then
                     Append (R, '0');
                  end if;
               else
                  Append (R, Utils.Hex (P, 2));
               end if;
            end;
         end loop;

         return R;
      end To_Hex;

      -------------
      -- To_Time --
      -------------

      function To_Time (tv_sec : TSSL.time_t) return Calendar.Time is
         use Ada.Calendar;
         DT : constant Time := Conversions.To_Ada_Time (C.long (tv_sec));
      begin
         return DT - Duration (Time_Zones.UTC_Time_Offset (DT)) * 60.0;
      end To_Time;

      Buffer_Size : constant := 256;
      --  Buffer size for the subject and issuer

      RC       : C.int;
      Subject  : aliased C.char_array := (1 .. Buffer_Size => C.nul);
      Subj_Len : aliased C.size_t := Buffer_Size;
      Issuer   : aliased C.char_array := (1 .. Buffer_Size => C.nul);
      Iss_Len  : aliased C.size_t := Buffer_Size;
      CN       : aliased C.char_array := (1 .. Buffer_Size => C.nul);
      CN_Len   : aliased C.size_t := Buffer_Size;

      T_Activation, T_Expiration : TSSL.time_t;

      Serial     : aliased Stream_Element_Array := (1 .. Buffer_Size => 0);
      Serial_Len : aliased C.size_t := Buffer_Size;

   begin
      Check_Error_Code
        (TSSL.gnutls_x509_crt_get_dn
           (X509,
            C.Strings.To_Chars_Ptr (Subject'Unchecked_Access),
            Subj_Len'Access),
         Socket);

      RC := TSSL.gnutls_x509_crt_get_dn_by_oid
              (cert     => X509,
               oid      => TSSL.GNUTLS_OID_X520_COMMON_NAME'Access,
               indx     => 0,
               raw_flag => 0,
               buf      => CN'Address,
               buf_size => CN_Len'Access);

      if RC = TSSL.GNUTLS_E_REQUESTED_DATA_NOT_AVAILABLE then
         CN_Len := 0;
      else
         Check_Error_Code (RC, Socket);
      end if;

      Check_Error_Code
        (TSSL.gnutls_x509_crt_get_issuer_dn
           (X509,
            C.Strings.To_Chars_Ptr (Issuer'Unchecked_Access),
            Iss_Len'Access),
         Socket);

      Check_Error_Code
        (TSSL.gnutls_x509_crt_get_serial
           (X509,
            Serial'Address,
            Serial_Len'Access),
         Socket);

      T_Activation := TSSL.gnutls_x509_crt_get_activation_time (X509);
      T_Expiration := TSSL.gnutls_x509_crt_get_expiration_time (X509);

      return (Verified      => Status = 0,
              Status        => Long_Integer (Status),
              Common_Name   => To_Ada (CN, CN_Len),
              Subject       => To_Ada (Subject, Subj_Len),
              Issuer        => To_Ada (Issuer, Iss_Len),
              Serial_Number => To_Hex (Serial, Serial_Len),
              DER           => To_DER,
              Activation    => To_Time (T_Activation),
              Expiration    => To_Time (T_Expiration));
   end Read;

   ----------
   -- Read --
   ----------

   function Read
     (Status : C.unsigned; X509 : TSSL.gnutls_x509_crt_t) return Object is
   begin
      return Read (null, Status, X509);
   end Read;

   -------------------
   -- Status_String --
   -------------------

   function Status_String (Status : C.long) return String is
      use type C.unsigned_long;

      function To_Value is new Ada.Unchecked_Conversion
        (TSSL.gnutls_certificate_status_t, C.unsigned);

      Status_Value : constant C.unsigned_long := C.unsigned_long (Status);
      Message      : Unbounded_String;

   begin
      --  The GNUTLS status is an unsigned long bitwise or’d
      --  gnutls_certificate_status_t values or zero if the certificate is
      --  trusted.

      for S in TSSL.gnutls_certificate_status_t loop
         if (Status_Value and C.unsigned_long (To_Value (S))) /= 0 then
            if Message /= Null_Unbounded_String then
               Append (Message, " ");
            end if;

            Append (Message, TSSL.gnutls_certificate_status_t'Image (S));
         end if;
      end loop;

      return To_String (Message);
   end Status_String;

end AWS.Net.SSL.Certificate.Impl;
