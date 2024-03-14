------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2012-2014, AdaCore                     --
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

with Ada.Strings.Fixed;
with Interfaces.C.Strings;
with System;

package body AWS.Net.SSL.Certificate.Impl is

   use type C.int;
   use type TSSL.X509;

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

   ---------
   -- Get --
   ---------

   function Get (Socket : Socket_Type) return Object is
      use type System.Address;

      X509   : constant TSSL.X509 :=
                 TSSL.SSL_get_peer_certificate (Socket.SSL);
      Result : Object;
   begin
      Result := Read (C.int (TSSL.SSL_get_verify_result (Socket.SSL)), X509);
      TSSL.X509_free (X509);
      return Result;
   end Get;

   ----------
   -- Load --
   ----------

   function Load (Filename : String) return Object is
      IO     : constant TSSL.BIO_Access := TSSL.BIO_new (TSSL.BIO_s_file);
      Name   : aliased C.char_array := C.To_C (Filename);
      X509   : aliased TSSL.X509 := TSSL.Null_X509;
      Result : Object;
   begin
      if TSSL.BIO_read_filename
           (IO, C.Strings.To_Chars_Ptr (Name'Unchecked_Access)) = 0
        or else TSSL.PEM_read_bio_X509
                  (IO, X509'Access, null, TSSL.Null_Pointer)
                = TSSL.Null_X509
      then
         TSSL.X509_free (X509);
         TSSL.BIO_free (IO);

         raise Constraint_Error with
            "Certificate file """ & Filename & """ error." & ASCII.LF
            & Error_Stack;
      end if;

      TSSL.BIO_free (IO);

      Result := Read (0, X509);

      TSSL.X509_free (X509);

      return Result;
   end Load;

   ----------
   -- Read --
   ----------

   function Read (Status : C.int; X509 : TSSL.X509) return Object is

      use type C.Strings.chars_ptr;

      Subj : TSSL.X509_NAME;

      function NAME_oneline (Name : TSSL.X509_NAME) return Unbounded_String;

      function Common_Name (Name : TSSL.X509_NAME) return Unbounded_String;

      function To_Time
        (tm : access constant TSSL.ASN1_UTCTIME) return Calendar.Time;
      --  Convert a ASN1_UTCTIME to an Ada time
      --
      --  The format are either:
      --
      --   YYMMDDhhmmssZ
      --   YYMMDDhhmmss+hh'mm'
      --   YYMMDDhhmmss-hh'mm'

      function To_String
        (Number : access constant TSSL.ASN1_INTEGER) return Unbounded_String;
      --  Returns the string value for Number of the empty string if not
      --  defined.

      function To_Ada (Item : access constant TSSL.ASN1_STRING) return String;

      function To_DER return Binary_Holders.Holder;

      -----------------
      -- Common_Name --
      -----------------

      function Common_Name (Name : TSSL.X509_NAME) return Unbounded_String is
         use TSSL;
         Idx : constant C.int :=
                 X509_NAME_get_index_by_NID (Name, NID_commonName, -1);
      begin
         if Idx = -1 then
            return Null_Unbounded_String;
         end if;

         return To_Unbounded_String
                  (To_Ada
                     (X509_NAME_ENTRY_get_data
                        (X509_NAME_get_entry (Name, Idx))));
      end Common_Name;

      ------------------
      -- NAME_oneline --
      ------------------

      function NAME_oneline (Name : TSSL.X509_NAME) return Unbounded_String is
         use TSSL;
         IO : constant BIO_Access := BIO_new (BIO_s_mem);
         RC : Interfaces.C.int;
      begin
         RC := X509_NAME_print_ex
           (Output => IO,
            Name   => Name,
            Indent => 0,
            flags  => XN_FLAG_RFC2253_DIRECT);

         declare
            use Strings.Fixed;
            Result : String (1 .. Natural (BIO_pending (IO)));
            EMail  : constant String := ",emailAddress=";
            Idx    : Natural;
         begin
            if BIO_read (IO, Result'Address, Result'Length) /= Result'Length
              or else RC /= Result'Length
            then
               raise Constraint_Error;
            end if;

            BIO_free (IO);

            Idx := Index (Result, EMail);

            if Idx = 0 then
               return To_Unbounded_String (Result);
            end if;

            --  Make the certificate output like in GNUTLS

            return To_Unbounded_String (Replace_Slice
                     (Result, Idx, Idx + EMail'Length - 1, ",EMAIL="));
         end;
      end NAME_oneline;

      ------------
      -- To_Ada --
      ------------

      function To_Ada
        (Item : access constant TSSL.ASN1_STRING) return String is
      begin
         return C.Strings.Value (Item.data, C.size_t (Item.length));
      end To_Ada;

      ------------
      -- To_DER --
      ------------

      function To_DER return Binary_Holders.Holder is
         DER : aliased Stream_Element_Array
                         (1 .. Stream_Element_Offset
                                 (TSSL.i2d_X509 (X509, null)));
         DP : aliased TSSL.Pointer := DER'Address;
      begin
         if TSSL.i2d_X509 (X509, DP'Access) /= DER'Length then
            raise Socket_Error with Error_Stack;
         end if;

         return Binary_Holders.To_Holder (DER);
      end To_DER;

      ---------------
      -- To_String --
      ---------------

      function To_String
        (Number : access constant TSSL.ASN1_INTEGER) return Unbounded_String is
      begin
         if Number = null or else Number.data = C.Strings.Null_Ptr then
            return Null_Unbounded_String;

         else
            declare
               N : TSSL.BIGNUM;
               R : C.Strings.chars_ptr;
               V : Unbounded_String;
            begin
               N := TSSL.ASN1_INTEGER_to_BN (Number, null);
               R := TSSL.BN_bn2hex (N);
               V := To_Unbounded_String (C.Strings.Value (R));

               TSSL.BN_free (N);
               C.Strings.Free (R);

               return V;
            end;
         end if;
      end To_String;

      -------------
      -- To_Time --
      -------------

      function To_Time
        (tm : access constant TSSL.ASN1_UTCTIME) return Calendar.Time is
      begin
         if tm.data = C.Strings.Null_Ptr then
            return Utils.AWS_Epoch;

         else
            declare
               Value : constant String := To_Ada (tm);
               F     : constant Positive := Value'First;
               Year  : constant Calendar.Year_Number :=
                         2000 + Natural'Value (Value (F .. F + 1));
               Month : constant Calendar.Month_Number :=
                         Calendar.Month_Number'Value (Value (F + 2 .. F + 3));
               Day   : constant Calendar.Day_Number :=
                         Calendar.Day_Number'Value (Value (F + 4 .. F + 5));
               Hour  : constant Calendar.Day_Duration :=
                         Calendar.Day_Duration'Value (Value (F + 6 .. F + 7));
               Min   : constant Calendar.Day_Duration :=
                         Calendar.Day_Duration'Value (Value (F + 8 .. F + 9));
               Sec   : constant Calendar.Day_Duration :=
                         Calendar.Day_Duration'Value
                           (Value (F + 10 .. F + 11));
            begin
               return Calendar.Time_Of
                 (Year, Month, Day, Hour * 3_600.0 + Min * 60.0 + Sec);
            end;
         end if;
      exception
         when others =>
            --  We really do not want to fail in this procedure, if the date is
            --  not properly formatted just return the Epoch.
            return Utils.AWS_Epoch;
      end To_Time;

      T_Activation, T_Expiration : access constant TSSL.ASN1_UTCTIME;

   begin
      if X509 = TSSL.Null_X509 then
         return Undefined;

      else
         T_Activation := TSSL.X509_get_notBefore (X509);
         T_Expiration := TSSL.X509_get_notAfter (X509);

         --  Get Subj before result construction because CN detecting inside

         Subj := TSSL.X509_get_subject_name (X509);

         return
           (Verified      => Status = 0,
            Status        => Long_Integer (Status),
            Common_Name   => Common_Name (Subj),
            Subject       => NAME_oneline (Subj),
            Issuer        => NAME_oneline (TSSL.X509_get_issuer_name (X509)),
            Serial_Number => To_String (TSSL.X509_get_serialNumber (X509)),
            DER           => To_DER,
            Activation    => To_Time (T_Activation),
            Expiration    => To_Time (T_Expiration));
      end if;
   end Read;

   -------------------
   -- Status_String --
   -------------------

   function Status_String (Status : C.long) return String is
   begin
      return C.Strings.Value (TSSL.X509_verify_cert_error_string (Status));
   end Status_String;

end AWS.Net.SSL.Certificate.Impl;
