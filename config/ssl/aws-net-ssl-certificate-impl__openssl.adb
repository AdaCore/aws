------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                       Copyright (C) 2012, AdaCore                        --
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

   ---------
   -- Get --
   ---------

   function Get (Socket : Socket_Type) return Object is
      use type System.Address;

      X509   : constant TSSL.X509 :=
                 TSSL.SSL_get_peer_certificate (Socket.SSL);
      Result : Object;
   begin
      Result := Read (X509);
      TSSL.X509_free (X509);
      return Result;
   end Get;

   ----------
   -- Read --
   ----------

   function Read (X509 : TSSL.X509) return Object is

      use Interfaces;

      use type C.Strings.chars_ptr;
      use type TSSL.Pointer;

      function NAME_oneline (Name : TSSL.X509_Name) return String;
      --  Return the value for Name

      function To_Time
        (tm : access constant TSSL.ASN1_UTCTIME) return Calendar.Time;
      --  Convert a ASN1_UCTIME to an Ada time
      --
      --  The format are either:
      --
      --   YYMMDDhhmmssZ
      --   YYMMDDhhmmss+hh'mm'
      --   YYMMDDhhmmss-hh'mm'

      ------------------
      -- NAME_oneline --
      ------------------

      function NAME_oneline (Name : TSSL.X509_Name) return String is
         use TSSL;
         use type Interfaces.C.int;
         IO : constant BIO_Access := BIO_new (BIO_s_mem);
         RC : Interfaces.C.int;
      begin
         RC := X509_NAME_print_ex
           (Output => IO,
            Name   => Name,
            Indent => 0,
            flags  => XN_FLAG_RFC2253_DIRECT);

         declare
            use Ada.Strings.Fixed;
            Result : String (1 .. Natural (BIO_ctrl (IO, BIO_CTRL_PENDING)));
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
               return Result;
            end if;

            --  Make the certificate output like in GNUTLS

            return Replace_Slice
              (Result, Idx, Idx + EMail'Length - 1, ",EMAIL=");
         end;
      end NAME_oneline;

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
               Value : constant String :=
                         C.Strings.Value (tm.data, C.size_t (tm.length));
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
      if X509 = TSSL.Null_Pointer then
         return Undefined;

      else
         T_Activation := TSSL.X509_get_notBefore (X509);
         T_Expiration := TSSL.X509_get_notAfter (X509);

         return
           (Subject    => To_Unbounded_String
              (NAME_oneline (TSSL.X509_get_subject_name (X509))),
            Issuer     => To_Unbounded_String
              (NAME_oneline (TSSL.X509_get_issuer_name (X509))),
            Activation => To_Time (T_Activation),
            Expiration => To_Time (T_Expiration));
      end if;
   end Read;

end AWS.Net.SSL.Certificate.Impl;
