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

with Ada.Calendar.Conversions;
with Interfaces.C.Strings;

with AWS.Net.Log;
with SSL.Thin;

package body AWS.Net.SSL.Certificate.Impl is

   use Interfaces;

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
            if Socket /= null then
               Net.Log.Error (Socket.all, Error);
            end if;
            Ada.Exceptions.Raise_Exception (Socket_Error'Identity, Error);
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
        (TSSL.gnutls_x509_crt_init (Cert'Access), Socket'Access);

      Check_Error_Code
        (TSSL.gnutls_x509_crt_import
           (Cert, Datum.all, TSSL.GNUTLS_X509_FMT_DER),
         Socket'Access);

      Result := Read (Socket'Access, 2, Cert);

      TSSL.gnutls_x509_crt_deinit (Cert);

      return Result;
   end Get;

   ----------
   -- Read --
   ----------

   function Read
     (Socket : access constant Socket_Type'Class;
      Status : C.unsigned;
      X509   : TSSL.gnutls_x509_crt_t) return Object
   is
      use type C.unsigned;

      function To_Time (tv_sec : TSSL.time_t) return Calendar.Time;
      pragma Inline (To_Time);
      --  Convert a time_t to an Ada duration

      -------------
      -- To_Time --
      -------------

      function To_Time (tv_sec : TSSL.time_t) return Calendar.Time is
         use Ada;
      begin
         return Calendar.Conversions.To_Ada_Time (C.long (tv_sec));
      end To_Time;

      Buffer_Size : constant := 256;
      --  Buffer size for the subject and issuer

      Subject  : aliased C.char_array := (1 .. Buffer_Size => C.nul);
      Subj_Len : aliased C.size_t := Buffer_Size;
      Issuer   : aliased C.char_array := (1 .. Buffer_Size => C.nul);
      Iss_Len  : aliased C.size_t := Buffer_Size;

      T_Activation, T_Expiration : TSSL.time_t;
   begin
      Check_Error_Code
        (TSSL.gnutls_x509_crt_get_dn
           (X509,
            C.Strings.To_Chars_Ptr (Subject'Unchecked_Access),
            Subj_Len'Access),
         Socket);

      Check_Error_Code
        (TSSL.gnutls_x509_crt_get_issuer_dn
           (X509,
            C.Strings.To_Chars_Ptr (Issuer'Unchecked_Access),
            Iss_Len'Access),
         Socket);

      T_Activation := TSSL.gnutls_x509_crt_get_activation_time (X509);
      T_Expiration := TSSL.gnutls_x509_crt_get_expiration_time (X509);

      return (Verified   => Status = 0,
              Subject    => To_Unbounded_String
                              (C.To_Ada (Subject (1 .. Subj_Len), False)),
              Issuer     => To_Unbounded_String
                              (C.To_Ada (Issuer (1 .. Iss_Len), False)),
              Activation => To_Time (T_Activation),
              Expiration => To_Time (T_Expiration));
   end Read;

   ----------
   -- Read --
   ----------

   function Read
     (Status : C.unsigned; X509 : TSSL.gnutls_x509_crt_t)
      return Object
   is
   begin
      return Read (null, Status, X509);
   end Read;

end AWS.Net.SSL.Certificate.Impl;
