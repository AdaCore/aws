------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2003-2015, AdaCore                     --
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

with Ada.Calendar;

private with Ada.Containers.Indefinite_Holders;
private with Ada.Strings.Unbounded;
private with AWS.Utils;

package AWS.Net.SSL.Certificate is

   type Object is private;

   Undefined : constant Object;

   function Get (Socket : Socket_Type) return Object;
   --  Returns the certificate used by the SSL

   function Common_Name (Certificate : Object) return String with Inline;
   --  Returns the certificate's common name

   function Subject (Certificate : Object) return String with Inline;
   --  Returns the certificate's subject

   function Issuer (Certificate : Object) return String with Inline;
   --  Returns the certificate's issuer

   function Serial_Number (Certificate : Object) return String with Inline;
   --  Returns the certificate's serial number

   function DER (Certificate : Object) return Stream_Element_Array with Inline;
   --  Returns all certificate's data in DER format

   overriding function "=" (Left, Right : Object) return Boolean with Inline;
   --  Compare 2 certificates

   function Load (Filename : String) return Object;
   --  Load certificate from file in PEM format

   function Activation_Time (Certificate : Object) return Calendar.Time
     with Inline;
   --  Certificate validity starting date

   function Expiration_Time (Certificate : Object) return Calendar.Time
     with Inline;
   --  Certificate validity ending date

   function Verified (Certificate : Object) return Boolean with Inline;
   --  Returns True if the certificate has already been verified, this is
   --  mostly interresting when used from the Verify_Callback below. If this
   --  routine returns True it means that the certificate has already been
   --  properly checked. If checked the certificate can be trusted and the
   --  Verify_Callback should return True also. If it is False it is up to
   --  the application to check the certificate into the Verify_Callback and
   --  returns the appropriate status.

   function Status (Certificate : Object) return Long_Integer with Inline;
   --  Returns the status for the certificate. This is to be used inside the
   --  verify callback to know why the certificate has been rejected.

   function Status_Message (Certificate : Object) return String;
   --  Returns the error message for the current certificate status (as
   --  returned by Status above).

   --
   --  Client verification support
   --

   type Verify_Callback is
     access function (Cert : SSL.Certificate.Object) return Boolean;
   --  Client certificate verification callback, must return True if Cert can
   --  be accepted or False otherwise. Such callback should generally return
   --  the value returned by Verified above.

   procedure Set_Verify_Callback
     (Config : in out SSL.Config; Callback : Verify_Callback);
   --  Register the callback to use to verify client's certificates

   type Password_Callback is
     access function (Certificate_Filename : String) return String;
   --  Callback to get password for signed server's keys. An empty string
   --  must be returned if the password is unknown or the certificate isn't
   --  signed.

   procedure Set_Password_Callback (Callback : Password_Callback);
   --  Set the password callback

   function Get_Password (Certificate_Filename : String) return String;
   --  Request a password for the giver certificate. The default
   --  implementation just returns an empty string.

private

   use Ada.Strings.Unbounded;

   package Binary_Holders is
     new Ada.Containers.Indefinite_Holders (Stream_Element_Array);

   type Object is record
      Verified      : Boolean;
      Status        : Long_Integer;
      Common_Name   : Unbounded_String;
      Subject       : Unbounded_String;
      Issuer        : Unbounded_String;
      Serial_Number : Unbounded_String;
      DER           : Binary_Holders.Holder;
      Activation    : Calendar.Time;
      Expiration    : Calendar.Time;
   end record;

   Undefined : constant Object :=
                 (False, 0, Null_Unbounded_String, Null_Unbounded_String,
                  Null_Unbounded_String, Null_Unbounded_String,
                  Binary_Holders.Empty_Holder,
                  Utils.AWS_Epoch, Utils.AWS_Epoch);

   function Common_Name (Certificate : Object) return String is
     (To_String (Certificate.Common_Name));

   function Subject (Certificate : Object) return String is
     (To_String (Certificate.Subject));

   function Issuer (Certificate : Object) return String is
     (To_String (Certificate.Issuer));

   function Serial_Number (Certificate : Object) return String is
     (To_String (Certificate.Serial_Number));

   function Activation_Time (Certificate : Object) return Calendar.Time is
     (Certificate.Activation);

   function Expiration_Time (Certificate : Object) return Calendar.Time is
     (Certificate.Expiration);

   function Verified (Certificate : Object) return Boolean is
     (Certificate.Verified);

   function Status (Certificate : Object) return Long_Integer is
     (Certificate.Status);

   function DER (Certificate : Object) return Stream_Element_Array is
     (if Certificate.DER.Is_Empty then (1 .. 0 => <>)
      else Certificate.DER.Element);

   overriding function "=" (Left, Right : Object) return Boolean is
     (Binary_Holders."=" (Left.DER, Right.DER));

end AWS.Net.SSL.Certificate;
