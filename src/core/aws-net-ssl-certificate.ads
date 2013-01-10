------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2003-2013, AdaCore                     --
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

with Ada.Calendar;
with Ada.Strings.Unbounded;

private with AWS.Utils;

package AWS.Net.SSL.Certificate is

   use Ada.Strings.Unbounded;

   type Object is private;

   Undefined : constant Object;

   function Get (Socket : Socket_Type) return Object;
   --  Returns the certificate used by the SSL

   function Subject (Certificate : Object) return String;
   --  Returns the certificate's subject

   function Issuer (Certificate : Object) return String;
   --  Returns the certificate's issuer

   function Serial_Number (Certificate : Object) return String;
   --  Returns the certificate's serial number

   function Activation_Time (Certificate : Object) return Calendar.Time;
   --  Certificate validity starting date

   function Expiration_Time (Certificate : Object) return Calendar.Time;
   --  Certificate validity ending date

   function Verified (Certificate : Object) return Boolean;
   --  Returns True if the certificate has already been verified, this is
   --  mostly interresting when used from the Verify_Callback below. If this
   --  routine returns True it means that the certificate has already been
   --  properly checked. If checked the certificate can be trusted and the
   --  Verify_Callback should return True also. If it is False it is up to
   --  the application to check the certificate into the Verify_Callback and
   --  returns the appropriate status.

   function Status (Certificate : Object) return Long_Integer;
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

private

   type Object is record
      Verified      : Boolean;
      Status        : Long_Integer;
      Subject       : Unbounded_String;
      Issuer        : Unbounded_String;
      Serial_Number : Unbounded_String;
      Activation    : Calendar.Time;
      Expiration    : Calendar.Time;
   end record;

   Undefined : constant Object :=
                 (False, 0, Null_Unbounded_String, Null_Unbounded_String,
                  Null_Unbounded_String, Utils.AWS_Epoch, Utils.AWS_Epoch);

end AWS.Net.SSL.Certificate;
