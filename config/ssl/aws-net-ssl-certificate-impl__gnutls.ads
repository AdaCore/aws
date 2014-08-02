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

with Interfaces.C;

with AWS.Utils;

package AWS.Net.SSL.Certificate.Impl is

   use Interfaces;

   function Get (Socket : Socket_Type) return Object;
   --  Read certificate from peer socket. Note that GNUTLS sends the client
   --  certificate only of the server CA corresponds. This means that if no
   --  CA is used this routine will return an empty certificate when trying to
   --  retreive the client certificate.

   function Load (Filename : String) return Object;
   --  Load certificate data from file

   function Read
     (Status : C.unsigned;
      X509   : Standard.SSL.Thin.gnutls_x509_crt_t) return Object;
   --  Read certificate data

   function Status_String (Status : C.long) return String;
   --  Returns the message string for status

   type Datum_Type is record
      Datum : aliased Standard.SSL.Thin.gnutls_datum_t;
      Data  : Utils.Stream_Element_Array_Access;
   end record;

   function Load_File (Filename : String) return Datum_Type;

end AWS.Net.SSL.Certificate.Impl;
