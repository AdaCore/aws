------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                            Copyright (C) 2002                            --
--                               ACT-Europe                                 --
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

--  $Id$:

package body AWS.LDAP.Thin is

   ----------
   -- Item --
   ----------

   function Item
     (Set   : in Attribute_Set_Access;
      Index : in C.int)
      return chars_ptr
   is
      use type C.size_t;
   begin
      return Set (C.size_t'First + C.size_t (Index) - 1);
   end Item;

   --------------------
   -- LDAP_API_ERROR --
   --------------------

   function LDAP_API_ERROR (n : in Return_Code) return Boolean is
   begin
      return n in API_ERROR;
   end LDAP_API_ERROR;

   ---------------------
   -- LDAP_API_RESULT --
   ---------------------

   function LDAP_API_RESULT (n : in Return_Code) return Boolean is
      use type C.int;
   begin
      return n = 0 or else LDAP_API_ERROR (n);
   end LDAP_API_RESULT;

   ---------------------
   -- LDAP_ATTR_ERROR --
   ---------------------

   function LDAP_ATTR_ERROR (n : in Return_Code) return Boolean is
   begin
      return n in ATTR_ERROR;
   end LDAP_ATTR_ERROR;

   ---------------------
   -- LDAP_NAME_ERROR --
   ---------------------

   function LDAP_NAME_ERROR (n : in Return_Code) return Boolean is
   begin
      return n in NAME_ERROR;
   end LDAP_NAME_ERROR;

   -------------------------
   -- LDAP_SECURITY_ERROR --
   -------------------------

   function LDAP_SECURITY_ERROR (n : in Return_Code) return Boolean is
   begin
      return n in SECURITY_ERROR;
   end LDAP_SECURITY_ERROR;

   ------------------------
   -- LDAP_SERVICE_ERROR --
   ------------------------

   function LDAP_SERVICE_ERROR (n : in Return_Code) return Boolean is
   begin
      return n in SERVICE_ERROR;
   end LDAP_SERVICE_ERROR;

   -----------------------
   -- LDAP_UPDATE_ERROR --
   -----------------------

   function LDAP_UPDATE_ERROR (n : in Return_Code) return Boolean is
   begin
      return n in UPDATE_ERROR;
   end LDAP_UPDATE_ERROR;

end AWS.LDAP.Thin;
