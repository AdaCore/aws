------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2002-2017, AdaCore                     --
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

package body AWS.LDAP.Thin is

   ----------
   -- Item --
   ----------

   function Item
     (Set   : Constr_Ber_Val_Array_Access;
      Index : C.int) return C.char_array
   is
      use type C.size_t;

      function Value (Item : Ber_Val_Access) return C.char_array;
      --  Get value from Item pointer and return a char_array

      -----------
      -- Value --
      -----------

      function Value (Item : Ber_Val_Access) return C.char_array is
         Result : C.char_array (0 .. C.size_t (Item.BV_Len) - 1);
      begin
         for I in Result'Range loop
            Result (I) := Item.BV_Val (I);
         end loop;

         return Result;
      end Value;

   begin
      return Value (Set (C.size_t'First + C.size_t (Index) - 1));
   end Item;

   ----------
   -- Item --
   ----------

   function Item
     (Set   : Attribute_Set_Access;
      Index : C.int) return chars_ptr
   is
      use type C.size_t;
   begin
      return Set (C.size_t'First + C.size_t (Index) - 1);
   end Item;

   --------------------
   -- LDAP_API_ERROR --
   --------------------

   function LDAP_API_ERROR (n : Return_Code) return Boolean is
   begin
      return n in API_ERROR;
   end LDAP_API_ERROR;

   ---------------------
   -- LDAP_API_RESULT --
   ---------------------

   function LDAP_API_RESULT (n : Return_Code) return Boolean is
   begin
      return n = 0 or else LDAP_API_ERROR (n);
   end LDAP_API_RESULT;

   ---------------------
   -- LDAP_ATTR_ERROR --
   ---------------------

   function LDAP_ATTR_ERROR (n : Return_Code) return Boolean is
   begin
      return n in ATTR_ERROR;
   end LDAP_ATTR_ERROR;

   ---------------------
   -- LDAP_NAME_ERROR --
   ---------------------

   function LDAP_NAME_ERROR (n : Return_Code) return Boolean is
   begin
      return n in NAME_ERROR;
   end LDAP_NAME_ERROR;

   -------------------------
   -- LDAP_SECURITY_ERROR --
   -------------------------

   function LDAP_SECURITY_ERROR (n : Return_Code) return Boolean is
   begin
      return n in SECURITY_ERROR;
   end LDAP_SECURITY_ERROR;

   ------------------------
   -- LDAP_SERVICE_ERROR --
   ------------------------

   function LDAP_SERVICE_ERROR (n : Return_Code) return Boolean is
   begin
      return n in SERVICE_ERROR;
   end LDAP_SERVICE_ERROR;

   -----------------------
   -- LDAP_UPDATE_ERROR --
   -----------------------

   function LDAP_UPDATE_ERROR (n : Return_Code) return Boolean is
   begin
      return n in UPDATE_ERROR;
   end LDAP_UPDATE_ERROR;

end AWS.LDAP.Thin;
