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

with Ada.Unchecked_Conversion;

with Interfaces.C;

with AWS.Net.SSL.Certificate.Impl;

package body AWS.Net.SSL.Certificate is

   Pwd_Callback : Password_Callback;

   ---------
   -- Get --
   ---------

   function Get (Socket : Socket_Type) return Object is
   begin
      return Impl.Get (Socket);
   end Get;

   ------------------
   -- Get_Password --
   ------------------

   function Get_Password (Certificate_Filename : String) return String is
   begin
      if Pwd_Callback = null then
         return "";
      else
         return Pwd_Callback (Certificate_Filename);
      end if;
   end Get_Password;

   ----------
   -- Load --
   ----------

   function Load (Filename : String) return Object is
   begin
      return Impl.Load (Filename);
   end Load;

   ---------------------------
   -- Set_Password_Callback --
   ---------------------------

   procedure Set_Password_Callback
     (Callback : Password_Callback) is
   begin
      Pwd_Callback := Callback;
   end Set_Password_Callback;

   -------------------------
   -- Set_Verify_Callback --
   -------------------------

   procedure Set_Verify_Callback
     (Config : in out SSL.Config; Callback : Verify_Callback)
   is
      function To_Address is new Unchecked_Conversion
        (Net.SSL.Certificate.Verify_Callback, System.Address);
   begin
      Set_Verify_Callback (Config, To_Address (Callback));
   end Set_Verify_Callback;

   --------------------
   -- Status_Message --
   --------------------

   function Status_Message (Certificate : Object) return String is
   begin
      return Impl.Status_String (Interfaces.C.long (Certificate.Status));
   end Status_Message;

end AWS.Net.SSL.Certificate;
