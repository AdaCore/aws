------------------------------------------------------------------------------
--                            Secure Sockets Layer                          --
--                                                                          --
--                        Copyright (C) 2022, AdaCore                       --
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

with GNATCOLL.Plugins;
with GNATCOLL.OS.Constants;

package body SSL.Loader is

   use GNATCOLL.Plugins;

   Library_Name : constant String :=
                    "libgnutls" & GNATCOLL.OS.Constants.DLL_Ext;

   Libgnutls : constant Plugin := Load (Library_Name);

   Error_Message : aliased constant String :=
                     Library_Name & " not found" & ASCII.NUL;

   function Unsupported return Integer is (-59) with Convention => C;
   --  Returns GNUTLS_E_INTERNAL_ERROR error code

   procedure Raise_Exception with No_Return, Convention => C;
   --  Raises Program_Error exception

   function Strerror return access constant String with Convention => C;

   ---------------------
   -- Raise_Exception --
   ---------------------

   procedure Raise_Exception is
   begin
      raise Program_Error with
        Error_Message (Error_Message'First .. Error_Message'Last - 1);
   end Raise_Exception;

   --------------
   -- Strerror --
   --------------

   function Strerror return access constant String is
   begin
      return Error_Message'Access;
   end Strerror;

   ------------
   -- Symbol --
   ------------

   function Symbol (Name : String) return System.Address is
      use type System.Address;

      Result : constant System.Address :=
                 (if Libgnutls = No_Plugin
                  then System.Null_Address
                  else Routine_Address (Libgnutls, Name));
   begin
      if Result = System.Null_Address then
         return (if Name = "gnutls_init" then Raise_Exception'Address
                 elsif Name = "gnutls_strerror" then Strerror'Address
                 else Unsupported'Address);
      else
         return Result;
      end if;
   end Symbol;

end SSL.Loader;
