------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                       Copyright (C) 2014, AdaCore                        --
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

with Ada.Directories;
with Ada.Unchecked_Deallocation;

with AWS.Config;

package body AWS.Net.SSL.RSA_DH_Generators is

   use Ada;

   task type RSA_DH_Generator (DH : Boolean);

   type RSA_DH_Generator_Access is access all RSA_DH_Generator;

   RSA_DH_Worker : RSA_DH_Generator_Access;

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (RSA_DH_Generator, RSA_DH_Generator_Access);

   Starter : Utils.Test_And_Set;

   -------------------------
   -- Parameters_Filename --
   -------------------------

   function Parameters_Filename
     (Kind : String; Exist : Boolean) return String
   is
      Filename : constant String :=
                   Directories.Compose
                     (AWS.Config.Security_Home_Directory,
                      "aws-" & Kind & ".pem");
   begin
      if Exist then
         if Directories.Exists (Filename) then
            return Filename;
         else
            Log_Error ("File " & Filename & " does not exists");
            return "";
         end if;

      elsif not Directories.Exists (AWS.Config.Security_Home_Directory) then
         Directories.Create_Path (AWS.Config.Security_Home_Directory);
      end if;

      return Filename;
   end Parameters_Filename;

   ----------------------
   -- RSA_DH_Generator --
   ----------------------

   task body RSA_DH_Generator is
   begin
      Generate_RSA;

      if DH then
         Generate_DH;
      end if;

   exception
      when E : others =>
         Log_Error (Exceptions.Exception_Message (E));
   end RSA_DH_Generator;

   ---------------------------------
   -- Start_Parameters_Generation --
   ---------------------------------

   procedure Start_Parameters_Generation (DH : Boolean) is
      OK : Boolean;
   begin
      Starter.Try_Lock (OK);

      if not OK then
         return;
      end if;

      if RSA_DH_Worker = null or else RSA_DH_Worker'Terminated then
         Unchecked_Free (RSA_DH_Worker);
         RSA_DH_Worker := new RSA_DH_Generator (DH);
      end if;

      Starter.Unlock;
   end Start_Parameters_Generation;

end AWS.Net.SSL.RSA_DH_Generators;
