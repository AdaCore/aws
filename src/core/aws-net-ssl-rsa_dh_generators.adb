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

   task type RSA_DH_Generator
     (DH : Boolean; Logging : access procedure (Text : String));

   type RSA_DH_Generator_Access is access all RSA_DH_Generator;

   RSA_DH_Worker : RSA_DH_Generator_Access;

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (RSA_DH_Generator, RSA_DH_Generator_Access);

   Single_Task : Utils.Test_And_Set;

   -------------------------
   -- Parameters_Filename --
   -------------------------

   function Parameters_Filename
     (Kind : String; Exist : Boolean) return String
   is
      Filename : constant String :=
                   Directories.Compose
                     (AWS.Config.Config_Directory,
                      "aws-" & Kind & ".pem");
   begin
      if Exist then
         if Directories.Exists (Filename) then
            return Filename;
         else
            Log_Error ("File " & Filename & " does not exist");
            return "";
         end if;

      elsif not Directories.Exists (AWS.Config.Config_Directory) then
         Directories.Create_Path (AWS.Config.Config_Directory);
      end if;

      return Filename;
   end Parameters_Filename;

   ----------------------
   -- RSA_DH_Generator --
   ----------------------

   task body RSA_DH_Generator is
      use type Ada.Calendar.Time;
      Stamp : Ada.Calendar.Time;
   begin
      if Logging /= null then
         Stamp := Ada.Calendar.Clock;
      end if;

      Generate_RSA;

      if Logging /= null then
         Logging ("RSA" & Duration'Image (Ada.Calendar.Clock - Stamp));
      end if;

      if DH then
         if Logging /= null then
            Stamp := Ada.Calendar.Clock;
         end if;

         Generate_DH;

         if Logging /= null then
            Logging ("DH" & Duration'Image (Ada.Calendar.Clock - Stamp));
         end if;
      end if;

      Single_Task.Unlock;

   exception
      when E : others =>
         Log_Error (Exceptions.Exception_Message (E));
         Single_Task.Unlock;
   end RSA_DH_Generator;

   ---------------------------------
   -- Start_Parameters_Generation --
   ---------------------------------

   procedure Start_Parameters_Generation
     (DH : Boolean; Logging : access procedure (Text : String) := null)
   is
      OK  : Boolean;
      Cnt : Natural := 8;

   begin
      Single_Task.Try_Lock (OK);

      if not OK then
         if Logging /= null then
            Logging ("Generation task already running");
         end if;

         return;
      end if;

      while RSA_DH_Worker /= null and then not RSA_DH_Worker'Terminated loop
         delay 0.125;

         Cnt := Cnt - 1;

         if Cnt = 0 then
            raise Program_Error with "Generation task is not terminating";
         end if;
      end loop;

      Unchecked_Free (RSA_DH_Worker);
      RSA_DH_Worker := new RSA_DH_Generator (DH, Logging);
   end Start_Parameters_Generation;

end AWS.Net.SSL.RSA_DH_Generators;
