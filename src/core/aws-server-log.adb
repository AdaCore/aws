------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2003-2012, AdaCore                     --
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

with AWS.Config.Set;

package body AWS.Server.Log is

   ----------------
   -- Error_Name --
   ----------------

   function Error_Name (Web_Server : HTTP) return String is
   begin
      return AWS.Log.Filename (Web_Server.Error_Log);
   end Error_Name;

   -----------
   -- Flush --
   -----------

   procedure Flush (Web_Server : in out HTTP) is
   begin
      AWS.Log.Flush (Web_Server.Log);
   end Flush;

   ---------------
   -- Is_Active --
   ---------------

   function Is_Active (Web_Server : HTTP) return Boolean is
   begin
      return AWS.Log.Is_Active (Web_Server.Log);
   end Is_Active;

   ---------------------
   -- Is_Error_Active --
   ---------------------

   function Is_Error_Active (Web_Server : HTTP) return Boolean is
   begin
      return AWS.Log.Is_Active (Web_Server.Error_Log);
   end Is_Error_Active;

   ----------
   -- Name --
   ----------

   function Name (Web_Server : HTTP) return String is
   begin
      return AWS.Log.Filename (Web_Server.Log);
   end Name;

   -----------
   -- Start --
   -----------

   procedure Start
     (Web_Server      : in out HTTP;
      Split_Mode      : AWS.Log.Split_Mode := AWS.Log.None;
      Filename_Prefix : String             := "";
      Auto_Flush      : Boolean            := False)
   is
      use type AWS.Log.Split_Mode;

      procedure Register_Extended_Field (Id : String);

      -----------------------------
      -- Register_Extended_Field --
      -----------------------------

      procedure Register_Extended_Field (Id : String) is
      begin
         AWS.Log.Register_Field (Web_Server.Log, Id);
      end Register_Extended_Field;

      procedure Register_Extended_Fields is
        new CNF.Log_Extended_Fields_Generic_Iterate (Register_Extended_Field);

   begin
      if Split_Mode /= AWS.Log.None then
         CNF.Set.Log_Split_Mode
           (Web_Server.Properties, AWS.Log.Split_Mode'Image (Split_Mode));
      end if;

      if Filename_Prefix /= "" then
         CNF.Set.Log_Filename_Prefix
           (Web_Server.Properties, Filename_Prefix);
      end if;

      Register_Extended_Fields (Web_Server.Properties);

      AWS.Log.Start
        (Web_Server.Log,
         AWS.Log.Split_Mode'Value (CNF.Log_Split_Mode (Web_Server.Properties)),
         Size_Limit      => CNF.Log_Size_Limit (Web_Server.Properties),
         File_Directory  => CNF.Log_File_Directory (Web_Server.Properties),
         Filename_Prefix => CNF.Log_Filename_Prefix (Web_Server.Properties),
         Auto_Flush      => Auto_Flush);
   end Start;

   -----------
   -- Start --
   -----------

   procedure Start
     (Web_Server : in out HTTP;
      Callback   : AWS.Log.Callback;
      Name       : String)
   is
      procedure Register_Extended_Field (Id : String);

      -----------------------------
      -- Register_Extended_Field --
      -----------------------------

      procedure Register_Extended_Field (Id : String) is
      begin
         AWS.Log.Register_Field (Web_Server.Log, Id);
      end Register_Extended_Field;

      procedure Register_Extended_Fields is
        new CNF.Log_Extended_Fields_Generic_Iterate (Register_Extended_Field);

   begin
      Register_Extended_Fields (Web_Server.Properties);

      AWS.Log.Start (Web_Server.Log, Callback, Name);
   end Start;

   -----------------
   -- Start_Error --
   -----------------

   procedure Start_Error
     (Web_Server      : in out HTTP;
      Split_Mode      : AWS.Log.Split_Mode := AWS.Log.None;
      Filename_Prefix : String             := "")
   is
      use type AWS.Log.Split_Mode;
   begin
      if Split_Mode /= AWS.Log.None then
         CNF.Set.Error_Log_Split_Mode
           (Web_Server.Properties, AWS.Log.Split_Mode'Image (Split_Mode));
      end if;

      if Filename_Prefix /= "" then
         CNF.Set.Error_Log_Filename_Prefix
           (Web_Server.Properties, Filename_Prefix);
      end if;

      AWS.Log.Start
        (Web_Server.Error_Log,
         AWS.Log.Split_Mode'Value
           (CNF.Error_Log_Split_Mode (Web_Server.Properties)),
         Size_Limit      => CNF.Log_Size_Limit (Web_Server.Properties),
         File_Directory  => CNF.Log_File_Directory (Web_Server.Properties),
         Filename_Prefix => CNF.Error_Log_Filename_Prefix
                              (Web_Server.Properties),
         Auto_Flush      => True);
   end Start_Error;

   -----------------
   -- Start_Error --
   -----------------

   procedure Start_Error
     (Web_Server : in out HTTP;
      Callback   : AWS.Log.Callback;
      Name       : String) is
   begin
      AWS.Log.Start (Web_Server.Error_Log, Callback, Name);
   end Start_Error;

   ----------
   -- Stop --
   ----------

   procedure Stop (Web_Server : in out HTTP) is
   begin
      AWS.Log.Stop (Web_Server.Log);
   end Stop;

   ----------------
   -- Stop_Error --
   ----------------

   procedure Stop_Error (Web_Server : in out HTTP) is
   begin
      AWS.Log.Stop (Web_Server.Error_Log);
   end Stop_Error;

end AWS.Server.Log;
