------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2002-2012, AdaCore                     --
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

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;

with AWS.Resources.Streams.ZLib;

with ZLib;

package body AWS.Resources.Embedded is

   type Node is record
      File_Buffer : Buffer_Access;
      File_Time   : Calendar.Time;
   end record;

   package Res_Files is new Ada.Containers.Indefinite_Hashed_Maps
     (String, Node, Ada.Strings.Hash, "=", "=");

   Files_Table : Res_Files.Map;

   procedure Append
     (Stream : Streams.Stream_Access;
      Data   : Buffer_Access);

   ------------
   -- Append --
   ------------

   procedure Append
     (Stream : Streams.Stream_Access;
      Data   : Buffer_Access) is
   begin
      Streams.Memory.Append
        (Streams.Memory.Stream_Type (Stream.all), Data);
   end Append;

   ------------
   -- Create --
   ------------

   procedure Create
     (File   : out File_Type;
      Buffer : Buffer_Access)
   is
      Stream : Streams.Stream_Access;

   begin
      Stream := new Streams.Memory.Stream_Type;

      Append (Stream, Buffer);

      Streams.Create (File, Stream);
   end Create;

   -----------
   -- Exist --
   -----------

   function Exist (Name : String) return File_Instance is
      VP, VG : Boolean := False;
   begin
      if Is_GZip (Name) then
         VG := Res_Files.Contains (Files_Table, Name);
         VP := Res_Files.Contains
           (Files_Table,
            (Name (Name'First .. Name'Last - GZip_Ext'Length)));
      else
         VP := Res_Files.Contains (Files_Table, Name);
         VG := Res_Files.Contains (Files_Table, Name & GZip_Ext);
      end if;

      if VG and then VP then
         return Both;
      elsif VG then
         return GZip;
      elsif VP then
         return Plain;
      else
         return None;
      end if;
   end Exist;

   ---------------
   -- File_Size --
   ---------------

   function File_Size (Name : String) return Utils.File_Size_Type is
      Cursor : Res_Files.Cursor;
   begin
      Cursor := Res_Files.Find (Files_Table, Name);

      if Res_Files.Has_Element (Cursor) then
         return Res_Files.Element (Cursor).File_Buffer'Length;

      elsif Is_GZip (Name) then
         --  Don't look for resource Name & ".gz.gz"

         raise Resource_Error;
      end if;

      Cursor := Res_Files.Find (Files_Table, Name & GZip_Ext);

      if Res_Files.Has_Element (Cursor) then
         return Res_Files.Element (Cursor).File_Buffer'Length;
      else
         raise Resource_Error;
      end if;
   end File_Size;

   --------------------
   -- File_Timestamp --
   --------------------

   function File_Timestamp (Name : String) return Ada.Calendar.Time is
      Cursor : Res_Files.Cursor;
   begin
      Cursor := Res_Files.Find (Files_Table, Name);

      if Res_Files.Has_Element (Cursor) then
         return Res_Files.Element (Cursor).File_Time;

      elsif Is_GZip (Name) then
         --  Don't look for resource Name & ".gz.gz";

         raise Resource_Error;
      end if;

      Cursor := Res_Files.Find (Files_Table, Name & GZip_Ext);

      if Res_Files.Has_Element (Cursor) then
         return Res_Files.Element (Cursor).File_Time;
      else
         raise Resource_Error;
      end if;
   end File_Timestamp;

   ---------------------
   -- Is_Regular_File --
   ---------------------

   function Is_Regular_File (Name : String) return Boolean is
   begin
      return Res_Files.Contains (Files_Table, Name)
        or else (not Is_GZip (Name)
                 and then Res_Files.Contains (Files_Table, Name & GZip_Ext));
   end Is_Regular_File;

   ----------
   -- Open --
   ----------

   procedure Open
     (File : out File_Type;
      Name : String;
      Form : String    := "";
      GZip : in out Boolean)
   is
      pragma Unreferenced (Form);

      Stream : Streams.Stream_Access;
      Found  : Boolean;

      procedure Open_File (Name : String);

      ---------------
      -- Open_File --
      ---------------

      procedure Open_File (Name : String) is
         Cursor : Res_Files.Cursor;
      begin
         Cursor := Res_Files.Find (Files_Table, Name);

         if Res_Files.Has_Element (Cursor) then
            Found := True;
            Stream := new Streams.Memory.Stream_Type;

            Append (Stream, Res_Files.Element (Cursor).File_Buffer);

         else
            Found := False;
         end if;
      end Open_File;

   begin
      if Is_GZip (Name) then
         --  Don't try to open file Name & ".gz.gz"

         GZip := False;

         Open_File (Name);

      elsif GZip then
         Open_File (Name & GZip_Ext);

         if not Found then
            Open_File (Name);

            if Found then
               GZip := False;
            end if;
         end if;

      else
         Open_File (Name);

         if not Found then
            Open_File (Name & GZip_Ext);

            if Found then
               Stream
                 := Streams.ZLib.Inflate_Create (Stream, Header => ZLib.GZip);
            end if;
         end if;
      end if;

      Streams.Create (File, Stream);
   end Open;

   --------------
   -- Register --
   --------------

   procedure Register
     (Name      : String;
      Content   : Buffer_Access;
      File_Time : Calendar.Time)
   is
      N : constant Node := (Content, File_Time);
   begin
      Res_Files.Include (Files_Table, Name, N);
   end Register;

end AWS.Resources.Embedded;
