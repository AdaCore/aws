------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2001                          --
--                               ACT-Europe                                 --
--                                                                          --
--  Authors: Dmitriy Anisimov - Pascal Obry                                 --
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

--  $Id$

with Ada.Calendar;
with Ada.Command_Line;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;

with GNAT.Calendar.Time_IO;
with AWS.OS_Lib;

with AWS.Messages;
with AWS.Utils;

package body AWS.Log is

   function Log_Prefix (Prefix : in String) return String;
   --  Returns the prefix to be added before the log filename. The returned
   --  value is the executable name without directory and filetype if Prefix
   --  is No_Prefix otherwise Prefix is returned.

   --------------
   -- Filename --
   --------------

   function Filename (Log : in Object) return String is
   begin
      if Text_IO.Is_Open (Log.File) then
         return Text_IO.Name (Log.File);
      else
         return "";
      end if;
   end Filename;

   ---------------
   -- Is_Active --
   ---------------

   function Is_Active (Log : in Object) return Boolean is
   begin
      return Text_IO.Is_Open (Log.File);
   end Is_Active;

   ----------------
   -- Log_Prefix --
   ----------------

   function Log_Prefix (Prefix : in String) return String is
      Name  : constant String := Ada.Command_Line.Command_Name;
      First : Natural;
      Last  : Natural;
   begin
      if Prefix = Not_Specified then
         First := Strings.Fixed.Index
           (Name, Strings.Maps.To_Set ("/\"), Going => Strings.Backward);

         if First = 0 then
            First := Name'First;
         else
            First := First + 1;
         end if;

         Last := Strings.Fixed.Index
           (Name (First .. Name'Last), ".", Strings.Backward);

         if Last = 0 then
            Last := Name'Last;
         else
            Last := Last - 1;
         end if;

         return Name (First .. Last);

      else
         return Prefix;
      end if;
   end Log_Prefix;

   ----------
   -- Mode --
   ----------

   function Mode (Log : in Object) return Split_Mode is
   begin
      return Log.Split;
   end Mode;

   -----------
   -- Start --
   -----------

   procedure Start
     (Log             : in out Object;
      Split           : in     Split_Mode := None;
      File_Directory  : in     String     := Not_Specified;
      Filename_Prefix : in     String     := Not_Specified)
   is
      Now      : constant Calendar.Time := Calendar.Clock;
      Filename : Unbounded_String;
      use GNAT;
   begin
      Log.Filename_Prefix := To_Unbounded_String (Filename_Prefix);
      Log.File_Directory  := To_Unbounded_String (File_Directory);
      Log.Split           := Split;

      Filename := To_Unbounded_String
        (File_Directory
         & Log_Prefix (Filename_Prefix)
         & GNAT.Calendar.Time_IO.Image (Now, "-%Y-%m-%d.log"));

      case Split is
         when None =>
            null;

         when Each_Run =>
            for K in 1 .. 86_400 loop
               --  no more than one run per second during a full day.

               exit when not OS_Lib.Is_Regular_File (To_String (Filename));

               Filename := To_Unbounded_String
                 (File_Directory
                  & Log_Prefix (Filename_Prefix)
                  & GNAT.Calendar.Time_IO.Image (Now, "-%Y-%m-%d-")
                  & Utils.Image (K) & ".log");
            end loop;

         when Daily =>
            Log.Current_Tag := Ada.Calendar.Day (Now);

         when Monthly =>
            Log.Current_Tag := Ada.Calendar.Month (Now);
      end case;

      Text_IO.Open (Log.File, Text_IO.Append_File, To_String (Filename));

   exception
      when Text_IO.Name_Error =>
         Text_IO.Create (Log.File, Text_IO.Out_File, To_String (Filename));
   end Start;

   ----------
   -- Stop --
   ----------

   procedure Stop (Log : in out Object) is
   begin
      Text_IO.Close (Log.File);
   end Stop;

   -----------
   -- Write --
   -----------

   --  Here is the log format compatible with Apache:
   --
   --  127.0.0.1 - - [25/Apr/1998:15:37:29 +0200] "GET / HTTP/1.0" 200 1363


   procedure Write
     (Log          : in out Object;
      Connect_Stat : in     Status.Data;
      Answer       : in     Response.Data) is
   begin
      Write (Log, Connect_Stat,
         Messages.Image (Response.Status_Code (Answer))
         & ' '
         & Utils.Image (Response.Content_Length (Answer)));
   end Write;

   procedure Write
     (Log          : in out Object;
      Connect_Stat : in     Status.Data;
      Data         : in     String)
   is
      Now : constant Calendar.Time := Calendar.Clock;
   begin
      if Text_IO.Is_Open (Log.File) then

         if (Log.Split = Daily
             and then Log.Current_Tag /= Calendar.Day (Now))
           or else
            (Log.Split = Monthly
             and then Log.Current_Tag /= Calendar.Month (Now))
         then
            Stop (Log);
            Start (Log,
                   Log.Split,
                   To_String (Log.File_Directory),
                   To_String (Log.Filename_Prefix));
         end if;

         Text_IO.Put_Line
           (Log.File,
            AWS.Status.Peername (Connect_Stat)
            & " - "
            & Status.Authorization_Name (Connect_Stat)
            & " - ["
            & GNAT.Calendar.Time_IO.Image (Now, "%d/%b/%Y:%T")
            & "] """
            & Status.Request_Method'Image (Status.Method (Connect_Stat))
            & ' '
            & Status.URI (Connect_Stat) & " "
            & Status.HTTP_Version (Connect_Stat) & """ "
            & Data);

         Text_IO.Flush (Log.File);
      end if;
   end Write;

end AWS.Log;
