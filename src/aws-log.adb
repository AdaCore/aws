------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2006                          --
--                                 AdaCore                                  --
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

with Ada.Calendar;
with Ada.Command_Line;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;

with GNAT.Calendar.Time_IO;

with AWS.OS_Lib;

package body AWS.Log is

   package KV renames AWS.Containers.Key_Value.Table.Containers;

   function Log_Prefix (Prefix : in String) return String;
   --  Returns the prefix to be added before the log filename. The returned
   --  value is the executable name without directory and filetype if Prefix
   --  is No_Prefix otherwise Prefix is returned.

   procedure Check_Split (Log : in out Object; Now : in Ada.Calendar.Time);
   --  Split log file if necessary

   procedure Write_Log
     (Log  : in out Object;
      Now  : in     Calendar.Time;
      Data : in     String);
   --  Write data into the log file, change log file depending on the log file
   --  split mode and Now.

   ---------
   -- Add --
   ---------

   procedure Add (Data : in out Fields_Table; Name, Value : in String) is
      Dummy   : KV.Cursor;
      Success : Boolean;
   begin
      KV.Insert (Data.Data, Name, Value, Dummy, Success);

      if not Success then
         KV.Delete (Data.Data, Name);
         KV.Insert (Data.Data, Name, "*duplicated*" & Value, Dummy, Success);
      end if;
   end Add;

   -----------------
   -- Check_Split --
   -----------------

   procedure Check_Split (Log : in out Object; Now : in Ada.Calendar.Time) is
   begin
      if (Log.Split = Daily
          and then Log.Current_Tag /= Calendar.Day (Now))
        or else
         (Log.Split = Monthly
          and then Log.Current_Tag /= Calendar.Month (Now))
      then
         --  Could not call Stop, because Stop would write to log again and
         --  it cause unlimited recursion.

         Text_IO.Close (Log.File);

         Start (Log,
                Log.Split,
                To_String (Log.File_Directory),
                To_String (Log.Filename_Prefix));
      end if;
   end Check_Split;

   -----------
   -- Clear --
   -----------

   procedure Clear (Data : in out Fields_Table) is
   begin
      KV.Clear (Data.Data);
   end Clear;

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

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Log : in out Object) is
   begin
      Stop (Log);
   end Finalize;

   -----------
   -- Flush --
   -----------

   procedure Flush (Log : in out Object) is
      use Text_IO;
   begin
      if Log.Auto_Flush then
         return;
      end if;

      Log.Semaphore.Seize;

      if Is_Open (Log.File) then
         Flush (Log.File);
      end if;

      Log.Semaphore.Release;

   exception
      when others =>
         Log.Semaphore.Release;
         raise;
   end Flush;

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

      function Prog_Name return String;
      --  Return current program name

      ---------------
      -- Prog_Name --
      ---------------

      function Prog_Name return String is
         Name  : constant String := Ada.Command_Line.Command_Name;
         First : Natural;
         Last  : Natural;
      begin
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
      end Prog_Name;

   begin
      if Prefix = Not_Specified then
         return "";

      else
         declare
            K : constant Natural := Strings.Fixed.Index (Prefix, "@");
         begin
            if K = 0 then
               return Prefix & '-';
            else
               return Prefix (Prefix'First .. K - 1)
                 & Prog_Name & Prefix (K + 1 .. Prefix'Last) & '-';
            end if;
         end;
      end if;
   end Log_Prefix;

   ----------
   -- Mode --
   ----------

   function Mode (Log : in Object) return Split_Mode is
   begin
      return Log.Split;
   end Mode;

   --------------------
   -- Register_Field --
   --------------------

   procedure Register_Field (Log : in out Object; Id : in String) is
      Dummy   : SN.Cursor;
      Success : Boolean;
   begin
      SN.Insert
        (Log.Extended_Fields,
         Id, Natural (SN.Length (Log.Extended_Fields)) + 1,
         Dummy, Success);
   end Register_Field;

   -----------
   -- Start --
   -----------

   procedure Start
     (Log             : in out Object;
      Split           : in     Split_Mode := None;
      File_Directory  : in     String     := Not_Specified;
      Filename_Prefix : in     String     := Not_Specified;
      Auto_Flush      : in     Boolean    := False)
   is
      Now      : constant Calendar.Time := Calendar.Clock;
      Filename : Unbounded_String;
      use GNAT;
   begin
      Log.Filename_Prefix := To_Unbounded_String (Filename_Prefix);
      Log.File_Directory  := To_Unbounded_String (File_Directory);
      Log.Split           := Split;
      Log.Auto_Flush      := Auto_Flush;
      Log.Header_Written  := False;

      Filename := To_Unbounded_String
        (File_Directory
         & Log_Prefix (Filename_Prefix)
         & GNAT.Calendar.Time_IO.Image (Now, "%Y-%m-%d.log"));

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
                  & GNAT.Calendar.Time_IO.Image (Now, "%Y-%m-%d-")
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
      if Text_IO.Is_Open (Log.File) then
         Write (Log, "Stop logging.");
         Text_IO.Close (Log.File);
      end if;
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
             Response.Status_Code (Answer),
             Response.Content_Length (Answer));
   end Write;

   procedure Write
     (Log            : in out Object;
      Connect_Stat   : in     Status.Data;
      Status_Code    : in     Messages.Status_Code;
      Content_Length : in     Response.Content_Length_Type)
   is
      function Length_Image return String;
      pragma Inline (Length_Image);

      function Length_Image return String is
      begin
         if Content_Length = Response.Undefined_Length then
            return "";
         else
            return Utils.Image (Content_Length);
         end if;
      end Length_Image;

   begin
      Write
        (Log, Connect_Stat, Messages.Image (Status_Code) & ' ' & Length_Image);
   end Write;

   procedure Write
     (Log          : in out Object;
      Connect_Stat : in     Status.Data;
      Data         : in     String)
   is
      Now : constant Calendar.Time := Calendar.Clock;
   begin
      Write_Log
        (Log, Now,
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
   end Write;

   procedure Write (Log : in out Object; Data : in String) is
      Now : constant Calendar.Time := Calendar.Clock;
   begin
      Write_Log (Log, Now,
                 "[" & GNAT.Calendar.Time_IO.Image (Now, "%d/%b/%Y:%T") & "] "
                   & Data);
   end Write;

   --  Here is the extended log format:
   --
   --  #Version: 1.0
   --  #Date: 12-Jan-1996 00:00:00
   --  #Fields: time cs-method cs-uri
   --  00:34:23 GET /foo/bar.html

   procedure Write (Log  : in out Object; Data : in Fields_Table) is
      use GNAT.Calendar.Time_IO;

      Length : constant Natural := Natural (SN.Length (Log.Extended_Fields));
      Order  : array (1 .. Length) of KV.Cursor;
      Now    : Ada.Calendar.Time;

      C : KV.Cursor := KV.First (Data.Data);
      S : SN.Cursor;

   begin
      if Length = 0 then
         --  It is not extended log
         return;
      end if;

      Log.Semaphore.Seize;

      if Text_IO.Is_Open (Log.File) then
         while KV.Has_Element (C) loop
            S := SN.Find (Log.Extended_Fields, KV.Element (C));

            if SN.Has_Element (S) then
               Order (SN.Element (S)) := C;
            end if;

            C := KV.Next (C);
         end loop;

         Now := Ada.Calendar.Clock;

         Check_Split (Log, Now);

         if not Log.Header_Written then
            Log.Header_Written := True;

            Text_IO.Put_Line (Log.File, "#Version: 1.0");
            Text_IO.Put_Line
              (Log.File, "#Software: AWS (Ada Web Server) v" & Version);
            Text_IO.Put_Line
              (Log.File, "#Date: " & Image (Now, ISO_Date & " %T"));
            Text_IO.Put (Log.File, "#Fields:");

            for J in Order'Range loop
               Text_IO.Put (Log.File, ' ' & KV.Key (Order (J)));
            end loop;

            Text_IO.New_Line (Log.File);
         end if;

         for J in Order'Range loop
            if KV.Has_Element (Order (J)) then
               Text_IO.Put (Log.File, KV.Element (Order (J)));
            else
               Text_IO.Put (Log.File, '-');
            end if;

            if J = Order'Last then
               Text_IO.New_Line;
            else
               Text_IO.Put (Log.File, ' ');
            end if;
         end loop;

         if Log.Auto_Flush then
            Text_IO.Flush (Log.File);
         end if;
      end if;

      Log.Semaphore.Release;

   exception
      when others =>
         Log.Semaphore.Release;
         raise;
   end Write;

   ---------------
   -- Write_Log --
   ---------------

   procedure Write_Log
     (Log  : in out Object;
      Now  : in     Calendar.Time;
      Data : in     String) is
   begin
      Log.Semaphore.Seize;

      if Text_IO.Is_Open (Log.File) then

         Check_Split (Log, Now);

         Text_IO.Put_Line (Log.File, Data);

         if Log.Auto_Flush then
            Text_IO.Flush (Log.File);
         end if;
      end if;

      Log.Semaphore.Release;

   exception
      when others =>
         Log.Semaphore.Release;
         raise;
   end Write_Log;

end AWS.Log;
