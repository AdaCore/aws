------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2007                          --
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
with Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;

with GNAT.Calendar.Time_IO;

package body AWS.Log is

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
      Position : SN.Cursor;
      Success  : Boolean;
   begin
      Log.Extended_Fields.Insert
        (Id, Natural (Log.Extended_Fields.Length) + 1, Position, Success);
   end Register_Field;

   ---------------
   -- Set_Field --
   ---------------

   procedure Set_Field
     (Log : in Object; Data : in out Fields_Table; Id, Value : in String)
   is
      Ext_Len  : constant Natural := Natural (Log.Extended_Fields.Length);
      Data_Len : constant Natural := Natural (Data.Values.Length);
   begin
      if Ext_Len = 0 then
         return;
      end if;

      if Data_Len = 0 then
         --  Fields record is not initialized. We have to define fields set
         --  with empty "-" values.

         for J in 1 .. Ext_Len loop
            Data.Values.Append ("-");
         end loop;

      elsif Data_Len /= Ext_Len then
         --  Looks like the record was used with different log file

         raise Constraint_Error;
      end if;

      declare
         CSN : constant SN.Cursor := Log.Extended_Fields.Find (Id);
      begin
         if Value /= "" and then SN.Has_Element (CSN) then
            Data.Values.Replace_Element (SN.Element (CSN), Value);
         end if;
      end;
   end Set_Field;

   -----------------------
   -- Set_Header_Fields --
   -----------------------

   procedure Set_Header_Fields
     (Log    : in     Object;
      Data   : in out Fields_Table;
      Prefix : in     String;
      Header : in     AWS.Headers.List)
   is
      procedure Process (Name, Value : in String);

      -------------
      -- Process --
      -------------

      procedure Process (Name, Value : in String) is
      begin
         Set_Field
           (Log, Data, Prefix & '(' & Name & ')',
            AWS.Utils.Quote (Value, """"""));
      end Process;

   begin
      Header.Iterate_Names (", ", Process'Access);
   end Set_Header_Fields;

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
               --  no more than one run per second during a full day

               exit when not Directories.Exists (To_String (Filename));

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
   --
   --  See http://httpd.apache.org/docs/trunk/logs.html

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

      ------------------
      -- Length_Image --
      ------------------

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

      function Authorization_Name return String;
      pragma Inline (Authorization_Name);

      ------------------------
      -- Authorization_Name --
      ------------------------

      function Authorization_Name return String is
         Result : constant String := Status.Authorization_Name (Connect_Stat);
      begin
         if Result = "" then
            return "-";
         else
            return Result;
         end if;
      end Authorization_Name;

   begin
      Write_Log
        (Log, Now,
         AWS.Status.Peername (Connect_Stat)
           & " - " & Authorization_Name
           & " ["
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
      Write_Log
        (Log, Now,
         "[" & GNAT.Calendar.Time_IO.Image (Now, "%d/%b/%Y:%T") & "] " & Data);
   end Write;

   --  Here is the extended log format:
   --
   --  #Version: 1.0
   --  #Date: 12-Jan-1996 00:00:00
   --  #Fields: time cs-method cs-uri
   --  00:34:23 GET /foo/bar.html

   procedure Write (Log  : in out Object; Data : in out Fields_Table) is
      use GNAT.Calendar.Time_IO;

      Length      : constant Natural := Natural (Log.Extended_Fields.Length);
      Now         : Ada.Calendar.Time;
      First_Field : Boolean := True;

      procedure Write_And_Clear (Position : in SV.Cursor);

      ---------------------
      -- Write_And_Clear --
      ---------------------

      procedure Write_And_Clear (Position : in SV.Cursor) is
      begin
         if First_Field then
            First_Field := False;
            Text_IO.Put (Log.File, SV.Element (Position));
         else
            Text_IO.Put (Log.File, ' ' & SV.Element (Position));
         end if;

         Data.Values.Replace_Element (Position, "-");
      end Write_And_Clear;

   begin
      if Length = 0 then
         --  It is not extended log
         return;
      end if;

      Log.Semaphore.Seize;

      if Text_IO.Is_Open (Log.File) then
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

            declare
               Order : array (1 .. Length) of SN.Cursor;

               procedure Process (Position : in SN.Cursor);

               -------------
               -- Process --
               -------------

               procedure Process (Position : in SN.Cursor) is
               begin
                  Order (SN.Element (Position)) := Position;
               end Process;

            begin
               Log.Extended_Fields.Iterate (Process'Access);

               for J in Order'Range loop
                  Text_IO.Put (Log.File, ' ' & SN.Key (Order (J)));
               end loop;

               Text_IO.New_Line (Log.File);
            end;
         end if;

         --  Set date and time fields if the used does not fill it

         declare
            CSN : SN.Cursor := Log.Extended_Fields.Find ("date");
            P   : Positive;
         begin
            if SN.Has_Element (CSN) then
               P := SN.Element (CSN);

               if Data.Values.Element (P) = "-" then
                  Data.Values.Replace_Element (P, Image (Now, ISO_Date));
               end if;
            end if;

            CSN := Log.Extended_Fields.Find ("time");

            if SN.Has_Element (CSN) then
               P := SN.Element (CSN);

               if Data.Values.Element (P) = "-" then
                  Data.Values.Replace_Element (P, Image (Now, "%T"));
               end if;
            end if;
         end;

         Data.Values.Iterate (Write_And_Clear'Access);

         Text_IO.New_Line (Log.File);

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
