------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                            Copyright (C) 2000                            --
--                      Dmitriy Anisimkov & Pascal Obry                     --
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

with Ada.Text_IO;
with Ada.Calendar;

with GNAT.Calendar.Time_IO;

package body AWS.Log is

   use Ada;

   Log_Activated : Boolean := False;
   Log_File      : Text_IO.File_Type;
   Split         : Split_Mode;
   Current_Tag   : Positive;

   -----------
   -- Start --
   -----------

   procedure Start (Split : in Split_Mode := None) is
      Now      : constant Calendar.Time := Calendar.Clock;
      Filename : constant String := "aws-"
        & GNAT.Calendar.Time_IO.Image (Now, "%a-%d-%b-%Y") & ".log";
   begin
      Log_Activated := True;
      Log.Split     := Split;

      case Split is
         when None =>
            null;
         when Daily =>
            Current_Tag := Calendar.Day (Now);
         when Monthly =>
            Current_Tag := Calendar.Month (Now);
      end case;

      Text_IO.Open (Log_File, Text_IO.Append_File, Filename);
   exception
      when Text_IO.Name_Error =>
         Text_IO.Create (Log_File, Text_IO.Out_File, Filename);
   end Start;

   ----------
   -- Stop --
   ----------

   procedure Stop is
   begin
      Log_Activated := False;
      Text_IO.Close (Log_File);
   end Stop;

   -----------
   -- Write --
   -----------

   --  127.0.0.1 - - [25/Apr/1998:15:37:29 +0200] "GET / HTTP/1.0" 200 1363

   procedure Write
     (Connect_Stat : in Status.Data;
      Answer_Stat  : in Messages.Status_Code;
      Peername     : in String)
   is
      Now : constant Calendar.Time := Calendar.Clock;
   begin
      if Log_Activated then

         if (Split = Daily and then Current_Tag /= Calendar.Day (Now))
           or else
           (Split = Monthly and then Current_Tag /= Calendar.Month (Now))
         then
            Stop;
            Start (Split);
         end if;

         Text_IO.Put_Line
           (Log_File,
            Peername & " - "
            & Status.Authorization_Name (Connect_Stat) & " - ["
            & GNAT.Calendar.Time_IO.Image (Now, "%d/%b/%Y:%T")
            & "] """
            & Status.Request_Method'Image (Status.Method (Connect_Stat))
            & ' '
            & Status.URI (Connect_Stat) & " "
            & Status.HTTP_Version (Connect_Stat) & """ "
            & Messages.Image (Answer_Stat));
         Text_IO.Flush (Log_File);
      end if;
   end Write;

end AWS.Log;
