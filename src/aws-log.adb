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

   -----------
   -- Start --
   -----------

   procedure Start is
   begin
      Log_Activated := True;
      Text_IO.Open (Log_File, Text_IO.Append_File, "aws.log");
   exception
      when Text_IO.Name_Error =>
         Text_IO.Create (Log_File, Text_IO.Out_File, "aws.log");
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
      Peername     : in String) is
   begin
      if Log_Activated then
         Text_IO.Put_Line
           (Log_File,
            Peername & " - "
            & Status.Authorization_Name (Connect_Stat) & " - ["
            & GNAT.Calendar.Time_IO.Image (Calendar.Clock, "%d/%b/%Y:%T")
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
