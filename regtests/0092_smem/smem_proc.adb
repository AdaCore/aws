------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2005-2010, AdaCore                     --
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

--  Common code for plain and SSL test for broke memory on socket reusing

with Ada.Exceptions;
with Ada.Streams;
with Ada.Text_IO;
with AWS.Net.Std;

with Stack_Size;

procedure SMem_Proc (Security : Boolean) is
   use AWS.Net;
   use Ada.Streams;
   use Ada.Text_IO;

   Server : Std.Socket_Type;
   S1, S2 : Socket_Type'Class := Socket (Security);
   C1, C2 : Socket_Type'Class := Socket (Security);

   Sample : Stream_Element_Array (1 .. 128);

   procedure Test (S1, S2 : in out AWS.Net.Socket_Type'Class);

   -------------
   -- Connect --
   -------------

   procedure Connect is
      task Connector is
         pragma Storage_Size (Stack_Size.Value);
      end Connector;

      task body Connector is
      begin
         Connect (S1, "127.0.0.1", Std.Get_Port (Server));
      exception
         when E : others =>
            Put_Line
              ("On connect " & Ada.Exceptions.Exception_Information (E));
      end Connector;
   begin
      Accept_Socket (Server, S2);
   end Connect;

   ----------
   -- Test --
   ----------

   procedure Test (S1, S2 : in out AWS.Net.Socket_Type'Class) is
      Buffer : Stream_Element_Array (Sample'First .. Sample'Last + 2);
      Last   : Stream_Element_Offset;

      task Send is
         pragma Storage_Size (Stack_Size.Value);
      end Send;

      task body Send is
      begin
         AWS.Net.Send (S2, Sample);
      exception
         when E : others =>
            Put_Line ("On send " & Ada.Exceptions.Exception_Information (E));
      end Send;

   begin
      AWS.Net.Receive (S1, Buffer, Last);

      if Sample = Buffer (1 .. Last) then
         Put_Line ("OK");
      else
         Put_Line ("Error");
      end if;
   end Test;

begin
   for J in Sample'Range loop
      Sample (J) := Stream_Element (J);
   end loop;

   Std.Bind (Server, 0);
   Std.Listen (Server);

   Set_Timeout (S1, 2.0);
   Set_Timeout (S2, 2.0);

   Connect;

   Test (S1, S2);
   Test (S2, S1);

   C1 := S1;
   C2 := S2;

   Connect;

   Test (S1, S2);
   Test (S2, S1);

   Shutdown (C1);
   Shutdown (C2);

   Test (S1, S2);
   Test (S2, S1);

   Shutdown (S2);
   Shutdown (S1);
   Std.Shutdown (Server);
end SMem_Proc;
