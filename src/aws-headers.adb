------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2001                          --
--                                ACT-Europe                                --
--                                                                          --
--  Authors: Dmitriy Anisimkov - Pascal Obry                                --
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

with Ada.Streams;
with AWS.Translator;

package body AWS.Headers is

   --------------
   -- Get_Line --
   --------------

   function Get_Line
     (Headers : in List;
      N       : in Positive)
      return String
   is
      Pair : constant Name_Value_Type :=
         Get_Name_Value (Headers, N);
   begin
      if Pair.Name = "" then
         return "";
      else
         return Pair.Name & ": " & Pair.Value;
      end if;
   end Get_Line;

   -----------------
   -- Send_Header --
   -----------------

   procedure Send_Header
     (Headers : in List;
      Socket  : in Sockets.Socket_FD'Class)
   is
      use Ada.Streams;

      Buffer : Stream_Element_Array (1 .. 1024);
      pragma Warnings (Off, Buffer);
      --  The buffer is to send bigger data chunk at once.
      --  The SSL implementation is working better with bigger chunk.
      --  ??? This buffering should not be done there but in the socket
      --  implementation.

      CRLF   : Stream_Element_Array :=
         (1 => Character'Pos (ASCII.CR),
          2 => Character'Pos (ASCII.LF));
      --  End of line.

      First  : Stream_Element_Offset := Buffer'First;
      Last   : Stream_Element_Offset;

   begin
      for J in 1 .. Count (Headers) loop
         declare
            Line : constant Stream_Element_Array
              := AWS.Translator.To_Stream_Element_Array
              (Get_Line (Headers, J)) & CRLF;
         begin
            Last := First + Line'Length - 1;

            if Last > Buffer'Last then
               Sockets.Send
                 (Socket,
                  Buffer (Buffer'First .. First - 1) & Line);
               First := Buffer'First;

            else
               Buffer (First .. Last) := Line;
               First := Last + 1;
            end if;
         end;
      end loop;

      if First > Buffer'First then
         Sockets.Send
           (Socket,
            Buffer (Buffer'First .. First - 1));
      end if;
   end Send_Header;

end AWS.Headers;
