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

with Ada.Unchecked_Deallocation;

package body AWS.Net.Stream_IO is

   -----------------
   -- Write_Cache --
   -----------------

   protected body Write_Cache is

      -----------
      -- Flush --
      -----------

      procedure Flush is
      begin
         if Last /= 0 then
            Sockets.Send (Socket.all, Buffer (1 .. Last));
            Last := 0;
         end if;
      exception
         when others =>
            Last := 0;
            raise;
      end Flush;

      ----------------
      -- Initialize --
      ----------------

      procedure Initialize (Socket : in Socket_Access) is
      begin
         Write_Cache.Socket := Initialize.Socket;
      end Initialize;

      -----------
      -- Write --
      -----------

      procedure Write (Item : in Stream_Element_Array) is
      begin
         if Last + Item'Length > Cache_Size then
            Sockets.Send (Socket.all, Buffer (1 .. Last) & Item);
            Last := 0;
         else
            Buffer (Last + 1 .. Last + Item'Length) := Item;
            Last := Last + Item'Length;
         end if;

      exception
         when others =>
            Last := 0;
            raise;
      end Write;

   end Write_Cache;

   -----------
   -- Flush --
   -----------

   procedure Flush (Stream : in Socket_Stream_Access) is
   begin
      Stream.Cache.Flush;
   end Flush;

   ----------
   -- Free --
   ----------

   procedure Free (Stream : in out Socket_Stream_Access) is

      procedure Free is new Ada.Unchecked_Deallocation
        (Socket_Stream_Type, Socket_Stream_Access);

      procedure Free is new Ada.Unchecked_Deallocation
        (Socket_Type, Socket_Access);

   begin
      Flush (Stream);
      AWS.Net.Free (Stream.Socket.all);
      Free (Stream.Socket);
      Free (Stream);
   end Free;

   ----------
   -- Read --
   ----------

   procedure Read
     (Stream : in out Socket_Stream_Type;
      Item   :    out Ada.Streams.Stream_Element_Array;
      Last   :    out Ada.Streams.Stream_Element_Offset)
   is
   begin
      Stream.Cache.Flush;
      Sockets.Receive (Stream.Socket.all, Item);
      Last := Item'Last;
   end Read;

   --------------
   -- Shudtown --
   --------------

   procedure Shutdown (Stream : in Socket_Stream_Access) is
   begin
      Flush (Stream);
      Sockets.Shutdown (Stream.Socket.all);
   end Shutdown;

   ------------
   -- Stream --
   ------------

   function Stream
     (FD     : in Socket_Type)
     return Socket_Stream_Access
   is
      Result : Socket_Stream_Access := new Socket_Stream_Type;
   begin
      Result.Socket := new Socket_Type'(FD);
      Result.Cache.Initialize (Result.Socket);
      return Result;
   end Stream;

   -----------
   -- Write --
   -----------

   procedure Write
     (Stream : in out Socket_Stream_Type;
      Item   : in     Stream_Element_Array) is
   begin
      Stream.Cache.Write (Item);
   end Write;

end AWS.Net.Stream_IO;
