------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2004                          --
--                                ACT-Europe                                --
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

with AWS.Net.Buffered;

package body AWS.Net.Stream_IO is

   -----------
   -- Flush --
   -----------

   procedure Flush (Stream : in Socket_Stream_Access) is
   begin
      Buffered.Flush (Stream.Socket.all);
   end Flush;

   ----------
   -- Free --
   ----------

   procedure Free
     (Stream : in out Socket_Stream_Access;
      Socket : in     Boolean)
   is
      procedure Free is new Ada.Unchecked_Deallocation
        (Socket_Stream_Type, Socket_Stream_Access);

      procedure Free is new Ada.Unchecked_Deallocation
        (Socket_Type'Class, Socket_Access);

   begin
      if Socket then
         AWS.Net.Free (Stream.Socket);
      else
         Free (Stream.Socket);
      end if;

      Free (Stream);
   end Free;

   ----------
   -- Read --
   ----------

   procedure Read
     (Stream : in out Socket_Stream_Type;
      Item   :    out Ada.Streams.Stream_Element_Array;
      Last   :    out Ada.Streams.Stream_Element_Offset) is
   begin
      Buffered.Read (Stream.Socket.all, Item);
      Last := Item'Last;
   end Read;

   --------------
   -- Shudtown --
   --------------

   procedure Shutdown (Stream : in Socket_Stream_Access) is
   begin
      Buffered.Shutdown (Stream.Socket.all);
   end Shutdown;

   ------------
   -- Stream --
   ------------

   function Stream
     (Socket : in Socket_Type'Class)
      return Socket_Stream_Access
   is
      Result : constant Socket_Stream_Access := new Socket_Stream_Type;
   begin
      Result.Socket := new Socket_Type'Class'(Socket);
      return Result;
   end Stream;

   -----------
   -- Write --
   -----------

   procedure Write
     (Stream : in out Socket_Stream_Type;
      Item   : in     Stream_Element_Array) is
   begin
      Buffered.Write (Stream.Socket.all, Item);
   end Write;

end AWS.Net.Stream_IO;
