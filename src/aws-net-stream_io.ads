------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2002                          --
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

package AWS.Net.Stream_IO is

   type Socket_Stream_Type is new Ada.Streams.Root_Stream_Type with private;

   type Socket_Stream_Access is access Socket_Stream_Type;

   function Stream
     (Socket : in Socket_Type'Class)
      return Socket_Stream_Access;
   --  Build a Stream Socket type.

   procedure Shutdown (Stream : in Socket_Stream_Access);
   --  Terminate the Stream and Flush the stream if needed.

   procedure Free (Stream : in out Socket_Stream_Access);
   --  Release memory associated with the Stream.

   procedure Flush (Stream : in Socket_Stream_Access);
   pragma Inline (Flush);
   --  Send all remaining data in the stream to the peer.

   procedure Read
     (Stream : in out Socket_Stream_Type;
      Item   :    out Ada.Streams.Stream_Element_Array;
      Last   :    out Ada.Streams.Stream_Element_Offset);
   --  Read a piece of data from the Stream. Returns the data into Item, Last
   --  point to the last Steam_Element read.

   procedure Write
     (Stream : in out Socket_Stream_Type;
      Item   : in     Ada.Streams.Stream_Element_Array);
   --  Write Item to the stream.

private

   use Ada.Streams;

   type Socket_Stream_Type is new Ada.Streams.Root_Stream_Type with record
      Socket : Socket_Access := null;
   end record;

end AWS.Net.Stream_IO;
