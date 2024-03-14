------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2013, AdaCore                     --
--                                                                          --
--  This library is free software;  you can redistribute it and/or modify   --
--  it under terms of the  GNU General Public License  as published by the  --
--  Free Software  Foundation;  either version 3,  or (at your  option) any --
--  later version. This library is distributed in the hope that it will be  --
--  useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                    --
--                                                                          --
--  As a special exception under Section 7 of GPL version 3, you are        --
--  granted additional permissions described in the GCC Runtime Library     --
--  Exception, version 3.1, as published by the Free Software Foundation.   --
--                                                                          --
--  You should have received a copy of the GNU General Public License and   --
--  a copy of the GCC Runtime Library Exception along with this program;    --
--  see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see   --
--  <http://www.gnu.org/licenses/>.                                         --
--                                                                          --
--  As a special exception, if other files instantiate generics from this   --
--  unit, or you link this unit with other files to produce an executable,  --
--  this  unit  does not  by itself cause  the resulting executable to be   --
--  covered by the GNU General Public License. This exception does not      --
--  however invalidate any other reasons why the executable file  might be  --
--  covered by the  GNU Public License.                                     --
------------------------------------------------------------------------------

pragma Ada_2012;

package AWS.Net.Stream_IO is

   type Socket_Stream_Type is new Root_Stream_Type with private;

   type Socket_Stream_Access is access Socket_Stream_Type;

   function Stream
     (Socket : Socket_Type'Class) return Socket_Stream_Access;
   --  Build a Stream Socket type

   procedure Shutdown (Stream : Socket_Stream_Access);
   --  Terminate the Stream and Flush the stream if needed

   procedure Free (Stream : in out Socket_Stream_Access);
   --  Release memory associated with the Stream

   procedure Flush (Stream : Socket_Stream_Access) with Inline;
   --  Send all remaining data in the stream to the peer

   overriding procedure Read
     (Stream : in out Socket_Stream_Type;
      Item   : out Stream_Element_Array;
      Last   : out Stream_Element_Offset);
   --  Read a piece of data from the Stream. Returns the data into Item, Last
   --  point to the last Steam_Element read.

   overriding procedure Write
     (Stream : in out Socket_Stream_Type;
      Item   : Stream_Element_Array);
   --  Write Item to the stream

private

   type Socket_Stream_Type is new Root_Stream_Type with record
      Socket : Socket_Access := null;
   end record;

end AWS.Net.Stream_IO;
