------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                          Copyright (C) 2002-2005                         --
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

--  All routines below are buffered both ways (input and output) for better
--  performances.

package AWS.Net.Buffered is

   ------------
   -- Output --
   ------------

   procedure Put (Socket : in Socket_Type'Class; Item : in String);
   --  Write Item into Socket's buffer. Send the buffer to the socket if full

   procedure Put_Line (Socket : in Socket_Type'Class; Item : in String);
   --  Write Item & CRLF into Socket's buffer. Send the buffer to the socket
   --  if full.

   procedure New_Line (Socket : in Socket_Type'Class);
   pragma Inline (New_Line);
   --  Write CRLF into Socket's buffer. Send the buffer to the socket if full.

   procedure Write
     (Socket : in Socket_Type'Class;
      Item   : in Stream_Element_Array);
   --  Write Item into Socket's buffer. Send the buffer to the socket if full.

   procedure Flush (Socket : in Socket_Type'Class);
   --  Send the buffer to the socket

   -----------
   -- Input --
   -----------

   procedure Read
     (Socket : in     Socket_Type'Class;
      Data   :    out Stream_Element_Array);
   pragma Inline (Read);
   --  Returns Data array read from the socket

   function Read
     (Socket : in Socket_Type'Class;
      Max    : in Stream_Element_Count := 4096)
      return Ada.Streams.Stream_Element_Array;
   pragma Inline (Read);
   --  Returns an array of bytes read from the socket

   procedure Read
     (Socket : in     Socket_Type'Class;
      Data   :    out Stream_Element_Array;
      Last   :    out Stream_Element_Offset);
   --  Read any available data from buffered socket.
   --  Wait if no data available.
   --  Same semantic with Net.Receive procedure.

   function Get_Line (Socket : in Socket_Type'Class) return String;
   --  Returns a line read from Socket. A line is a set of character
   --  terminated by CRLF.

   function Get_Char (Socket : in Socket_Type'Class) return Character;
   pragma Inline (Get_Char);
   --  Returns a single character read from socket

   function Peek_Char (Socket : in Socket_Type'Class) return Character;
   pragma Inline (Peek_Char);
   --  Returns next character that will be read from Socket. It does not
   --  actually consume the character, this character will be returned by
   --  the next read operation on the socket.

   procedure Read_Buffer
     (Socket : in     Socket_Type'Class;
      Data   :    out Stream_Element_Array;
      Last   :    out Stream_Element_Offset);
   --  Returns data read from the internal socket's read buffer. No data are
   --  read from the socket. This can be useful when switching to non buffered
   --  mode.

   -------------
   -- Control --
   -------------

   procedure Shutdown (Socket : in Socket_Type'Class);
   --  Shutdown and close the socket. Release all memory and resources
   --  associated with it.

end AWS.Net.Buffered;
