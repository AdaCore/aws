------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2002-2015, AdaCore                     --
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

--  All routines below are buffered both ways (input and output) for better
--  performances.

package AWS.Net.Buffered is

   ------------
   -- Output --
   ------------

   procedure Put (Socket : Socket_Type'Class; Item : String);
   --  Write Item into Socket's buffer. Send the buffer to the socket if full

   procedure Put_Line (Socket : Socket_Type'Class; Item : String);
   --  Write Item & CRLF into Socket's buffer. Send the buffer to the socket
   --  if full.

   procedure New_Line (Socket : Socket_Type'Class) with Inline;
   --  Write CRLF into Socket's buffer. Send the buffer to the socket if full

   procedure Write
     (Socket : Socket_Type'Class; Item : Stream_Element_Array);
   --  Write Item into Socket's buffer. Send the buffer to the socket if full

   procedure Flush (Socket : Socket_Type'Class);
   --  Send the buffer to the socket

   -----------
   -- Input --
   -----------

   Data_Overflow : exception;
   --  Raised from Get_Line and Read_Until routines when size of receiving data
   --  exceeds the limit defined by Set_Input_Limit. It avoid unlimited dynamic
   --  memory allocation inside of Get_Line and Read_Until when client trying
   --  to attack the server by the very long lines in request. Moreover it
   --  avoid stack overflow on very long data returned from Get_Line and
   --  Read_Until.

   procedure Set_Input_Limit (Limit : Positive) with Inline;
   --  Set the input size limit for Get_Line and Read_Until routines

   function Get_Input_Limit return Stream_Element_Offset with Inline;
   --  Get the input size limit for Get_Line and Read_Until routines

   procedure Read
     (Socket : Socket_Type'Class; Data : out Stream_Element_Array) with Inline;
   --  Returns Data array read from the socket

   function Read
     (Socket : Socket_Type'Class;
      Max    : Stream_Element_Count := 4096) return Stream_Element_Array
     with Inline;
   --  Returns an array of bytes read from the socket

   procedure Read
     (Socket : Socket_Type'Class;
      Data   : out Stream_Element_Array;
      Last   : out Stream_Element_Offset);
   --  Read any available data from buffered socket.
   --  Wait if no data available.
   --  Same semantic with Net.Receive procedure.

   function Get_Line (Socket : Socket_Type'Class) return String;
   --  Returns a line read from Socket. A line is a set of character
   --  terminated by CRLF.

   function Get_Char (Socket : Socket_Type'Class) return Character with Inline;
   --  Returns a single character read from socket

   function Peek_Char (Socket : Socket_Type'Class) return Character
     with Inline;
   --  Returns next character that will be read from Socket. It does not
   --  actually consume the character, this character will be returned by
   --  the next read operation on the socket.

   procedure Read_Buffer
     (Socket : Socket_Type'Class;
      Data   : out Stream_Element_Array;
      Last   : out Stream_Element_Offset);
   --  Returns data read from the internal socket's read buffer. No data are
   --  read from the socket. This can be useful when switching to non buffered
   --  mode.

   function Read_Until
     (Socket    : Socket_Type'Class;
      Delimiter : Stream_Element_Array;
      Wait      : Boolean := True) return Stream_Element_Array;
   --  Read data on the Socket until the delimiter (including the delimiter).
   --  If Wait is False the routine looking for the delimiter only in the
   --  cache buffer, if delimiter not found in the cache buffer, empty array
   --  is be returned.
   --  If returned data is without delimiter at the end, it means that socket
   --  is closed from peer or socket error occured and rest of data returned.
   --  This routine could loose some data on timeout if does not meet delimiter
   --  longer then Read buffer size.

   function Read_Until
     (Socket    : Socket_Type'Class;
      Delimiter : String;
      Wait      : Boolean := True) return String;
   --  Same as above but returning a standard string

   -------------
   -- Control --
   -------------

   procedure Shutdown (Socket : Socket_Type'Class);
   --  Shutdown and close the socket. Release all memory and resources
   --  associated with it.

end AWS.Net.Buffered;
