------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2004-2014, AdaCore                     --
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

with Input_Sources;
with Unicode.CES;

package AWS.Client.XML.Input_Sources is

   Invalid_Encoding : exception renames Unicode.CES.Invalid_Encoding;

   package Sources renames Standard.Input_Sources;

   type HTTP_Input is new Sources.Input_Source with private;
   --  A special implementation of a reader, that reads from an HTTP stream

   procedure Create
     (Connection : HTTP_Connection;
      Input      : out HTTP_Input);
   --  Returns the HTTP_Input stream from a client connection

   overriding procedure Next_Char
     (From : in out HTTP_Input;
      C    : out Unicode.Unicode_Char);
   --  Returns the next character in the file

   overriding function Eof (From : HTTP_Input) return Boolean;
   --  True if From is past the last character in the file

private

   type HTTP_Input is new Sources.Input_Source with record
      Self   : not null access HTTP_Input :=
                 HTTP_Input'Unchecked_Access;
      HTTP   : HTTP_Connection_Access;
      Buffer : Stream_Element_Array (1 .. 4_096);
      First  : Stream_Element_Offset;
      Last   : Stream_Element_Offset;
   end record;

end AWS.Client.XML.Input_Sources;
