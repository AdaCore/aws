------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2009-2014, AdaCore                     --
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

with GNAT.SHA1;

package AWS.Utils.Streams is

   --------------------
   -- String Streams --
   --------------------

   type Strings is new Root_Stream_Type with private;
   --  A stream that reads and writes to a string

   overriding procedure Read
     (Stream : in out Strings;
      Item   : out Stream_Element_Array;
      Last   : out Stream_Element_Offset)
   with Inline;

   overriding procedure Write
     (Stream : in out Strings; Item : Stream_Element_Array)
   with Inline;

   procedure Open (Stream : in out Strings'Class; Str : String) with Inline;
   --  Open a new string. Str is the initial value of the string, to which will
   --  be appended the result of 'Output.

   function Value
     (Stream : not null access Strings'Class) return String with Inline;
   --  Returns the stream value

   ------------------
   -- SHA1 Streams --
   ------------------

   type SHA1 is new Root_Stream_Type with private;
   --  A stream that compute a SHA-1 out of the stream content

   overriding procedure Read
     (Stream : in out SHA1;
      Item   : out Stream_Element_Array;
      Last   : out Stream_Element_Offset)
   with Inline;

   overriding procedure Write
     (Stream : in out SHA1; Item : Stream_Element_Array) with Inline;

   function Value
     (Stream : not null access SHA1'Class) return GNAT.SHA1.Message_Digest
   with Inline;
   --  Returns the stream value

private

   type Strings is new Root_Stream_Type with record
      Str        : Unbounded_String;
      Read_Index : Natural := 1;
   end record;

   type SHA1 is new Root_Stream_Type with record
      C : GNAT.SHA1.Context;
   end record;

end AWS.Utils.Streams;
