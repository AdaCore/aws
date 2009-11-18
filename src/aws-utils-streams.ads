------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                       Copyright (C) 2009, AdaCore                        --
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

with GNAT.SHA1;

private with Ada.Strings.Unbounded;

package AWS.Utils.Streams is

   --------------------
   -- String Streams --
   --------------------

   type Strings is new Root_Stream_Type with private;
   --  A stream that reads and writes to a string

   overriding procedure Read
     (Stream : in out Strings;
      Item   : out Stream_Element_Array;
      Last   : out Stream_Element_Offset);
   pragma Inline (Read);

   overriding procedure Write
     (Stream : in out Strings; Item : Stream_Element_Array);
   pragma Inline (Write);

   procedure Open (Stream : in out Strings'Class; Str : String);
   pragma Inline (Open);
   --  Open a new string. Str is the initial value of the string, to which will
   --  be appended the result of 'Output.

   function Value (Stream : access Strings'Class) return String;
   pragma Inline (Value);
   --  Returns the stream value

   ------------------
   -- SHA1 Streams --
   ------------------

   type SHA1 is new Root_Stream_Type with private;
   --  A stream that compute a SHA-1 out of the stream content

   overriding procedure Read
     (Stream : in out SHA1;
      Item   : out Stream_Element_Array;
      Last   : out Stream_Element_Offset);
   pragma Inline (Read);

   overriding procedure Write
     (Stream : in out SHA1; Item : Stream_Element_Array);
   pragma Inline (Write);

   function Value (Stream : access SHA1'Class) return GNAT.SHA1.Message_Digest;
   pragma Inline (Value);
   --  Returns the stream value

private

   use Ada.Strings.Unbounded;

   type Strings is new Root_Stream_Type with record
      Str        : Unbounded_String;
      Read_Index : Natural := 1;
   end record;

   type SHA1 is new Root_Stream_Type with record
      C : GNAT.SHA1.Context;
   end record;

end AWS.Utils.Streams;
