------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                            Copyright (C) 2007                          --
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

--  An ready-to-use implementation of the stream API where the stream content
--  is read from a pipe.

with Ada.Strings.Unbounded;

with GNAT.Expect;
with GNAT.OS_Lib;

package AWS.Resources.Streams.Pipe is

   use GNAT;

   type Stream_Type is new Streams.Stream_Type with private;

   procedure Open
     (Pipe    :    out Stream_Type;
      Command : in     String;
      Args    : in     OS_Lib.Argument_List;
      Timeout : in     Integer := 10_000);
   --  Open the pipe and connect it to the given command's output. Args are
   --  passed to the command. Timeout is given in milliseconds and corresponds
   --  to the time waiting for output data before timeout. This timeout must be
   --  adjusted to be compatible to the output activity of the Command process.

   overriding function End_Of_File (Resource : in Stream_Type) return Boolean;

   overriding procedure Read
     (Resource : in out Stream_Type;
      Buffer   :    out Stream_Element_Array;
      Last     :    out Stream_Element_Offset);

   overriding procedure Close (Resource : in out Stream_Type);

   overriding procedure Reset (Resource : in out Stream_Type);
   --  Does nothing as not supported on pipe streams

   overriding procedure Set_Index
     (Resource : in out Stream_Type;
      To       : in     Stream_Element_Offset);
   --  Does nothing as not supported on pipe streams

private

   use Ada.Strings.Unbounded;

   type Stream_Type is new Streams.Stream_Type with record
      Pid     : Expect.Process_Descriptor;
      Timeout : Integer;
      EOF     : Boolean;
      Buffer  : Unbounded_String;
   end record;

end AWS.Resources.Streams.Pipe;
