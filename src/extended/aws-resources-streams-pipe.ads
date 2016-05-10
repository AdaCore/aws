------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2007-2016, AdaCore                     --
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

--  An ready-to-use implementation of the stream API where the stream content
--  is read from a pipe.

with GNAT.OS_Lib;

private with Ada.Strings.Unbounded;
private with GNAT.Expect;

package AWS.Resources.Streams.Pipe is

   use GNAT;

   type Stream_Type is new Streams.Stream_Type with private;

   type On_Error_Callback is
     access procedure (Status : Integer; Error : String);

   procedure Open
     (Pipe     : out Stream_Type;
      Command  : String;
      Args     : OS_Lib.Argument_List;
      Timeout  : Integer := 10_000;
      On_Error : On_Error_Callback := null);
   --  Open the pipe and connect it to the given command's output. Args are
   --  passed to the command. Timeout is given in milliseconds and corresponds
   --  to the time waiting for output data before timeout. This timeout must be
   --  adjusted to be compatible to the output activity of the Command process.

   overriding function End_Of_File (Resource : Stream_Type) return Boolean;

   overriding procedure Read
     (Resource : in out Stream_Type;
      Buffer   : out Stream_Element_Array;
      Last     : out Stream_Element_Offset);

   overriding procedure Close (Resource : in out Stream_Type);

   overriding procedure Reset (Resource : in out Stream_Type) is null;
   --  Does nothing as not supported on pipe streams

   overriding procedure Set_Index
     (Resource : in out Stream_Type;
      To       : Stream_Element_Offset) is null;
   --  Does nothing as not supported on pipe streams

private

   use Ada.Strings.Unbounded;

   type Stream_Type is new Streams.Stream_Type with record
      Pid      : Expect.Process_Descriptor;
      Timeout  : Integer;
      EOF      : Boolean;
      Buffer   : Unbounded_String;
      On_Error : On_Error_Callback;
   end record;

end AWS.Resources.Streams.Pipe;
