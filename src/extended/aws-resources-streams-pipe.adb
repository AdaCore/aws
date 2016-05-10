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

with GNAT.Regpat;

with AWS.Translator;

package body AWS.Resources.Streams.Pipe is

   -----------
   -- Close --
   -----------

   overriding procedure Close (Resource : in out Stream_Type) is
      ED   : constant OS_Lib.File_Descriptor :=
               Expect.Get_Error_Fd (Resource.Pid);
      Code : Integer;
      Err  : aliased String (1 .. 4096);
      Last : constant Natural := OS_Lib.Read (ED, Err'Address, Err'Length);
   begin
      Expect.Close (Resource.Pid, Code);

      if Resource.On_Error /= null
        and then (Code /= 0 or else Last > 0)
      then
         Resource.On_Error (Code, Err (1 .. Last));
      end if;
   end Close;

   -----------------
   -- End_Of_File --
   -----------------

   overriding function End_Of_File
     (Resource : Stream_Type) return Boolean is
   begin
      return Resource.EOF;
   end End_Of_File;

   ----------
   -- Open --
   ----------

   procedure Open
     (Pipe     : out Stream_Type;
      Command  : String;
      Args     : OS_Lib.Argument_List;
      Timeout  : Integer := 10_000;
      On_Error : On_Error_Callback := null) is
   begin
      Expect.Non_Blocking_Spawn (Pipe.Pid, Command, Args);
      Pipe.EOF := False;
      Pipe.Timeout := Timeout;
      Pipe.On_Error := On_Error;
   end Open;

   ----------
   -- Read --
   ----------

   overriding procedure Read
     (Resource : in out Stream_Type;
      Buffer   : out Stream_Element_Array;
      Last     : out Stream_Element_Offset)
   is
      Regexp : constant Regpat.Pattern_Matcher :=
                 Regpat.Compile (".+", Flags => Regpat.Single_Line);
      Result : Expect.Expect_Match;
   begin
      while Length (Resource.Buffer) < Buffer'Length loop
         begin
            Expect.Expect (Resource.Pid, Result, Regexp, Resource.Timeout);
         exception
            when Expect.Process_Died =>
               Resource.EOF := True;
               exit;
         end;

         case Result is
            when 1 =>
               Append (Resource.Buffer, Expect.Expect_Out (Resource.Pid));

            when Expect.Expect_Timeout =>
               Last := Buffer'First - 1;
               return;

            when others =>
               Last := Buffer'First - 1;
               return;
         end case;
      end loop;

      declare
         L : constant Natural := Length (Resource.Buffer);
      begin
         if Buffer'Length >= L then
            Last := Buffer'First + Stream_Element_Offset (L) - 1;
            Buffer (Buffer'First .. Last) :=
              Translator.To_Stream_Element_Array (To_String (Resource.Buffer));
            Resource.Buffer := Null_Unbounded_String;

         else
            Buffer := Translator.To_Stream_Element_Array
              (Slice (Resource.Buffer, 1, Buffer'Length));
            Last := Buffer'Last;
            Delete (Resource.Buffer, 1, Buffer'Length);
         end if;
      end;
   end Read;

end AWS.Resources.Streams.Pipe;
