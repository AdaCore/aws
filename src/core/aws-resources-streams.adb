------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2002-2021, AdaCore                     --
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

with Ada.IO_Exceptions;
with Ada.Strings.Unbounded;

with AWS.Resources.Embedded;
with AWS.Resources.Streams.Disk.Once;
with AWS.Resources.Streams.Memory;
with AWS.Resources.Streams.ZLib;

package body AWS.Resources.Streams is

   use Ada.Strings.Unbounded;

   ------------
   -- Create --
   ------------

   procedure Create
     (Resource : out File_Type;
      Stream   : Stream_Access) is
   begin
      Resource := File_Type (Stream);
   end Create;

   --------------
   -- Get_Line --
   --------------

   function Get_Line (Resource : in out Stream_Type'Class) return String is
      Result : Unbounded_String;
      B      : Stream_Element_Array (1 .. 1);
      L      : Stream_Element_Offset;
      Stop   : Boolean := False;
   begin
      while not Stop and then not Resource.End_Of_File loop
         Resource.Read (B, L);

         if  B (B'First) not in 13 | 10 then
            Append (Result, Character'Val (Natural (B (B'First))));

         elsif B (B'First) = 10 then
            Stop := True;
         end if;
      end loop;

      return To_String (Result);
   end Get_Line;

   ----------
   -- Name --
   ----------

   function Name (Resource : Stream_Type) return String is
      pragma Unreferenced (Resource);
   begin
      return "";
   end Name;

   ----------
   -- Open --
   ----------

   function Open
     (Name : String;
      Form : String         := "";
      GZip : in out Boolean;
      Once : Boolean        := False) return Stream_Access
   is
      use type Embedded.Buffer_Access;

      In_GZip : constant Boolean := GZip;
      Buffer  : constant Embedded.Buffer_Access :=
                  Embedded.Get_Buffer (Name, GZip);
      Stream  : Stream_Access;
   begin
      if Buffer /= null then
         Stream := new Memory.Stream_Type;
         Memory.Stream_Type (Stream.all).Append (Buffer);

      else
         declare
            Filename : constant String  :=
                         Check_Name (Name, Utils.Is_Regular_File'Access, GZip);
         begin
            if Filename = "" then
               raise Ada.IO_Exceptions.Name_Error with
                 "File '" & Name & "' not found.";

            else
               Stream := (if Once
                          then new Disk.Once.Stream_Type
                          else new Disk.Stream_Type);
               Disk.Stream_Type (Stream.all).Open (Filename, Form => Form);
            end if;
         end;
      end if;

      if GZip and not In_GZip then
         return ZLib.Inflate_Create (Stream, Header => ZLib.ZL.GZip);

      else
         return Stream;
      end if;
   end Open;

   ----------
   -- Size --
   ----------

   overriding function Size
     (Resource : Stream_Type) return Stream_Element_Offset
   is
      pragma Unreferenced (Resource);
   begin
      return Undefined_Length;
   end Size;

end AWS.Resources.Streams;
