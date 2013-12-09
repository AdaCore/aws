------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2003-2013, AdaCore                     --
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
--  covered by the GNU Public License.                                      --
------------------------------------------------------------------------------

pragma Ada_2012;

package body AWS.Resources.Streams.Memory.ZLib is

   procedure Flush (Resource : in out Stream_Type);
   --  Complete compression, flush internal compression buffer to the
   --  memory stream.

   ------------
   -- Append --
   ------------

   overriding procedure Append
     (Resource : in out Stream_Type;
      Buffer   : Stream_Element_Array;
      Trim     : Boolean := False)
   is
      pragma Unreferenced (Trim);
      --  Ignore the Trim parameter, because stream would be trimmed anyway
      --  in the Flush routine.

      procedure Append (Item : Stream_Element_Array) with Inline;

      ------------
      -- Append --
      ------------

      procedure Append (Item : Stream_Element_Array) is
      begin
         Append (Memory.Stream_Type (Resource), Item);
      end Append;

      procedure Write is new ZL.Write (Append);

   begin
      Write (Resource.Filter, Buffer, ZL.No_Flush);
   end Append;

   -----------
   -- Close --
   -----------

   overriding procedure Close (Resource : in out Stream_Type) is
   begin
      Close (Memory.Stream_Type (Resource));
      ZL.Close (Resource.Filter, Ignore_Error => True);
   end Close;

   ------------------------
   -- Deflate_Initialize --
   ------------------------

   procedure Deflate_Initialize
     (Resource     : in out Stream_Type;
      Level        : Compression_Level  := ZL.Default_Compression;
      Strategy     : Strategy_Type      := ZL.Default_Strategy;
      Method       : Compression_Method := ZL.Deflated;
      Window_Bits  : Window_Bits_Type   := ZL.Default_Window_Bits;
      Memory_Level : Memory_Level_Type  := ZL.Default_Memory_Level;
      Header       : Header_Type        := ZL.Default) is
   begin
      Resource.Flushed := False;

      ZL.Deflate_Init
        (Resource.Filter, Level, Strategy, Method,
         Window_Bits, Memory_Level, Header);
   end Deflate_Initialize;

   -----------
   -- Flush --
   -----------

   procedure Flush (Resource : in out Stream_Type) is
      Flush_Buffer : Stream_Element_Array (1 .. 1_024);
      Last         : Stream_Element_Offset;
   begin
      loop
         ZL.Flush (Resource.Filter, Flush_Buffer, Last, ZL.Finish);

         if Last < Flush_Buffer'Last then
            Append
              (Memory.Stream_Type (Resource),
               Flush_Buffer (1 .. Last),
               Trim => True);

            exit;
         else
            Append (Memory.Stream_Type (Resource), Flush_Buffer);
         end if;
      end loop;

      Resource.Flushed := True;
   end Flush;

   ------------------------
   -- Inflate_Initialize --
   ------------------------

   procedure Inflate_Initialize
     (Resource    : in out Stream_Type;
      Window_Bits : Window_Bits_Type := ZL.Default_Window_Bits;
      Header      : Header_Type      := ZL.Default) is
   begin
      Resource.Flushed := False;

      ZL.Inflate_Init (Resource.Filter, Window_Bits, Header);
   end Inflate_Initialize;

   ----------
   -- Read --
   ----------

   overriding procedure Read
     (Resource : in out Stream_Type;
      Buffer   : out Stream_Element_Array;
      Last     : out Stream_Element_Offset) is
   begin
      if not Resource.Flushed then
         Flush (Resource);
      end if;

      Read (Memory.Stream_Type (Resource), Buffer, Last);
   end Read;

   ----------
   -- Size --
   ----------

   overriding function Size
     (Resource : Stream_Type) return Stream_Element_Offset is
   begin
      if not Resource.Flushed then
         Flush (Resource.Self.all);
      end if;

      return Size (Memory.Stream_Type (Resource));
   end Size;

end AWS.Resources.Streams.Memory.ZLib;
