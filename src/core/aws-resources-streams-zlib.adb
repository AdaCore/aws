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

package body AWS.Resources.Streams.ZLib is

   -----------
   -- Close --
   -----------

   overriding procedure Close (Resource : in out Stream_Type) is
   begin
      if Resource.Source /= null then
         Close (Resource.Source);
         ZL.Close (Resource.Filter, Ignore_Error => True);
      end if;
   end Close;

   --------------------
   -- Deflate_Create --
   --------------------

   function Deflate_Create
     (Source       : Streams.Stream_Access;
      Level        : Compression_Level     := ZL.Default_Compression;
      Strategy     : Strategy_Type         := ZL.Default_Strategy;
      Method       : Compression_Method    := ZL.Deflated;
      Window_Bits  : Window_Bits_Type      := ZL.Default_Window_Bits;
      Memory_Level : Memory_Level_Type     := ZL.Default_Memory_Level;
      Header       : Header_Type           := ZL.Default)
      return Stream_Access
   is
      Result : constant Streams.Stream_Access := new Stream_Type;
   begin
      Deflate_Initialize
        (Stream_Type (Result.all), Source, Level, Strategy, Method,
         Window_Bits, Memory_Level, Header);

      return Result;
   end Deflate_Create;

   ------------------------
   -- Deflate_Initialize --
   ------------------------

   procedure Deflate_Initialize
     (Resource     : in out Stream_Type;
      Source       : Streams.Stream_Access;
      Level        : Compression_Level  := ZL.Default_Compression;
      Strategy     : Strategy_Type      := ZL.Default_Strategy;
      Method       : Compression_Method := ZL.Deflated;
      Window_Bits  : Window_Bits_Type   := ZL.Default_Window_Bits;
      Memory_Level : Memory_Level_Type  := ZL.Default_Memory_Level;
      Header       : Header_Type        := ZL.Default) is
   begin
      Create (Resource.Source, Source);

      Resource.End_Of_File := False;
      Resource.Rest_First  := Resource.Buffer'Last + 1;
      Resource.Rest_Last   := Resource.Buffer'Last;

      ZL.Deflate_Init
        (Resource.Filter, Level, Strategy, Method,
         Window_Bits, Memory_Level, Header);
   end Deflate_Initialize;

   -----------------
   -- End_Of_File --
   -----------------

   overriding function End_Of_File
     (Resource : Stream_Type) return Boolean is
   begin
      --  We could not use return End_Of_File (Resource.Source);
      --  because end of source file would be reached earlier then
      --  end of file of the encoded stream.

      return Resource.End_Of_File;
   end End_Of_File;

   --------------------
   -- Inflate_Create --
   --------------------

   function Inflate_Create
     (Source      : Streams.Stream_Access;
      Window_Bits : Window_Bits_Type      := ZL.Default_Window_Bits;
      Header      : Header_Type           := ZL.Default)
      return Stream_Access
   is
      Result : constant Streams.Stream_Access := new Stream_Type;
   begin
      Inflate_Initialize
        (Stream_Type (Result.all), Source, Window_Bits, Header);

      return Result;
   end Inflate_Create;

   ------------------------
   -- Inflate_Initialize --
   ------------------------

   procedure Inflate_Initialize
     (Resource    : in out Stream_Type;
      Source      : Streams.Stream_Access;
      Window_Bits : Window_Bits_Type := ZL.Default_Window_Bits;
      Header      : Header_Type      := ZL.Default) is
   begin
      Create (Resource.Source, Source);

      Resource.End_Of_File := False;
      Resource.Rest_First  := Resource.Buffer'Last + 1;
      Resource.Rest_Last   := Resource.Buffer'Last;

      ZL.Inflate_Init (Resource.Filter, Window_Bits, Header);
   end Inflate_Initialize;

   ----------
   -- Read --
   ----------

   overriding procedure Read
     (Resource : in out Stream_Type;
      Buffer   : out Stream_Element_Array;
      Last     : out Stream_Element_Offset)
   is
      procedure Get
        (Buffer : out Stream_Element_Array;
         Last   : out Stream_Element_Offset)
        with Inline;
      --  Generic parameter for read source data

      ---------
      -- Get --
      ---------

      procedure Get
        (Buffer : out Stream_Element_Array;
         Last   : out Stream_Element_Offset) is
      begin
         Read (Resource.Source, Buffer, Last);
      end Get;

      procedure Read_Encoded is new ZL.Read
        (Read       => Get,
         Buffer     => Resource.Buffer,
         Rest_First => Resource.Rest_First,
         Rest_Last  => Resource.Rest_Last);

   begin
      Read_Encoded (Resource.Filter, Buffer, Last);

      Resource.End_Of_File := Last < Buffer'Last;
   end Read;

   -----------
   -- Reset --
   -----------

   overriding procedure Reset (Resource : in out Stream_Type) is
   begin
      Reset (Resource.Source);
   end Reset;

   ---------------
   -- Set_Index --
   ---------------

   overriding procedure Set_Index
     (Resource : in out Stream_Type;
      To       : Stream_Element_Offset) is
   begin
      Set_Index (Resource.Source, To);
   end Set_Index;

end AWS.Resources.Streams.ZLib;
