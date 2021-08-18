------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2007-2012, AdaCore                     --
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

with Ada.Command_Line;
with Ada.Streams.Stream_IO;
with Ada.Text_IO;

with AWS.Containers.Memory_Streams;

with System;
with ZLib.Streams;

procedure Zeof is
   use Ada;
   use Ada.Streams;
   use ZLib;

   package MS renames AWS.Containers.Memory_Streams;

   F  : Stream_IO.File_Type;
   ZS : ZLib.Streams.Stream_Type;
   SA : Stream_IO.Stream_Access;

   GZip_Size : Stream_Element_Offset := 0;
   Collector : MS.Stream_Type;
   Plain     : access Stream_Element_Array;
   Last      : Stream_Element_Offset;

begin
   Stream_IO.Open (F, Stream_IO.In_File, Ada.Command_Line.Argument (1));

   SA := Stream_IO.Stream (F);

   GZip_Size := Stream_Element_Offset (Stream_IO.Size (F));

   for Buffer_Size in reverse GZip_Size - 20 .. GZip_Size + 1 loop
      ZLib.Streams.Create
        (ZS,
         Mode             => ZLib.Streams.In_Stream,
         Back             => ZLib.Streams.Stream_Access (SA),
         Back_Compressed  => True,
         Header           => GZip,
         Read_Buffer_Size => Buffer_Size);

      if Plain = null then
         while not ZS.End_Of_Stream loop
            declare
               Buffer : Stream_Element_Array (1 .. 1024);
            begin
               ZS.Read (Buffer, Last);
               MS.Append (Collector, Buffer (1 .. Last));

               if Last < Buffer'Last and then not ZS.End_Of_Stream then
                  Text_IO.Put_Line
                    ("EOF not detected" & Last'Img & Buffer'Length'Img);
               end if;
            end;
         end loop;

         Plain := new Stream_Element_Array (1 .. MS.Size (Collector));
         MS.Read (Collector, Plain.all, Last);

         if Last /= Plain'Last then
            Text_IO.Put_Line ("Wrong Last" & Last'Img);
         end if;

         if MS.Pending (Collector) > 0 then
            Text_IO.Put_Line ("Wrong pending" & MS.Pending (Collector)'Img);
         end if;

         MS.Close (Collector);

      else
         Stream_IO.Set_Index (F, 1);

         declare
            Buffer : Stream_Element_Array (Plain.all'Range);
         begin
            ZS.Read (Buffer, Last);
            if Last /= Buffer'Last then
               Text_IO.Put_Line
                 ("Wrong Last" & Last'Img & " on buffer" & Buffer_Size'Img);
            end if;

            if Buffer /= Plain.all then
               Text_IO.Put_Line
                 ("Wrong inflate on buffer" & Buffer_Size'Img);
            end if;

            if not ZS.End_Of_Stream then
               Text_IO.Put_Line
                 ("End of stream is not detected on buffer" & Buffer_Size'Img);

               ZS.Read (Buffer, Last);

               if Last >= Buffer'First then
                  Text_IO.Put_Line
                    ("Inflate longer than expected on " & Buffer_Size'Img);
               end if;

               if not ZS.End_Of_Stream then
                  Text_IO.Put_Line
                    ("End of stream is not detected at all on buffer"
                     & Buffer_Size'Img);
               end if;
            end if;
         end;
      end if;

      ZS.Close;
   end loop;

   Stream_IO.Close (F);
end Zeof;
