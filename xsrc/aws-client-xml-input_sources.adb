------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                            Copyright (C) 2004                            --
--                                ACT-Europe                                --
--                                                                          --
--  Authors: Dmitriy Anisimkov - Pascal Obry                                --
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

--  $RCSfile$
--  $Revision$
--  $Date$
--  $Author$

with AWS.Translator;

with Unicode.CCS;

with Unicode.CES;
with Unicode.CES.Utf32;
with Unicode.CES.Utf16;
with Unicode.CES.Utf8;

package body AWS.Client.XML.Input_Sources is

   function "+" (Item : Stream_Element_Array) return String
     renames AWS.Translator.To_String;

   ------------
   -- Create --
   ------------

   procedure Create
     (Connection : in     HTTP_Connection;
      Input      :    out HTTP_Input)
   is
      use Unicode.CES;
      Length : Integer;
      BOM    : Bom_Type;
   begin
      Input.HTTP := Connection.Self;

      Read_Some (Input.HTTP.all, Input.Buffer, Input.Last);

      Read_Bom (+Input.Buffer, Length, BOM);

      case BOM is
         when Utf32_LE =>
            Set_Encoding (Input, Utf32.Utf32_LE_Encoding);

         when Utf32_BE =>
            Set_Encoding (Input, Utf32.Utf32_BE_Encoding);

         when Utf16_LE =>
            Set_Encoding (Input, Utf16.Utf16_LE_Encoding);

         when Utf16_BE =>
            Set_Encoding (Input, Utf16.Utf16_BE_Encoding);

         when Ucs4_BE | Ucs4_LE | Ucs4_2143 | Ucs4_3412 =>
            raise Invalid_Encoding;

         when Utf8_All | Unknown =>
            Set_Encoding (Input, Utf8.Utf8_Encoding);
      end case;

      -- Move First index past the BOM

      Input.First := Input.First + Stream_Element_Offset (Length);
   end Create;

   ---------
   -- Eof --
   ---------

   function Eof (From : in HTTP_Input) return Boolean is
   begin
      if From.First > From.Last then
         Read_Some (From.Self.HTTP.all, From.Self.Buffer, From.Self.Last);
         From.Self.First := From.Buffer'First;
      end if;

      return From.First > From.Last;
   end Eof;

   ---------------
   -- Next_Char --
   ---------------

   procedure Next_Char
     (From : in out HTTP_Input;
      C    :    out Unicode.Unicode_Char)
   is
      ES   : Unicode.CES.Encoding_Scheme;
      CS   : Unicode.CCS.Character_Set;
      Temp : Stream_Element_Offset;
   begin
      if From.First > From.Last then
         --  Unexpected end of stream detected. Data should have been taken
         --  from the HTTP connection in the Eof routine or after partial
         --  character detected.
         raise Unicode.CES.Invalid_Encoding;
      end if;

      ES := Get_Encoding (From);
      CS := Get_Character_Set (From);

      if From.Buffer'Last - From.First < 5 then
         --  UTF8 encoding character length can take from 1 to 6 bytes.
         --  UTF16 can be from 2 or 4 bytes.
         --  Unicode.CES.Read routine is in danger to violate byte sequence
         --  range if last character in buffer is only a part of the encoded
         --  character. We move remaining data in buffer to the begin, to
         --  have some place if we need to read some data.

         Temp := From.Buffer'First + From.Last - From.First;

         From.Buffer (From.Buffer'First .. Temp)
           := From.Buffer (From.First .. From.Last);

         From.First := From.Buffer'First;
         From.Last  := Temp;
      end if;

      Read_Encoded_Char : loop
         --  We need a loop to append data to buffer if last character in
         --  buffer is only part of the encoded character.

         Temp := From.First;

         begin
            ES.Read (+From.Buffer, Integer (From.First), C);
         exception
            when Unicode.CES.Invalid_Encoding =>
               if From.Last - From.First < 5 then
                  --  It can be the case where we have only a part of the
                  --  UTF8 or UTF16 character. Set From.First to the last
                  --  buffer position to force reading more bytes in the next
                  --  section.
                  --  ??? Note, we could not distinguish character portion from
                  --  the wrong encoding in the first attempt when number of
                  --  bytes is less then 5 with the current XMLAda interface.
                  From.First := From.Buffer'Last;

               else
                  raise;
               end if;
         end;

         if From.First > From.Last + 1 then
            --  We have only a part of the character in the buffer. We have to
            --  read some more bytes from the HTTP connection.

            From.First := Temp;

            Temp := From.Last;

            Read_Some
              (From.HTTP.all,
               Data => From.Buffer (Temp + 1 .. From.Buffer'Last),
               Last => From.Last);

            if From.Last <= Temp then
               --  No more bytes to read, so we have the start of an encoded
               --  character but not the end of it.
               raise Unicode.CES.Invalid_Encoding;
            end if;

         else
            C := CS.To_Unicode (C);
            exit Read_Encoded_Char;
         end if;
      end loop Read_Encoded_Char;
   end Next_Char;

end AWS.Client.XML.Input_Sources;
