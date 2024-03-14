------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2004-2013, AdaCore                     --
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

with Unicode.CCS;
with Unicode.CES.Utf32;
with Unicode.CES.Utf16;
with Unicode.CES.Utf8;

with AWS.Translator;

package body AWS.Client.XML.Input_Sources is

   function "+" (Item : Stream_Element_Array) return String
     renames AWS.Translator.To_String;

   ------------
   -- Create --
   ------------

   procedure Create
     (Connection : HTTP_Connection;
      Input      : out HTTP_Input)
   is
      use Unicode.CES;
      Length : Integer;
      BOM    : Bom_Type;

      First : Stream_Element_Offset          := Input.Buffer'First;
      Last  : constant Stream_Element_Offset := Input.Buffer'Last;
   begin
      Input.HTTP := Connection.Self;

      loop
         Read_Some (Input.HTTP.all, Input.Buffer (First .. Last), Input.Last);

         --  We have to read 4 or more bytes before be able to read BOM
         --  from byte sequence.

         exit when Input.Last >= 4 or else First > Input.Last;

         First := Input.Last + 1;
      end loop;

      Read_Bom (+Input.Buffer (1 .. Input.Last), Length, BOM);

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

      --  Move First index past the BOM

      Input.First := Input.Buffer'First + Stream_Element_Offset (Length);
   end Create;

   ---------
   -- Eof --
   ---------

   overriding function Eof (From : HTTP_Input) return Boolean is
   begin
      if From.First <= From.Last then
         return False;
      end if;

      Read_Some (From.Self.HTTP.all, From.Self.Buffer, From.Self.Last);
      From.Self.First := From.Buffer'First;

      --  Do not change comparision below to From.First > From.Last.
      --  ??? This is workaround of a possible GNAT optimization problem.

      return From.Self.First > From.Self.Last;
   end Eof;

   ---------------
   -- Next_Char --
   ---------------

   overriding procedure Next_Char
     (From : in out HTTP_Input;
      C    : out Unicode.Unicode_Char)
   is
      ES  : Unicode.CES.Encoding_Scheme;
      CS  : Unicode.CCS.Character_Set;
      Pos : Stream_Element_Offset;
   begin
      C := 0;

      if From.First > From.Last then
         --  Unexpected end of stream detected. Data should have been taken
         --  from the HTTP connection in the Eof routine or after partial
         --  character detected.

         raise Invalid_Encoding;
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

         Pos := From.Buffer'First + From.Last - From.First;

         From.Buffer (From.Buffer'First .. Pos) :=
           From.Buffer (From.First .. From.Last);

         From.First := From.Buffer'First;
         From.Last  := Pos;
      end if;

      Read_Encoded_Char : loop
         --  We need a loop to append data to buffer if last character in
         --  buffer is only part of the encoded character.

         --  Store the current position, the start of the character

         Pos := From.First;

         begin
            ES.Read (+From.Buffer, Integer (From.First), C);
         exception
            when Invalid_Encoding =>
               if From.Last - From.First < 5 then
                  --  It can be the case where we have only a part of the
                  --  UTF8 or UTF16 character until From.Last index.
                  --  Characters after the From.Last index is undefined, and
                  --  could cause Invalid_Encoding exception.
                  --  Note that it is not possible to detect this earlier. It
                  --  can be the case that there is less than 6 characters in
                  --  the buffer and that it is not a problem as UTF encoding
                  --  are not using the same number of bytes for all
                  --  characters. So we know about missing bytes in the buffer
                  --  either if we have an Invalid_Encoding exception or if we
                  --  have a From.First more then From.Last + 1 after ES.Read
                  --  call.

                  From.First := From.Last + 2;

               else
                  raise;
               end if;
         end;

         if From.First > From.Last + 1 then
            --  We have a buffer overrun, the read procedure has read bytes
            --  past the last valid character. So we probably had only part
            --  of the character in the buffer. We have to read some more
            --  bytes from the HTTP connection.

            --  Reset start of character at the saved position above

            From.First := Pos;

            Pos := From.Last;

            Read_Some
              (From.HTTP.all,
               Data => From.Buffer (Pos + 1 .. From.Buffer'Last),
               Last => From.Last);

            if From.Last <= Pos then
               --  No more bytes to read, so we have the start of an encoded
               --  character but not the end of it.

               raise Invalid_Encoding;
            end if;

         else
            --  No buffer overrun, C contains a valid character

            C := CS.To_Unicode (C);
            exit Read_Encoded_Char;
         end if;
      end loop Read_Encoded_Char;
   end Next_Char;

end AWS.Client.XML.Input_Sources;
