------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2002                          --
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

--  $Id$

with Ada.Strings.Unbounded;
with System;

with MD5;

package AWS.Utils is

   type Random_Integer is range 0 .. System.Max_Int;
   --  Integer type for random number generation.

   --  General helper functions are to be found here.

   function Random return Random_Integer;
   --  Returns a random integer number.

   function Image (N : in Natural) return String;
   --  return image of N without the leading blank.

   function Image (D : in Duration) return String;
   --  return image of N without the leading blank and with only 2 decimals
   --  numbers.

   function Hex (V : in Natural; Width : in Natural := 0) return String;
   --  Returns the hexadecimal string representation of the decimal
   --  number V. if Width /= 0, the result will have exactly Width characters
   --  eventually padded with leading 0 or trimmed on the right.

   function Hex_Value (Hex : in String) return Natural;
   --  Returns the value for the hexadecimal number Hex. Raises
   --  Constraint_Error is Hex is not an hexadecimal number.

   function Is_Number (S : in String) return Boolean;
   --  Returns True is S contains only decimal digits.

   generic
      type Enum is (<>);
      type Result_Set is
        array (Enum) of Ada.Strings.Unbounded.Unbounded_String;
   procedure Parse_HTTP_Header_Line
     (Data   : in     String;
      Result :    out Result_Set);
   --  Parse the HTTP header line. Set Result_Set (Name) with the value
   --  corresponding to variable Name.

   function Get_MD5 (Data : in String) return MD5.Digest_String;
   --  Returns the MD5 digest value for the Data string.

   function Quote (Str : in String) return String;
   pragma Inline (Quote);
   --  Returns Str with character '"' added at the start and the end.

   ---------------
   -- Semaphore --
   ---------------

   --  This is a binary semaphore, only a single task can enter it (Seize) and
   --  must call Release when the resource is not needed anymore. This
   --  implement a standard semaphore (P/V mutex).

   protected type Semaphore is
      entry Seize;
      procedure Release;
   private
      Seized : Boolean := False;
   end Semaphore;

   ------------------
   -- RW_Semaphore --
   ------------------

   --  This is a Read/Write semaphore. Many reader tasks can enter (Read) at
   --  the same time excluding all writers (Write). A single writer can enter
   --  (Write) excluding all readers (Read). The task must release the
   --  corresponding resource by calling either Release_Read or Release_Write.
   --  As soon as a writer arrive all readers will wait for it to complete.
   --  Writers discriminant is the maximum number of writers accepted into the
   --  critical section.

   protected type RW_Semaphore (Writers : Positive) is

      --  Readers must call Read to enter the critical section and call
      --  Release_Read at the end.

      entry Read;

      procedure Release_Read;

      --  Writers must call Write to enter the critical section and call
      --  Release_Write at the end.

      entry Write;

      procedure Release_Write;

   private
      R, W : Natural := 0;
   end RW_Semaphore;

end AWS.Utils;
