------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2004                          --
--                                ACT-Europe                                --
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

with Ada.Streams;
with Ada.Unchecked_Deallocation;

with ZLib;

package AWS.Utils is

   type Random_Integer is range 0 .. Integer'Last;
   --  Integer type for random number generation

   -------------------------------
   --  General helper functions --
   -------------------------------

   function Random return Random_Integer;
   --  Returns a random integer number

   function Image (N : in Natural) return String;
   --  Returns image of N without the leading blank

   function Image (D : in Duration) return String;
   --  Returns image of N without the leading blank and with only 2 decimals
   --  numbers.

   function Hex (V : in Natural; Width : in Natural := 0) return String;
   --  Returns the hexadecimal string representation of the decimal
   --  number V. if Width /= 0, the result will have exactly Width characters
   --  eventually padded with leading 0 or trimmed on the right.

   function Hex_Value (Hex : in String) return Natural;
   --  Returns the value for the hexadecimal number Hex. Raises
   --  Constraint_Error is Hex is not an hexadecimal number.

   function Is_Number (S : in String) return Boolean;
   --  Returns True is S contains only decimal digits and is not empty

   function Quote (Str : in String) return String;
   pragma Inline (Quote);
   --  Returns Str with character '"' added at the start and the end

   function CRLF_2_Spaces (Str : in String) return String;
   --  Returns an str in a single line. All CR and LF are converted to spaces,
   --  trailing spaces are removed.

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

   -------------
   -- Streams --
   -------------

   type Stream_Element_Array_Access is access Ada.Streams.Stream_Element_Array;

   procedure Free is new Ada.Unchecked_Deallocation
     (Ada.Streams.Stream_Element_Array, Stream_Element_Array_Access);

   -------------
   -- Mailbox --
   -------------

   generic
      type Message is private;
   package Mailbox_G is

      type Message_Set is array (Natural range <>) of Message;

      protected type Mailbox (Max_Size : Positive) is

         entry Add (M : in Message);
         --  Add a new message into the Mailbox, only possible if there is
         --  some free room on the Mailbox.

         entry Get (M : out Message);
         --  Get a message from the Mailbox, only possible if there is some
         --  message in the Mailbox.

         function Size return Natural;
         --  Returns the current number of message waiting in the Mailbox

      private
         Buffer       : Message_Set (1 .. Max_Size);
         Current_Size : Natural := 0;
         Current      : Natural := 0;
         Last         : Natural := 0;
      end Mailbox;

   end Mailbox_G;

   -----------------------
   -- File compresssion --
   -----------------------

   procedure Compress
     (Filename : in String;
      Level    : in ZLib.Compression_Level := ZLib.Default_Compression);
   --  Compress Filename, the compressed file is named filename & ".gz".
   --  If the compression is ok, original file is removed. Raises
   --  Name_Error if Filename does not exist.

   procedure Decompress (Filename : in String);
   --  Decompress Filename, the ".gz" extenssion is removed from the
   --  decompressed file. If the decompression is ok, original file is
   --  removed. Raises Name_Error if Filename does not exist.

end AWS.Utils;
