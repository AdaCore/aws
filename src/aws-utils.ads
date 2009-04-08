------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2009, AdaCore                     --
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

with Ada.Calendar;
with Ada.Directories;
with Ada.Finalization;
with Ada.Streams;
with Ada.Strings.Maps;
with Ada.Task_Identification;
with Ada.Unchecked_Deallocation;

with Templates_Parser.Utils;
with ZLib;

package AWS.Utils is

   use Ada;
   use Ada.Streams;
   use Ada.Task_Identification;

   type Random_Integer is range 0 .. Integer'Last;
   --  Integer type for random number generation

   type Null_Record is null record;
   --  Can be used to instantiate generic packages in place of generic
   --  parameters that are not needed.

   subtype File_Size_Type is Directories.File_Size;

   Spaces : constant Strings.Maps.Character_Set
     := Strings.Maps.To_Set (' ' & ASCII.HT & ASCII.LF & ASCII.CR);
   --  Set of spaces to ignore during parsing

   -------------------------------
   --  General helper functions --
   -------------------------------

   function Random return Random_Integer;
   --  Returns a random integer number

   procedure Random_String (Item : out String);
   --  Fill string by random printable characters

   function Random_String (Length : Natural) return String;
   pragma Inline (Random_String);
   --  Returns random string

   function Image (N : Natural) return String;
   --  Returns image of N without the leading blank

   function Image (D : Duration) return String;
   --  Returns image of D without the leading blank and with only 2 decimals
   --  numbers.

   function Significant_Image
     (Item : Duration; N : Positive) return String;
   --  Returns image of D without the leading blank and with N significant
   --  digits. If number of digits in integer part is more than N, the image
   --  would represent the whole integer part.

   function Hex (V : Natural; Width : Natural := 0) return String;
   --  Returns the hexadecimal string representation of the decimal
   --  number V. if Width /= 0, the result will have exactly Width characters
   --  eventually padded with leading 0 or trimmed on the right.

   function Hex_Value (Hex : String) return Natural;
   --  Returns the value for the hexadecimal number Hex. Raises
   --  Constraint_Error is Hex is not an hexadecimal number.

   function Is_Number (S : String) return Boolean;
   --  Returns True is S contains only decimal digits and is not empty

   function Quote (Str : String; Replace : String := """") return String;
   --  Returns Str with character '"' added at the start and the end
   --  Replace parameter is for replace the '"' inside of string.
   --  It is not replaced by default.

   function CRLF_2_Spaces (Str : String) return String;
   --  Returns an str in a single line. All CR and LF are converted to spaces,
   --  trailing spaces are removed.

   ---------------
   -- Semaphore --
   ---------------

   --  This is a binary semaphore, only a single task can enter it (Seize) and
   --  must call Release when the resource is not needed anymore. This
   --  implement a standard semaphore (P/V mutex).
   --  After first successful Seize call the same task could call Seize without
   --  blocking. This prevents a task from deadlocking itself while waiting for
   --  a Semaphore that it already owns. To release its ownership under such
   --  circumstances, the task must call Release as many times as Seize was
   --  called.

   protected type Semaphore is
      entry Seize;
      procedure Release;
   private
      entry Seize_Internal;
      TID    : Task_Id := Null_Task_Id;
      Seized : Natural := 0;
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

   type Stream_Element_Array_Access is access Streams.Stream_Element_Array;

   type Stream_Element_Array_Constant_Access is
     access constant Streams.Stream_Element_Array;

   procedure Free is new Unchecked_Deallocation
     (Streams.Stream_Element_Array, Stream_Element_Array_Access);

   ----------------
   --  Finalizer --
   ----------------

   type Finalizer (Action : not null access procedure) is
     new Ada.Finalization.Limited_Controlled with null record;
   --  C++, C#, Borland Delphi, Java, MS Basic have the "finally" block in
   --  exception handlers. Using a finally block it is possible to place some
   --  finalization code executed without depending on how the block is left.
   --  Ada exception handler does not have such block. A Finalizer object
   --  declared in a block allows to emulate a finally in Ada.

   overriding procedure Finalize (Object : in out Finalizer);

   -------------
   -- Mailbox --
   -------------

   generic
      type Message is private;
   package Mailbox_G is

      type Message_Set is array (Natural range <>) of Message;

      protected type Mailbox (Max_Size : Positive) is

         entry Add (M : Message);
         --  Add a new message into the Mailbox, only possible if there is
         --  some free room on the Mailbox.

         entry Get (M : out Message);
         --  Get a message from the Mailbox, only possible if there is some
         --  message in the Mailbox.

         procedure Get (M : out Message; Success : out Boolean);
         --  Get a message from the Mailbox, no wait,
         --  Success would show is message gotten.

         function Size return Natural;
         --  Returns the current number of message waiting in the Mailbox

      private
         Buffer       : Message_Set (1 .. Max_Size);
         Current_Size : Natural := 0;
         Current      : Natural := 0;
         Last         : Natural := 0;
      end Mailbox;

   end Mailbox_G;

   -------------
   -- Counter --
   -------------

   protected type Counter (Initial_Value : Natural) is

      procedure Increment (Amount : Natural := 1);
      --  Adds one to the counter

      procedure Decrement (Amount : Natural := 1);
      --  Removes one to the counter

      procedure Decrement (Amount : Natural := 1; Value : out Natural);
      --  Removes one to the counter and return the current value

      function Value return Natural;
      --  Returns the current counter value

      entry Zero;
      --  Accepted only when counter is equal to 0

   private
      C : Natural := Initial_Value;
   end Counter;

   type Counter_Access is access Natural;

   procedure Free is new Unchecked_Deallocation (Natural, Counter_Access);

   ----------------------------
   -- File oriented routines --
   ----------------------------

   No_Such_File : exception;
   --  Raised be the routines below when a file is not found

   function Is_Regular_File (Filename : String) return Boolean;
   pragma Inline (Is_Regular_File);
   --  Returns True if Filename is a regular file and is readable

   function Is_Directory (Filename : String) return Boolean;
   pragma Inline (Is_Directory);
   --  Returns True if Filename is a directory

   function File_Size (Filename : String) return File_Size_Type;
   pragma Inline (File_Size);
   --  Returns Filename's size in bytes

   function File_Time_Stamp (Filename : String) return Ada.Calendar.Time;
   pragma Inline (File_Time_Stamp);
   --  Get the time for last modification to a file in UTC/GMT

   function Normalized_Directory (Directory : String) return String;
   --  Returns the directory with an ending directory separator

   generic
      with procedure Action
        (Filename     : String;
         Is_Directory : Boolean;
         Quit         : in out Boolean);
   procedure For_Every_Directory_Entry (Directory_Name : String);
   --  Reads all entries in Directory_Name and calls Action for each
   --  one. Is_Directory is set to True if Filename is a directory. Quit can
   --  be set to True to stop the iterator. Raises No_Such_File if
   --  Directory_Name does not exists.

   function Get_Program_Directory
     return String renames Templates_Parser.Utils.Get_Program_Directory;
   --  Returns the directory full path name for the current running program

   ----------------------------
   -- Time oriented routines --
   ----------------------------

   function GMT_Clock return Calendar.Time;
   --  Returns current UTC/GMT time

   function Time_Zone return String;
   --  Returns the current offset between the GMT time and the local time-zone.
   --  The format used is (+|-)HHMM as described into RFC 822. If offset is
   --  zero it returns the empty string.

   -----------------------
   -- File compresssion --
   -----------------------

   procedure Compress
     (Filename : String;
      Level    : ZLib.Compression_Level := ZLib.Default_Compression);
   --  Compress Filename, the compressed file is named filename & ".gz".
   --  If the compression is ok, original file is removed. Raises
   --  Name_Error if Filename does not exist.

   procedure Decompress (Filename : String);
   --  Decompress Filename, the ".gz" extenssion is removed from the
   --  decompressed file. If the decompression is ok, original file is
   --  removed. Raises Name_Error if Filename does not exist.

   --------------
   -- Clonable --
   --------------

   type Clonable is interface;

   function Clone (Element : Clonable) return Clonable is abstract;
   --  Returns a deep copy of Element. The returned object must be
   --  equivalent to Element but fully independent. If Element contains no
   --  access type then Clone can be empty otherwise a new pointer must be
   --  created to point to a copy of the pointed data (and recursively,
   --  as a deep-copy is expected).

end AWS.Utils;
