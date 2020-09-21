------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2020, AdaCore                     --
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

pragma Ada_2012;

with Ada.Calendar;
with Ada.Directories;
with Ada.Finalization;
with Ada.Streams;
with Ada.Strings.Maps;
with Ada.Strings.Unbounded;
with Ada.Task_Identification;
with Ada.Unchecked_Deallocation;

with Templates_Parser.Utils;
with ZLib;

package AWS.Utils is

   use Ada;
   use Ada.Streams;
   use Ada.Strings.Unbounded;
   use Ada.Task_Identification;

   type Random_Integer is range 0 .. Integer'Last;
   --  Integer type for random number generation

   type Null_Record is null record;
   --  Can be used to instantiate generic packages in place of generic
   --  parameters that are not needed.

   subtype File_Size_Type is Directories.File_Size;

   Spaces : constant Strings.Maps.Character_Set :=
              Strings.Maps.To_Set (' ' & ASCII.HT & ASCII.LF & ASCII.CR);
   --  Set of spaces to ignore during parsing

   subtype Hex_String is String with
     Dynamic_Predicate =>
       (for all H of Hex_String => H in '0' .. '9' | 'a' .. 'f' | 'A' .. 'F');

   subtype Time_Zone_String is String with
     Dynamic_Predicate =>
       (Time_Zone_String'Length = 0
          or else
       (Time_Zone_String'Length = 5
        and then Time_Zone_String (Time_Zone_String'First) in '-' | '+'
        and then Time_Zone_String (Time_Zone_String'First + 1) in '0' .. '2'
        and then Time_Zone_String (Time_Zone_String'First + 2) in '0' .. '9'
        and then Time_Zone_String (Time_Zone_String'First + 3) in '0' .. '5'
        and then Time_Zone_String (Time_Zone_String'First + 4) in '0' .. '9'));

   -------------------------------
   --  General helper functions --
   -------------------------------

   function Random return Random_Integer;
   --  Returns a random integer number

   procedure Random_String (Item : out String) with
     Post => (for all C of Item => C in '0' .. '9' | 'a' .. 'z' | 'A' .. 'Z');
   --  Fill string by random printable characters

   function Random_String (Length : Natural) return String with
     Inline,
     Post => Random_String'Result'Length = Length
             and then (for all C of Random_String'Result
                         => C in '0' .. '9' | 'a' .. 'z' | 'A' .. 'Z');
   --  Returns random string

   function Image (N : Natural) return String with
     Post => Image'Result'Length > 0
             and then Image'Result (Image'Result'First) /= ' ';
   --  Returns image of N without the leading blank

   function Image (N : Stream_Element_Offset) return String with
     Post =>
       Image'Result'Length > 0
       and then Image'Result (Image'Result'First) /= ' ';
   --  Returns image of N without the leading blank

   function Image (D : Duration; Aft : Positive := 2) return String with
     Post => Image'Result'Length > 0
             and then Image'Result (Image'Result'First) /= ' ';
   --  Returns image of D without the leading blank and with only Aft decimals
   --  digits.

   function Significant_Image
     (Item : Duration; N : Positive) return String
   with Post =>
     Significant_Image'Result'Length > 0
     and then Significant_Image'Result (Significant_Image'Result'First) /= ' ';
   --  Returns image of D without the leading blank and with N significant
   --  digits. If number of digits in integer part is more than N, the image
   --  would represent the whole integer part.

   function Hex (V : Natural; Width : Natural := 0) return Hex_String with
     Post => (Width > 0 and then Hex'Result'Length = Width)
             or else Hex'Result'Length > 0;
   --  Returns the hexadecimal string representation of the decimal
   --  number V. if Width /= 0, the result will have exactly Width characters
   --  eventually padded with leading 0 or trimmed on the right.

   function Hex_Value (Hex : Hex_String) return Natural with
     Pre => Hex'Length > 0;
   --  Returns the value for the hexadecimal number Hex. Raises
   --  Constraint_Error is Hex is not an hexadecimal number.

   function Is_Number
     (S              : String;
      Allow_Negative : Boolean := False) return Boolean;
   --  Returns True if S contains only decimal digits and is not empty. If
   --  Allow_Negative is True, then also allow the '-' character in the first
   --  position of S.

   function Quote (Str : String; Replace : String := """") return String with
     Post =>
       Quote'Result (Quote'Result'First) = '"'
       and then Quote'Result (Quote'Result'Last) = '"'
       and then Quote'Result'Length >= Str'Length;
   --  Returns Str with character '"' added at the start and the end
   --  Replace parameter is for replacing the '"' inside of string.
   --  It is not replaced by default.

   function Dequote (Str : String) return String with
     Post =>
       Str'Length = 0
       or else Str (Str'First) /= '"'
       or else Str (Str'Last) /= '"'
       or else (Dequote'Result (Dequote'Result'First) /= '"'
                and then Dequote'Result (Dequote'Result'Last) /= '"');
   --  Removes quotes if any around Str and return the resulting string

   function CRLF_2_Spaces (Str : String) return String with
     Post => CRLF_2_Spaces'Result'Length <= Str'Length
             and then (for all C of CRLF_2_Spaces'Result
                        => C not in ASCII.CR | ASCII.LF);
   --  Returns an str in a single line. All CR and LF are converted to spaces,
   --  trailing spaces are removed.

   function Head_Before (Source, Pattern : String) return String with
     Pre  => Pattern'Length > 0,
     Post =>
       (Head_Before'Result'Length = Source'Length
        and then Head_Before'Result = Source)
      or else
       (Head_Before'Result'Length < Source'Length
        and then Head_Before'Result =
                 Source (Source'First
                         .. Source'First + Head_Before'Result'Length - 1));
   --  Returns first part of string before Pattern or the Source string if
   --  Pattern not found.

   procedure Append_With_Sep
     (Content : in out Unbounded_String;
      Value   : String;
      Sep     : String := ", ")
   with
     Inline => True,
     Post   => (if Length (Content'Old) = 0
                then Length (Content) = Value'Length
                else Length (Content) = Length (Content'Old)
                       + Value'Length + Sep'Length);
   --  Append Value into Content, append Sep before value if Content is not
   --  empty.

   function Match (Str, Pattern : String) return Boolean with Inline;
   --  Returns True if Pattern matches the begining of Str. The test is not
   --  case sensitive.

   function Is_Valid_UTF8 (Value : String) return Boolean;
   --  Returns True if Str is a valid UTF-8 sequence

   function Is_Valid_UTF8 (Value : Unbounded_String) return Boolean;
   --  Likewise for an unbounded string

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
   --  the same time excluding all writers (Write). Limited number of writers
   --  can enter (Write) excluding all readers (Read). The task must release
   --  the corresponding resource by calling either Release_Read or
   --  Release_Write. As soon as a writer arrive all readers will wait for it
   --  to complete. Writers discriminant is the maximum number of writers
   --  accepted into the critical section.

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

   ------------------
   -- Test_And_Set --
   ------------------

   --  Could be interpreted by compiler as a lock free operation

   protected type Test_And_Set is

      procedure Try_Lock (Succeeded : out Boolean);
      --  Return True if the lock has been acquired, otherwise don't wait for
      --  the lock and return False.

      procedure Unlock;
      --  Release the lock

      function Locked return Boolean;
      --  Returns the Locked state

   private
      Flag : Boolean := False; -- Mean locked
   end Test_And_Set;

   -------------
   -- Streams --
   -------------

   type Stream_Element_Array_Access is access Streams.Stream_Element_Array;

   type Stream_Element_Array_Constant_Access is
     access constant Streams.Stream_Element_Array;

   procedure Unchecked_Free is new Unchecked_Deallocation
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
      --  Adds Amount to the counter

      procedure Decrement (Amount : Natural := 1);
      --  Decrements counter to the Amount

      procedure Increment (Amount : Natural := 1; Value : out Natural);
      --  Adds Amount to the counter and returns the new value

      procedure Decrement (Amount : Natural := 1; Value : out Natural);
      --  Decrements counter to the Amount and returns the new value

      function Value return Natural;
      --  Returns the current counter value

      entry Zero;
      --  Accepted only when counter is equal to 0

   private
      C : Natural := Initial_Value;
   end Counter;

   type Counter_Access is access Natural;

   procedure Unchecked_Free is
     new Unchecked_Deallocation (Natural, Counter_Access);

   ----------------------------
   -- File oriented routines --
   ----------------------------

   No_Such_File : exception;
   --  Raised be the routines below when a file is not found

   function Is_Regular_File (Filename : String) return Boolean with Inline;
   --  Returns True if Filename is a regular file and is readable

   function Is_Directory (Filename : String) return Boolean with Inline;
   --  Returns True if Filename is a directory

   function File_Size (Filename : String) return File_Size_Type with Inline;
   --  Returns Filename's size in bytes

   function File_Time_Stamp (Filename : String) return Ada.Calendar.Time
     with Inline;
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
   --  Directory_Name does not exist.

   function Get_Program_Directory
     return String renames Templates_Parser.Utils.Get_Program_Directory;
   --  Returns the directory full path name for the current running program

   ----------------------------
   -- Time oriented routines --
   ----------------------------

   function GMT_Clock return Calendar.Time;
   --  Returns current UTC/GMT time

   function Time_Zone return Time_Zone_String;
   --  Returns the current offset between the GMT time and the local time-zone.
   --  The format used is (+|-)HHMM as described into RFC 822. If offset is
   --  zero it returns the empty string.

   function Is_Valid_HTTP_Date (HTTP_Date : String) return Boolean;
   --  Check the date format as some Web brower seems to return invalid date
   --  field.

   AWS_Epoch : constant Calendar.Time := Calendar.Time_Of (2000, 01, 01, 0.0);
   --  AWS birthdate

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
