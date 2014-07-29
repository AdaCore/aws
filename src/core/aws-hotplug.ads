------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2014, AdaCore                     --
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

with AWS.Response;
with AWS.Status;

private with Ada.Containers.Vectors;
private with Ada.Strings.Unbounded;
private with GNAT.Regexp;

package AWS.Hotplug is

   Register_Error : exception;
   --  Raised if the Register command failed

   type Register_Mode is (Add, Replace);
   --  Add     : Add a new filter at the end of the set if there is no such
   --            key, raises Register_Error otherwise (default value)
   --  Replace : Replace existing filter with the same key or add it if
   --            there is no such filter in the set.

   type Filter_Set is private;

   procedure Set_Mode (Filters : in out Filter_Set; Mode : Register_Mode);
   --  Set registering mode for this Filter_Set

   procedure Register
     (Filters : in out Filter_Set;
      Regexp  : String;
      URL     : String);
   --  Add a Filter in the Filter_Set, the URL will be called if the URI match
   --  the regexp. If Regexp already exist it just replace the current entry.

   procedure Unregister
     (Filters : in out Filter_Set;
      Regexp  : String);
   --  Removes a Filter from the Filter_Set. The filter name is defined by the
   --  regular expression. Does nothing if regexp is not found.

   procedure Apply
     (Filters : Filter_Set;
      Status  : AWS.Status.Data;
      Found   : out Boolean;
      Data    : out Response.Data);
   --  Run through the filters and apply the first one for which the regular
   --  expression match the URI. Set Found to True if one filter has been
   --  called and in that case Data contain the answer, otherwise Found is set
   --  to False.

   procedure Move_Up
     (Filters : in out Filter_Set;
      N       : Positive);
   --  Move filter number N up one position, it gives filter number N an
   --  higher priority.

   procedure Move_Down
     (Filters : in out Filter_Set;
      N       : Positive);
   --  Move filter number N down one position, it gives filter number N a
   --  lower priority.

private

   use Ada.Strings.Unbounded;

   type Filter_Data is record
      Regexp_Str : Unbounded_String;   -- The regexp
      Regexp     : GNAT.Regexp.Regexp; -- The compiled regexp
      URL        : Unbounded_String;   -- The redirection URL
   end record;

   function Equal_Data (Left, Right : Filter_Data) return Boolean;
   --  Returns True if Left.Regexp and Right.Regexp are equals

   package Filter_Table is
     new Ada.Containers.Vectors (Positive, Filter_Data, Equal_Data);

   type Filter_Set is record
      Mode : Register_Mode;
      Set  : Filter_Table.Vector;
   end record;

end AWS.Hotplug;
