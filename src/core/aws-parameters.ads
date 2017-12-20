------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2017, AdaCore                     --
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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with AWS.Containers.Tables;
with AWS.Resources.Streams.Memory;

package AWS.Parameters is

   type List is new AWS.Containers.Tables.Table_Type with private;

   subtype VString_Array is AWS.Containers.Tables.VString_Array;

   function URI_Format
     (Parameter_List : List; Limit : Positive := Positive'Last) return String;
   --  Returns the list of parameters in the URI format. This can be added
   --  after the resource to form the complete URI. The format is:
   --  "?name1=value1&name2=value2..."
   --  If there is no parameter in the list, the empty string is returned.
   --  Limit is maximum size of the output line, parameters name=value will be
   --  returned unbroken in case of limit applied.

   procedure Add
     (Parameter_List : in out List; Name, Value : String; Decode : Boolean);

   procedure Add
     (Parameter_List : in out List;
      Name, Value    : Unbounded_String;
      Decode         : Boolean);
   --  URL decode and add Name=Value pair into parameters

   procedure Add (Parameter_List : in out List; Parameters : String);
   --  Set parameters for the current request. The Parameters string has the
   --  form "name1=value1&name2=value2...". The paramaters are added to the
   --  list. The parameters can start with a '?' (standard Web character
   --  separator) which is just ignored.

   procedure Add
     (Parameter_List : in out List;
      Parameters     : in out Resources.Streams.Memory.Stream_Type'Class);
   --  Same as above, but use different parameters source. Used to reduce
   --  stack usage on big POST requests. This is the routine used by AWS for
   --  parsing the POST parameters. This routine also control the maximum
   --  number of parameter parsed as set by the corresponding configuration
   --  option.

   procedure Update
     (Parameter_List : in out List; Name, Value : String; Decode : Boolean);

   procedure Update
     (Parameter_List : in out List;
      Name, Value    : Unbounded_String;
      Decode         : Boolean);

   Too_Long_Parameter : exception;
   --  Raised if the Add routine detects a too long parameter line when reading
   --  parameters from Memory_Stream.

   Too_Many_Parameters : exception;
   --  Raised when the maximum number of parameters has been reached

   --  See AWS.Containers.Tables for inherited routines

private

   type List is new AWS.Containers.Tables.Table_Type with null record;

end AWS.Parameters;
