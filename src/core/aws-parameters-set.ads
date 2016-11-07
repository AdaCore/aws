------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2016, AdaCore                     --
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

package AWS.Parameters.Set is

   pragma Obsolescent ("Use same operations from package AWS.Parameters");

   procedure Add
     (Parameter_List : in out List;
      Name, Value    : String;
      Decode         : Boolean := True)
   with Post => Count (Parameter_List) = Count (Parameter_List'Old) + 1
              or else
                Count (Parameter_List, Name) =
                  Count (Parameter_List'Old, Name) + 1;
   --  Add a new Key/Value pair into the parameter set.
   --  If Decode is true, decodes Name and Value. This is used when handling
   --  multipart/form-data for example.
   --  A new value is always added, so if there is already a parameter with
   --  that name, Get will still return the old value.

   procedure Add (Parameter_List : in out List; Parameters : String)
     renames Parameters.Add;
   --  Set parameters for the current request. The Parameters string has the
   --  form "name1=value1&name2=value2...". The paramaters are added to the
   --  list. The parameters can start with a '?' (standard Web character
   --  separator) which is just ignored.

   procedure Add
     (Parameter_List : in out List;
      Parameters     : in out AWS.Resources.Streams.Memory.Stream_Type'Class)
     renames Parameters.Add;
   --  Same as above, but use different parameters source. Used to reduce
   --  stack usage on big POST requests. This is the routine used by AWS for
   --  parsing the POST parameters. This routine also control the maximum
   --  number of parameter parsed as set by the corresponding configuration
   --  option.

   procedure Update
     (Parameter_List : in out List;
      Name, Value    : String;
      Decode         : Boolean := True);
   --  Same as Add, but replace an existing parameter if there is one

   procedure Case_Sensitive (Parameter_List : in out List; Mode : Boolean)
      renames Parameters.Case_Sensitive;
   --  If Mode is True it will use all parameters with case sensitivity

   procedure Reset (Parameter_List : in out List) renames Parameters.Reset;
   --  Removes all object from the Set. Set will be reinitialized and will be
   --  ready for new use.

   Too_Long_Parameter : exception renames Parameters.Too_Long_Parameter;
   --  Raised if the Add routine detects a too long parameter line when reading
   --  parameters from Memory_Stream.

   Too_Many_Parameters : exception renames Parameters.Too_Many_Parameters;
   --  Raised when the maximum number of parameters has been reached

end AWS.Parameters.Set;
