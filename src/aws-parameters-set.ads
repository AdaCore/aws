------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2008, AdaCore                     --
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

with AWS.Containers.Memory_Streams;

package AWS.Parameters.Set is

   procedure Add
     (Parameter_List : in out List;
      Name, Value    : in     String;
      Decode         : in     Boolean := True);
   --  Add a new Key/Value pair into the parameter set.
   --  If Decode is true, decodes Name and Value. This is used when handling
   --  multipart/form-data for example.
   --  A new value is always added, so if there is already a parameter with
   --  that name, Get will still return the old value.

   procedure Add (Parameter_List : in out List; Parameters : in String);
   --  Set parameters for the current request. This is used for a POST method
   --  because the parameters are found in the message body and are not known
   --  when we parse the request line. The Parameters string has the form
   --  "name1=value1&name2=value2...". The paramaters are added to the list.
   --  The parameters can start with a '?' (standard Web character separator)
   --  which is just ignored.

   procedure Add
     (Parameter_List : in out List;
      Parameters     : in out AWS.Containers.Memory_Streams.Stream_Type);
   --  The same as above, but use different parameters source. Used to reduce
   --  stack usage on big POST requests.

   procedure Update
     (Parameter_List : in out List;
      Name, Value    : in     String;
      Decode         : in     Boolean := True);
   --  Same as Add, but replace an existing parameter if there is one

   procedure Case_Sensitive (Parameter_List : in out List; Mode : in Boolean);
   --  If Mode is True it will use all parameters with case sensitivity

   procedure Reset (Parameter_List : in out List);
   --  Removes all object from the Set. Set will be reinitialized and will be
   --  ready for new use.

   Too_Long_Parameter : exception;
   --  Raises if Add routine reading parameters from Memory_Stream detects
   --  too long parameter.

end AWS.Parameters.Set;
