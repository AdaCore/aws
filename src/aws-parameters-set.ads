------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2001                          --
--                     Dmitriy Anisimkov & Pascal Obry                      --
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

package AWS.Parameters.Set is

   procedure Add (Parameter_List : in out List;
                  Name, Value    : in     String);
   --  Add a new Key/Value pair into the parameter set.

   procedure Add (Parameter_List : in out List; Parameters : in String);
   --  Set parameters for the current request. This is used for a POST method
   --  because the parameters are found in the message body and are not known
   --  when we parse the request line. The Parameters string has the form
   --  "name1=value1&name2=value2...". The paramaters are added to the list.

   procedure Case_Sensitive
     (Parameter_List : in out List;
      Mode           : in     Boolean);
   --  If Mode is True it will use all parameters with case sensitivity.

   procedure Reset (Parameter_List : in out List);
   --  Release all memory used by Set. Set will be reinitialized and will be
   --  ready for new use.

end AWS.Parameters.Set;
