------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2001                          --
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

package AWS.Headers.Set is

   procedure Parse (Headers : in out List; Line : in String);
   --  Parse HTTP header Line and insert the corresponding data at the end
   --  of the Headers container.

   procedure Add (Headers : in out List; Name, Value : in String);
   pragma Inline (Add);
   --  Add HTTP header name/value at the end of the Headers container. Note
   --  that there is no check about validity of this header. This service is
   --  provided to be able to create user-defined headers.

   procedure Reset (Headers : in out List);
   pragma Inline (Reset);
   --  Removes all object from Headers. Headers will be reinitialized and will
   --  be ready for new use.

   procedure Free (Headers : in out List);
   pragma Inline (Free);
   --  Release all memory used by the List container.

   procedure Debug (Activate : in Boolean);
   --  Turn on Debug output.

end AWS.Headers.Set;
