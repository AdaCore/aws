------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2003                          --
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

   procedure Add (Headers : in out List; Name, Value : in String);
   pragma Inline (Add);
   --  Add HTTP header name/value at the end of the Headers container. Note
   --  that there is no check about validity of this header. This service is
   --  provided to be able to create user-defined headers.

   procedure Update
     (Headers : in out List;
      Name    : in     String;
      Value   : in     String;
      N       : in     Positive := 1);
   pragma Inline (Update);
   --  Update the N-th HTTP header Value with the given Name.
   --  The header could already have more than one value associated with
   --  this name. If there is M values with this Name, then if:
   --     N <= M      => update the value
   --     N  = M + 1  => the pair name=value is appended to the table
   --     N  > M + 1  => Constraint_Error raised

   procedure Read (Socket : in Net.Socket_Type'Class; Headers : in out List);
   --  Read and parse HTTP header from the socket.

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
