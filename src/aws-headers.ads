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

with AWS.Parameters;

package AWS.Headers is

   type Container is new AWS.Parameters.List;

   Header_Format_Error : exception;
   --  raises when header format is wrong.

   subtype VString_Array is AWS.Parameters.VString_Array;
   --  Just for the Get_Values inherited routine.

   procedure Debug (Activate : in Boolean);
   --  Turn on Debug output.

   function Get_Line
     (C : in Container;
      N : in Positive)
      return String;
   --  Returns the Nth Well formatted Header line in the placing order
   --  or the empty string if there is no header line with this number.

   --  See other inherited routines in the AWS.Parameters.

private

   Debug_Flag : Boolean := False;

end AWS.Headers;
