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

   type List is private;
   --  Header container. This set handles a set of HTTP header line, each new
   --  header line is inserted at the end of the list (see AWS.Headers.Set API)
   --  and can be retrieved by the following services. Header lines are
   --  numbered from 1 to N.

   Format_Error : exception;
   --  Raised when header line format is wrong.

   function Count (Headers : in List) return Natural;
   pragma Inline (Count);
   --  Returns the number of item in Headers.

   function Get_Line
     (Headers : in List;
      N       : in Positive)
      return String;
   --  Returns the Nth header line in Headers container. The returned value is
   --  formatted as a correct header line:
   --
   --     message-header = field-name ":" [ field-value ]
   --
   --  That is the header-name followed with character ':' and the header
   --  values. If there is less than Nth header line it returns the empty
   --  string. Note that this routine does returns all header line values, for
   --  example it would return:
   --
   --     Content_Type: multipart/mixed; boundary="0123_The_Boundary_Value_"
   --
   --  For a file upload content type header style.

private

   type List is new AWS.Parameters.List;

end AWS.Headers;
