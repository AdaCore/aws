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

with AWS.Containers.Tables;
with Sockets;

package AWS.Headers is

   type List is new AWS.Containers.Tables.Table_Type with private;
   --  Header container. This set handles a set of HTTP header line, each new
   --  header line is inserted at the end of the list (see AWS.Headers.Set API)
   --  and can be retrieved by the following services. Header lines are
   --  numbered from 1 to N.

   subtype VString_Array is AWS.Containers.Tables.VString_Array;

   subtype Name_Value_Type is AWS.Containers.Tables.Name_Value_Type;

   Format_Error : exception;
   --  Raised when header line format is wrong.

   procedure Send_Header
     (Headers : in List;
      Socket  : in Sockets.Socket_FD'Class);
   --  Send all header lines to the socket appropriate formated.

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

   ------------------------
   -- Inherited routines --
   ------------------------

   --  function Count (Table : in Table_Type) return Natural;
   --  Returns the number of item in Table.

   --  function Name_Count (Table : in Table_Type) return Natural;
   --  Returns the number of unique key name in Table.

   --  function Count (Table : in Table_Type; Name : in String) return Natural;
   --  Returns the number of value for Key Name in Table. It returns
   --  0 if Key does not exist.

   --  function Exist (Table : in Table_Type; Name : in String) return Boolean;
   --  Returns True if Key exist in Table.

   --  function Get
   --    (Table : in Table_Type;
   --     Name  : in String;
   --     N     : in Positive := 1)
   --     return String;
   --  Returns the Nth value associated with Key into Table. Returns
   --  the emptry string if key does not exist.

   --  function Get_Name
   --    (Table : in Table_Type;
   --     N     : in Positive := 1)
   --     return String;
   --  Returns the Nth Name in Table or the empty string if there is
   --  no parameter with this number.

   --  function Get_Value
   --    (Table : in Table_Type;
   --     N     : in Positive := 1)
   --     return String;
   --  Returns the Nth Value in Table or the empty string if there is
   --  no parameter with this number.

   --  function Get_Names (Table : in Table_Type) return VString_Array;
   --  Returns array of unique key names.

   --  function Get_Values
   --    (Table : in Table_Type;
   --     Name  : in String)
   --     return VString_Array;
   --  Returns all values for the specified parameter key name.

private

   type List is new AWS.Containers.Tables.Table_Type with null record;

end AWS.Headers;
