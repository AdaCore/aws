------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                      Copyright (C) 2021, AdaCore                         --
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

--  Support for HAPCK (Header compression) for HTTP/2 protocol

with AWS.Headers;

limited with AWS.HTTP2.HPACK.Table;
limited with AWS.HTTP2.Connection;

package AWS.HTTP2.HPACK is

   generic
      with function End_Of_Stream return Boolean;
      with function Get_Byte return Stream_Element;
   function Decode
     (Table    : not null access HPACK.Table.Object;
      Settings : not null access HTTP2.Connection.Object) return Headers.List;
   --  Decode a stream and return the conrresponding header list

   function Encode
     (Table    : not null access HPACK.Table.Object;
      Settings : not null access Connection.Object;
      List     : Headers.List) return Stream_Element_Array;
   --  Encode the header list

end AWS.HTTP2.HPACK;
