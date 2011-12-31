------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2004-2012, AdaCore                     --
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

--  Package with declarations for the Poll operation. This API is used to
--  implement AWS.Net.Sets.

with Interfaces.C.Strings;
with System;

with AWS.OS_Lib;

package AWS.Net.Thin is

   use Interfaces;

   subtype FD_Type is OS_Lib.FD_Type;
   subtype nfds_t is OS_Lib.nfds_t;
   subtype Timeout_Type is C.int;
   subtype Events_Type is OS_Lib.Events_Type;

   subtype chars_ptr is C.Strings.chars_ptr;

   type Pollfd is record
      FD      : FD_Type;
      Events  : Events_Type := 0;
      REvents : Events_Type := 0;
   end record;
   pragma Convention (C, Pollfd);

   function Poll
     (Fds     : System.Address;
      Nfds    : nfds_t;
      Timeout : Timeout_Type) return C.int;
   pragma Import (C, Poll, "poll");

end AWS.Net.Thin;
