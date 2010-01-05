------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2004-2010, AdaCore                     --
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

with System.Address_To_Access_Conversions;

with AWS.Net.Thin;

function Poll
  (Fds     : System.Address;
   Nfds    : AWS.OS_Lib.nfds_t;
   Timeout : C.int) return C.int
is
   use AWS;
   use AWS.Net;
   use AWS.OS_Lib;

   use type C.int;
   use type C.long;
   use type System.Address;

   Failure : constant C.int := -1;

   type FD_Array is array
     (nfds_t range 1 .. OS_Lib.FD_SETSIZE) of Thin.FD_Type;
   pragma Convention (C, FD_Array);

   type Poll_Array is array (nfds_t range 1 .. Nfds) of Thin.Pollfd;
   pragma Convention (C, Poll_Array);

   package Conversion is
     new System.Address_To_Access_Conversions (Poll_Array);

   --  FD_Set_Type is an opaque type and must only be large enough to
   --  match the equivalent C structure.

   type FD_Set_Type is record
      Pad : System.Address; -- this is Count on Win32 for example
      Set : FD_Array;
   end record;
   pragma Convention (C, FD_Set_Type);

   function C_Select
     (Nfds      : C.int;
      readfds   : System.Address;
      writefds  : System.Address;
      exceptfds : System.Address;
      timeout   : System.Address) return C.int;
   pragma Import (Stdcall, C_Select, "select");

   Poll_Ptr  : constant Conversion.Object_Pointer :=
                 Conversion.To_Pointer (Fds);

   Timeout_V : aliased OS_Lib.Timeval;

   Rfds      : aliased FD_Set_Type;
   Rcount    : Natural := 0;
   Wfds      : aliased FD_Set_Type;
   Wcount    : Natural := 0;
   Efds      : aliased FD_Set_Type;

   Rfdsa     : System.Address;
   Wfdsa     : System.Address;

   Width     : C.int := 0;

   FD_Events : Thin.Events_Type;
   Rs        : C.int;

begin
   if Fds = System.Null_Address then
      return Failure;
   end if;

   --  Setup (convert data from poll to select layout)

   Timeout_V.tv_sec  := OS_Lib.timeval_field_t (Timeout) / 1000;
   Timeout_V.tv_usec := OS_Lib.timeval_field_t (Timeout) mod 1000 * 1000;

   OS_Lib.FD_ZERO (Rfds'Address);
   OS_Lib.FD_ZERO (Wfds'Address);
   OS_Lib.FD_ZERO (Efds'Address);

   for J in Poll_Array'Range loop
      Poll_Ptr (J).REvents := 0;

      FD_Events := Poll_Ptr (J).Events;

      if (FD_Events and (POLLIN or POLLPRI)) /= 0 then
         OS_Lib.FD_SET (Poll_Ptr (J).FD, Rfds'Address);
         Rcount := Rcount + 1;
      elsif (FD_Events and POLLOUT) /= 0 then
         OS_Lib.FD_SET (Poll_Ptr (J).FD, Wfds'Address);
         Wcount := Wcount + 1;
      end if;

      OS_Lib.FD_SET (Poll_Ptr (J).FD, Efds'Address);

      Width := C.int'Max (Width, C.int (Poll_Ptr (J).FD));
   end loop;

   --  Any non-null descriptor set must contain at least one handle
   --  to a socket on Windows (MSDN).

   if Rcount = 0 then
      Rfdsa := System.Null_Address;
   else
      Rfdsa := Rfds'Address;
   end if;

   if Wcount = 0 then
      Wfdsa := System.Null_Address;
   else
      Wfdsa := Wfds'Address;
   end if;

   --  Call OS select

   if Timeout < 0 then
      Rs := C_Select
        (Width + 1, Rfdsa, Wfdsa, Efds'Address, System.Null_Address);
   else
      Rs := C_Select
        (Width + 1, Rfdsa, Wfdsa, Efds'Address, Timeout_V'Address);
   end if;

   --  Build result (convert back from select to poll layout)

   if Rs > 0 then
      Rs := 0;

      for J in Poll_Array'Range loop
         if OS_Lib.FD_ISSET (Poll_Ptr (J).FD, Rfds'Address) /= 0 then
            Poll_Ptr (J).REvents := Poll_Ptr (J).REvents or POLLIN;
            Rs := Rs + 1;
         end if;

         if OS_Lib.FD_ISSET (Poll_Ptr (J).FD, Wfds'Address) /= 0 then
            Poll_Ptr (J).REvents := Poll_Ptr (J).REvents or POLLOUT;
            Rs := Rs + 1;
         end if;

         if OS_Lib.FD_ISSET (Poll_Ptr (J).FD, Efds'Address) /= 0 then
            Poll_Ptr (J).REvents := Poll_Ptr (J).REvents or POLLERR;
            Rs := Rs + 1;
         end if;
      end loop;
   end if;

   return Rs;
end Poll;
