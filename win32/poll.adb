------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                            Copyright (C) 2004                            --
--                               ACT-Europe                                 --
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
--
--  $Id$
--
--  emulates unix "poll" call in Win32.
--
with AWS.Net.Sets.Thin;
with System.Address_To_Access_Conversions;

function Poll
  (Fds     : System.Address;
   Nfds    : C.unsigned_long;
   Timeout : C.int)
   return C.int
is
   use AWS.Net.Sets;

   use type Thin.Events_Type;
   use type C.int;
   use type C.long;
   use type System.Address;

   Failure : constant C.int := -1;

   type Timeval is record
      tv_sec  : C.long; -- Seconds
      tv_usec : C.long; -- Microseconds
   end record;
   pragma Convention (C, Timeval);

   type FD_Array is array (C.unsigned_long range 1 .. Nfds) of C.int;
   pragma Convention (C, FD_Array);

   type Poll_Array is array (FD_Array'Range) of Thin.Pollfd;
   pragma Convention (C, Poll_Array);

   package Conversion is
      new System.Address_To_Access_Conversions (Poll_Array);

   type FD_Set_Type is record
      Count : C.int := 0;
      Set   : FD_Array;
   end record;
   pragma Convention (C, FD_Set_Type);

   procedure FD_SET (FD : C.int; Set : in out FD_Set_Type);

   function FD_ISSET (FD : C.int; Set : System.Address)
     return C.int;
   pragma Import (Stdcall, FD_ISSET, "__WSAFDIsSet");

   function C_Select
     (Nfds      : C.int;
      readfds   : System.Address;
      writefds  : System.Address;
      exceptfds : System.Address;
      timeout   : System.Address)
     return C.int;
   pragma Import (Stdcall, C_Select, "select");

   Poll_Ptr  : constant Conversion.Object_Pointer
     := Conversion.To_Pointer (Fds);

   timeout_v : aliased Timeval;

   rfds : aliased FD_Set_Type;
   wfds : aliased FD_Set_Type;
   efds : aliased FD_Set_Type;

   FD_Events : Thin.Events_Type;
   Rs        : C.int;
   Good      : Boolean;

   ------------
   -- FD_SET --
   ------------

   procedure FD_SET (FD : C.int; Set : in out FD_Set_Type) is
   begin
      Set.Count := Set.Count + 1;
      Set.Set (C.unsigned_long (Set.Count)) := FD;
   end FD_SET;

begin
   if Fds = System.Null_Address then
      return Failure;
   end if;

   timeout_v.tv_sec  := C.long (Timeout) / 1000;
   timeout_v.tv_usec := C.long (Timeout) mod 1000;

   for J in 1 .. Nfds loop
      FD_Events := Poll_Ptr (J).Events;

      if (FD_Events and (Thin.Pollin or Thin.Pollpri)) /= 0 then
         FD_SET (Poll_Ptr (J).FD, rfds);
      elsif (FD_Events and Thin.Pollout) /= 0 then
         FD_SET (Poll_Ptr (J).FD, wfds);
      end if;

      FD_SET (Poll_Ptr (J).FD, efds);
   end loop;

   if Timeout < 0 then
      Rs := C_Select (0, rfds'Address, wfds'Address, efds'Address,
               System.Null_Address);
   else
      Rs := C_Select (0, rfds'Address, wfds'Address, efds'Address,
               timeout_v'Address);
   end if;

   if Rs > 0 then
      Rs      := 0;

      for J in 1 .. Nfds loop
         Good   := False;
         Poll_Ptr (J).REvents := 0;

         if FD_ISSET (Poll_Ptr (J).FD, rfds'Address) /= 0 then
            Good := True;
            Poll_Ptr (J).REvents := Poll_Ptr (J).REvents or Thin.Pollin;
         end if;

         if FD_ISSET (Poll_Ptr (J).FD, wfds'Address) /= 0 then
            Good := True;
            Poll_Ptr (J).REvents := Poll_Ptr (J).REvents or Thin.Pollout;
         end if;

         if FD_ISSET (Poll_Ptr (J).FD, efds'Address) /= 0 then
            Good := True;
            Poll_Ptr (J).REvents := Poll_Ptr (J).REvents or Thin.Pollerr;
         end if;

         if Good then
            Rs := Rs + 1;
         end if;
      end loop;
   end if;

   return Rs;
end Poll;
