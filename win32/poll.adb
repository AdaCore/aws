------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                            Copyright (C) 2004                            --
--                                ACT-Europe                                 --
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

with AWS.Net.Sets.Thin;
with System.Address_To_Access_Conversions;

function Poll
  (Fds     : in System.Address;
   Nfds    : in C.unsigned_long;
   Timeout : in C.int)
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

   procedure FD_SET (FD : in C.int; Set : in out FD_Set_Type);
   pragma Inline (FD_SET);

   function FD_ISSET (FD : in C.int; Set : in System.Address) return C.int;
   pragma Import (Stdcall, FD_ISSET, "__WSAFDIsSet");

   function C_Select
     (Nfds      : in C.int;
      readfds   : in System.Address;
      writefds  : in System.Address;
      exceptfds : in System.Address;
      timeout   : in System.Address)
      return C.int;
   pragma Import (Stdcall, C_Select, "select");

   Poll_Ptr : constant Conversion.Object_Pointer
     := Conversion.To_Pointer (Fds);

   Timeout_V : aliased Timeval;

   Rfds : aliased FD_Set_Type;
   Wfds : aliased FD_Set_Type;
   Efds : aliased FD_Set_Type;

   FD_Events : Thin.Events_Type;
   Rs        : C.int;
   Good      : Boolean;

   ------------
   -- FD_SET --
   ------------

   procedure FD_SET (FD : in C.int; Set : in out FD_Set_Type) is
   begin
      Set.Count := Set.Count + 1;
      Set.Set (C.unsigned_long (Set.Count)) := FD;
   end FD_SET;

begin
   if Fds = System.Null_Address then
      return Failure;
   end if;

   Timeout_V.tv_sec  := C.long (Timeout) / 1000;
   Timeout_V.tv_usec := C.long (Timeout) mod 1000;

   for J in 1 .. Nfds loop
      FD_Events := Poll_Ptr (J).Events;

      if (FD_Events and (Thin.Pollin or Thin.Pollpri)) /= 0 then
         FD_SET (Poll_Ptr (J).FD, Rfds);
      elsif (FD_Events and Thin.Pollout) /= 0 then
         FD_SET (Poll_Ptr (J).FD, Wfds);
      end if;

      FD_SET (Poll_Ptr (J).FD, Efds);
   end loop;

   if Timeout < 0 then
      Rs := C_Select (0, Rfds'Address, Wfds'Address, Efds'Address,
                      System.Null_Address);
   else
      Rs := C_Select (0, Rfds'Address, Wfds'Address, Efds'Address,
                      Timeout_V'Address);
   end if;

   if Rs > 0 then
      Rs := 0;

      for J in 1 .. Nfds loop
         Good   := False;
         Poll_Ptr (J).REvents := 0;

         if FD_ISSET (Poll_Ptr (J).FD, Rfds'Address) /= 0 then
            Good := True;
            Poll_Ptr (J).REvents := Poll_Ptr (J).REvents or Thin.Pollin;
         end if;

         if FD_ISSET (Poll_Ptr (J).FD, Wfds'Address) /= 0 then
            Good := True;
            Poll_Ptr (J).REvents := Poll_Ptr (J).REvents or Thin.Pollout;
         end if;

         if FD_ISSET (Poll_Ptr (J).FD, Efds'Address) /= 0 then
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
