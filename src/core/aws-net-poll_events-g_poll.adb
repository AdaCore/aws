------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                       Copyright (C) 2017, AdaCore                        --
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

pragma Ada_2012;

procedure AWS.Net.Poll_Events.G_Poll
  (Fds : in out Set; Timeout : Timeout_Type; Result : out Integer)
is
   use Interfaces;

   use type C.int;
   use type OS_Lib.Events_Type;
   use type OS_Lib.FD_Type;

   function C_Select
     (Nfds      : C.int;
      readfds   : access FD_Set_Type;
      writefds  : access FD_Set_Type;
      exceptfds : access FD_Set_Type;
      timeout   : access OS_Lib.Timeval) return Integer
     with Import => True, Convention => Stdcall, External_Name => "select";

   Timeout_V : aliased OS_Lib.Timeval;
   Timeout_A : access OS_Lib.Timeval;

   Rfds      : aliased FD_Set_Type;
   Rcount    : Natural := 0;
   Wfds      : aliased FD_Set_Type;
   Wcount    : Natural := 0;
   Efds      : aliased FD_Set_Type;

   Rfdsa     : access FD_Set_Type;
   Wfdsa     : access FD_Set_Type;

   FD_Events : OS_Lib.Events_Type;

begin
   --  Setup (convert data from poll to select layout)

   if Timeout >= 0 then
      Timeout_A := Timeout_V'Access;
      Timeout_V.tv_sec  := OS_Lib.timeval_tv_sec_t  (Timeout / 1000);
      Timeout_V.tv_usec :=
        OS_Lib.timeval_tv_usec_t (Timeout rem 1000 * 1000);
   end if;

   FD_ZERO (Rfds);
   FD_ZERO (Wfds);
   FD_ZERO (Efds);

   for J in Fds.Fds'First .. Fds.Length loop
      Fds.Fds (J).REvents := 0;

      FD_Events := Fds.Fds (J).Events;

      if (FD_Events and (OS_Lib.POLLIN or OS_Lib.POLLPRI)) /= 0 then
         FD_SET (Fds.Fds (J).FD, Rfds);
         Rcount := Rcount + 1;
      end if;

      if (FD_Events and OS_Lib.POLLOUT) /= 0 then
         FD_SET (Fds.Fds (J).FD, Wfds);
         Wcount := Wcount + 1;
      end if;

      FD_SET (Fds.Fds (J).FD, Efds);

      if Fds.Fds (J).FD > Fds.Max_FD then
         raise Program_Error with "Wrong Max_FD";
      end if;
   end loop;

   --  Any non-null descriptor set must contain at least one handle
   --  to a socket on Windows (MSDN).

   if Rcount /= 0 then
      Rfdsa := Rfds'Access;
   end if;

   if Wcount /= 0 then
      Wfdsa := Wfds'Access;
   end if;

   --  Call OS select

   Result :=
     C_Select (C.int (Fds.Max_FD + 1), Rfdsa, Wfdsa, Efds'Access, Timeout_A);

   --  Build result (convert back from select to poll layout)

   if Result > 0 then
      Result := 0;

      for J in Fds.Fds'First .. Fds.Length loop
         if FD_ISSET (Fds.Fds (J).FD, Rfds) /= 0 then
            --  Do not need "or" with Poll_Ptr (J).REvents because it's zero

            Fds.Fds (J).REvents := OS_Lib.POLLIN;
         end if;

         if FD_ISSET (Fds.Fds (J).FD, Wfds) /= 0 then
            Fds.Fds (J).REvents := Fds.Fds (J).REvents or OS_Lib.POLLOUT;
         end if;

         if FD_ISSET (Fds.Fds (J).FD, Efds) /= 0 then
            Fds.Fds (J).REvents := Fds.Fds (J).REvents or OS_Lib.POLLERR;
         end if;

         if Fds.Fds (J).REvents /= 0 then
            Result := Result + 1;
         end if;
      end loop;
   end if;
end AWS.Net.Poll_Events.G_Poll;
