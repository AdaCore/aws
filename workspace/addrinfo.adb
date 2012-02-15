------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2008-2012, AdaCore                     --
--                                                                          --
--  This is free software;  you can redistribute it  and/or modify it       --
--  under terms of the  GNU General Public License as published  by the     --
--  Free Software  Foundation;  either version 3,  or (at your option) any  --
--  later version.  This software is distributed in the hope  that it will  --
--  be useful, but WITHOUT ANY WARRANTY;  without even the implied warranty --
--  of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU     --
--  General Public License for  more details.                               --
--                                                                          --
--  You should have  received  a copy of the GNU General  Public  License   --
--  distributed  with  this  software;   see  file COPYING3.  If not, go    --
--  to http://www.gnu.org/licenses for a complete copy of the license.      --
------------------------------------------------------------------------------

with Ada.Text_IO;
with AWS.Net.Std;
with AWS.OS_Lib;
with Interfaces.C.Strings;
with System;

procedure AddrInfo is
   use Ada.Text_IO;
   use AWS;

   procedure Get_Addr_Info (Host : String; Flags : Interfaces.C.int) is
      use Interfaces;

      package CS renames Interfaces.C.Strings;
      use type C.int;
      use type C.unsigned;
      use type OS_Lib.Addr_Info_Access;

      function getnameinfo
        (sa      : System.Address;
         salen   : OS_Lib.socklen_t;
         host    : CS.chars_ptr;
         hostlen : C.size_t;
         serv    : CS.chars_ptr;
         servlen : C.size_t;
         flags   : C.int) return C.int;
      pragma Import (StdCall, getnameinfo, "getnameinfo");

      C_Node : aliased C.char_array := C.To_C (Host);
      P_Node : CS.chars_ptr;
      C_Serv : aliased C.char_array := C.To_C ("0");
      Res    : C.int;
      Result : aliased OS_Lib.Addr_Info_Access;
      Hints  : constant OS_Lib.Addr_Info :=
                 (ai_family    => OS_Lib.AF_UNSPEC,
                  ai_socktype  => OS_Lib.SOCK_STREAM,
                  ai_protocol  => OS_Lib.IPPROTO_IP,
                  ai_flags     => C.int (Flags),
                  ai_addrlen   => 0,
                  ai_canonname => CS.Null_Ptr,
                  ai_addr      => System.Null_Address,
                  ai_next      => null);

      procedure Check_Res is
      begin
         if Res = OS_Lib.EAI_SYSTEM then
            raise Program_Error with OS_Lib.Socket_Errno'Img;

         elsif Res /= 0 then
            raise Program_Error with CS.Value (OS_Lib.GAI_StrError (Res));
         end if;
      end Check_Res;

      Addr : aliased C.char_array := (0 .. 128 => C.nul);

   begin
      if Host = "" then
         P_Node := CS.Null_Ptr;
      else
         P_Node := CS.To_Chars_Ptr (C_Node'Unchecked_Access);
      end if;

      Res := OS_Lib.GetAddrInfo
               (node    => P_Node,
                service => CS.To_Chars_Ptr (C_Serv'Unchecked_Access),
                hints   => Hints,
                res     => Result'Access);

      Check_Res;

      Put (''' & Host & ''');
      while Result /= null loop
         Res := getnameinfo
                  (sa      => Result.ai_addr,
                   salen   => Result.ai_addrlen,
                   host    => CS.To_Chars_Ptr (Addr'Unchecked_Access),
                   hostlen => Addr'Length,
                   serv    => CS.Null_Ptr,
                   servlen => 0,
                   flags   => OS_Lib.NI_NUMERICHOST);

         Check_Res;

         Put (' ' & C.To_Ada (Addr));

         Result := Result.ai_next;
      end loop;

      New_Line;
   end Get_Addr_Info;

begin
   Put (AWS.Net.Std.Host_Name);

   if Net.IPv6_Available then
      Put_Line (" IPv6");
   else
      New_Line;
   end if;

   Get_Addr_Info ("", OS_Lib.AI_PASSIVE);
   Get_Addr_Info ("localhost", 0);
end AddrInfo;
