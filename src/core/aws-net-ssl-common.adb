------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                       Copyright (C) 2024, AdaCore                        --
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

package body AWS.Net.SSL.Common is

   -----------------------
   -- Get_Client_Method --
   -----------------------

   function Get_Client_Method (Mode : Method) return Method is
     (case Mode is
         when TLSv1   | TLSv1_Client   => TLSv1_Client,
         when TLSv1_1 | TLSv1_1_Client => TLSv1_1_Client,
         when TLSv1_2 | TLSv1_2_Client => TLSv1_2_Client,
         when others                   => TLS_Client);

   -----------------------
   -- Get_Server_Method --
   -----------------------

   function Get_Server_Method (Mode : Method) return Method is
     (case Mode is
         when TLSv1   | TLSv1_Server   => TLSv1_Server,
         when TLSv1_1 | TLSv1_1_Server => TLSv1_1_Server,
         when TLSv1_2 | TLSv1_2_Server => TLSv1_2_Server,
         when others                   => TLS_Server);

   -------------------------------
   -- Initialize_Default_Config --
   -------------------------------

   procedure Initialize_Default_Config
     (Security_Mode        : Method    := TLS;
      Server_Certificate   : String    := Default.Server_Certificate;
      Server_Key           : String    := Default.Server_Key;
      Client_Certificate   : String    := Default.Client_Certificate;
      Priorities           : String    := "";
      Ticket_Support       : Boolean   := False;
      Exchange_Certificate : Boolean   := False;
      Check_Certificate    : Boolean   := True;
      Trusted_CA_Filename  : String    := Default.Trusted_CA;
      CRL_Filename         : String    := "";
      Session_Cache_Size   : Natural   := 16#4000#;
      ALPN                 : SV.Vector := SV.Empty_Vector) is
   begin
      Default_Data :=
        (Security_Mode        => Security_Mode,
         Server_Certificate   => +Server_Certificate,
         Server_Key           => +Server_Key,
         Client_Certificate   => +Client_Certificate,
         Priorities           => +Priorities,
         Ticket_Support       => Ticket_Support,
         Exchange_Certificate => Exchange_Certificate,
         Check_Certificate    => Check_Certificate,
         Trusted_CA_Filename  => +Trusted_CA_Filename,
         CRL_Filename         => +CRL_Filename,
         Session_Cache_Size   => Session_Cache_Size,
         ALPN                 => ALPN);
   end Initialize_Default_Config;

end AWS.Net.SSL.Common;
