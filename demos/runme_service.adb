------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                            Copyright (C) 2001                            --
--                               Pascal Obry                                --
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

with Ada.Text_IO;
with GNAT.Directory_Operations;

with Service.Status;

with SSL;

with AWS.Server;
pragma Elaborate_All (AWS.Server);
with AWS.Log;
with Runme_CB;

package body Runme_Service is

   use Ada;
   use GNAT;

   Stop_Request   : Boolean := False;
   Update_Request : Boolean := False;

   Running        : Boolean := False;

   WSS : AWS.Server.HTTP (3);
   WS  : AWS.Server.HTTP (3);

   Output : Text_IO.File_Type;

   ----------
   -- Stop --
   ----------

   procedure Stop is
   begin
      Stop_Request := True;

      AWS.Server.Shutdown (WS);
      AWS.Server.Shutdown (WSS);
      AWS.Server.Stop_Log (WS);
      Text_IO.Close (Output);

      Running := False;
      Service.Status.Set
        (Target    => Server.Status,
         New_State => Service.Status.Stopped_State);
   end Stop;

   -----------------
   -- Interrogate --
   -----------------

   procedure Interrogate is
   begin
      if Running then
         Service.Status.Set
           (Target    => Server.Status,
            New_State => Service.Status.Run_State);
      else
         Service.Status.Set
           (Target    => Server.Status,
            New_State => Service.Status.Stopped_State);
      end if;
   end Interrogate;

   ----------
   -- Main --
   ----------

   procedure Main is
   begin
      --  change to the right directory
      Directory_Operations.Change_Dir (Runme_Info.Get_Executable_Path);

      --  set SSL certificate
      SSL.Set_Certificate (Runme_Info.Get_Executable_Path & "cert.pem");

      --  all output goes to a log file.
      Text_IO.Create (Output, Text_IO.Out_File, "runme.service.log");
      Text_IO.Set_Output (Output);

      --  update state.

      Service.Status.Set
        (Target    => Server.Status,
         New_State => Service.Status.Run_State);

      --  start servers.

      Running := True;

      AWS.Server.Start (WSS, "Runme Secure",
                        Runme_CB.Service_Sec'Access,
                        Port => 4433, Security => True);

      AWS.Server.Start (WS, "Runme", Runme_CB.Service'Access,
                        "/Admin-Page", 1234, False, True);

      AWS.Server.Start_Log (WS, Split_Mode => AWS.Log.Daily);

      loop
         delay 10.0;
         exit when Stop_Request;
      end loop;
   end Main;

end Runme_Service;
