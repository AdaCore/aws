------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2004                          --
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

with Ada.Exceptions;
with Ada.Text_IO;
with GNAT.Directory_Operations;

with Service.Status;

with SSL;

with AWS.Server.Log;
pragma Elaborate_All (AWS.Server);
with AWS.Log;
with Runme_CB;

package body Runme_Service is

   use Ada;
   use Ada.Exceptions;
   use GNAT;

   Stop_Request   : Boolean := False;
   pragma Atomic (Stop_Request);

   Running        : Boolean := False;
   pragma Atomic (Running);

   WSS : AWS.Server.HTTP;
   WS  : AWS.Server.HTTP;

   Output : Text_IO.File_Type;

   ----------
   -- Stop --
   ----------

   procedure Stop is
   begin
      Stop_Request := True;

      AWS.Server.Shutdown (WS);
      AWS.Server.Shutdown (WSS);
      AWS.Server.Log.Stop (WS);
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
      --  Change to the right directory
      Directory_Operations.Change_Dir (Runme_Info.Get_Executable_Path);

      --  Set SSL certificate
      AWS.Server.Set_Security
        (WSS, Runme_Info.Get_Executable_Path & "\cert.pem");

      --  All output goes to a log file.
      Text_IO.Create (Output, Text_IO.Out_File, "runme.service.log");
      Text_IO.Set_Output (Output);

      --  Update state.

      Service.Status.Set
        (Target    => Server.Status,
         New_State => Service.Status.Run_State);

      --  Start servers.

      Running := True;

      AWS.Server.Start
        (WSS, "Runme Secure",
         Runme_CB.Service_Sec'Access,
         Max_Connection => 3, Port => 4433, Security => True);

      AWS.Server.Start
        (WS, "Runme", Runme_CB.Service'Access,
         3, "/Admin-Page", 1234, False, True);

      AWS.Server.Log.Start (WS, Split_Mode => AWS.Log.Daily);

      loop
         delay 10.0;
         exit when Stop_Request;
      end loop;

   exception
      when E: others =>
         Text_IO.Put_Line ("Service Error : " & Exception_Information (E));
         Text_IO.Flush;
         raise;
   end Main;

end Runme_Service;
