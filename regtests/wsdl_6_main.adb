------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                            Copyright (C) 2003                            --
--                                ACT-Europe                                --
--                                                                          --
--  Authors: Dmitriy Anisimokv - Pascal Obry                                --
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

with Ada.Command_Line;
with Ada.Integer_Text_IO;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with AWS.Config.Set;
with AWS.Server;
with AWS.Status;
with AWS.Response;
with SOAP.Dispatchers.Callback;

with WSDL_6;
with WSDL_5_Server;
with WSDL_6_Service.CB;
with WSDL_6_Service.Client;
with WSDL_6_Service.Server;

procedure WSDL_6_Main is

   use AWS;

   ------------
   -- Client --
   ------------

   procedure Client is

      use Ada;
      use Ada.Integer_Text_IO;
      use Ada.Strings.Unbounded;
      use Ada.Text_IO;

      ---------------
      -- Double_IO --
      ---------------

      package Double_IO is new Float_IO (Long_Long_Float);
      use Double_IO;

      --------------
      -- Float_IO --
      --------------

      package Float_IO is new Text_IO.Float_IO (Long_Float);
      use Client.Float_IO;

      -------------
      -- Put_Rec --
      -------------

      procedure Put_Rec (Rec : in WSDL_6.Rec) is
      begin
         Put_Line ("-----");
         Put (Rec.A); New_Line;
         Put (Rec.B); New_Line;
         Put (Rec.C); New_Line;
         Put (Rec.D); New_Line;
         Put (To_String (Rec.E)); New_Line;
         Put (Boolean'Image (Rec.F)); New_Line;
         Put_Line ("-----");
      end Put_Rec;

      R   : constant WSDL_6.Rec
        := (6, 0.1, 0.2, 'r', To_Unbounded_String ("pascal"), True);

      NR  : constant WSDL_6.New_Rec := (WSDL_6.Blue, R);

      Res   : WSDL_6.Rec;
      N_Res : WSDL_6.New_Rec;

      I_Arr : WSDL_6.Set_Of_Int (1 .. 7);
      A_Res : WSDL_6.Set_Of_Int (1 .. 7);

      R_Arr : WSDL_6.Set_Of_Rec (1 .. 12);
      R_Res : WSDL_6.Set_Of_Rec (1 .. 12);

   begin
      Put_Line
        (WSDL_6.Color'Image (WSDL_6_Service.Client.Next (WSDL_6.Red)));

      Put (WSDL_6_Service.Client.Echo_Int (8)); New_Line;
      Put (WSDL_6_Service.Client.Echo_Int (3)); New_Line;
      Put (WSDL_6_Service.Client.Echo_Float (89.12)); New_Line;
      Put (WSDL_6_Service.Client.Echo_Double (998877.123456)); New_Line;
      Put (Boolean'Image (WSDL_6_Service.Client.Echo_Boolean (True)));
      New_Line;
      Put (Boolean'Image (WSDL_6_Service.Client.Echo_Boolean (False)));
      New_Line;

      --  Rec

      Res := WSDL_6_Service.Client.Echo_Rec (R);
      Put_Rec (Res);

      --  New_Rec

      N_Res := WSDL_6_Service.Client.Echo_New_Rec (NR);
      Put_Line (WSDL_6.Color'Image (N_Res.NC));
      Put_Rec (N_Res.NR);

      --  Array

      for K in I_Arr'Range loop
         I_Arr (K) := K;
      end loop;

      A_Res := WSDL_6_Service.Client.Echo_Set (I_Arr);

      Put_Line ("array");

      for K in A_Res'Range loop
         Put (A_Res (K)); New_Line;
      end loop;

      --  Array of Rec

      for K in R_Arr'Range loop
         R_Arr (K)
           := (K, Long_Float (K), Long_Long_Float (K),
               Character'Val (K + Character'Pos ('a') - 1),
               To_Unbounded_String ("This is number " & Integer'Image (K)),
               K mod 2 = 0);
      end loop;

      R_Res := WSDL_6_Service.Client.Echo_Set_Rec (R_Arr);

      Put_Line ("array of rec");

      for K in R_Res'Range loop
         Put_Rec (R_Res (K)); New_Line;
      end loop;
   end Client;

   WS   : AWS.Server.HTTP;
   Conf : Config.Object;
   Disp : WSDL_6_Service.CB.Handler;

begin
   Config.Set.Server_Port (Conf, WSDL_6_Service.Server.Port);

   Disp := SOAP.Dispatchers.Callback.Create
     (Wsdl_5_Server.HTTP_CB'Access, WSDL_6_Service.CB.SOAP_CB'Access);

   AWS.Server.Start (WS, Disp, Conf);

   if Ada.Command_Line.Argument_Count = 1
     and then Ada.Command_Line.Argument (1) = "-j"
   then
      AWS.Server.Wait (AWS.Server.Forever);

   else
      Client;

      AWS.Server.Shutdown (WS);
   end if;
end WSDL_6_Main;
