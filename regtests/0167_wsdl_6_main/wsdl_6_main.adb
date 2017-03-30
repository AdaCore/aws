------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2003-2017, AdaCore                     --
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

with Ada.Characters.Handling;
with Ada.Command_Line;
with Ada.Integer_Text_IO;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with AWS.Config.Set;
with AWS.Net.Log.Callbacks;
with AWS.Response;
with AWS.Server;
with AWS.Status;
with SOAP.Dispatchers.Callback;
with SOAP.Types;

with WSDL_6;
with WSDL_6_Server;
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
      use Ada.Strings.Unbounded;
      use Ada.Text_IO;

      use type SOAP.Types.Byte;
      use type SOAP.Types.Long;

      ---------------
      -- Double_IO --
      ---------------

      package Double_IO is new Float_IO (Long_Float);

      --------------
      -- Float_IO --
      --------------

      package Float_IO is new Text_IO.Float_IO (Float);

      -------------
      -- Long_IO --
      -------------

      package Long_IO is new Text_IO.Integer_IO (SOAP.Types.Long);

      --------------
      -- Short_IO --
      --------------

      package Short_IO is new Text_IO.Integer_IO (SOAP.Types.Short);

      -------------
      -- Byte_IO --
      -------------

      package Byte_IO is new Text_IO.Integer_IO (SOAP.Types.Byte);

      ----------------------
      -- Unsigned_Long_IO --
      ----------------------

      package Unsigned_Long_IO is
        new Text_IO.Modular_IO (SOAP.Types.Unsigned_Long);

      ---------------------
      -- Unsigned_Int_IO --
      ---------------------

      package Unsigned_Int_IO is
        new Text_IO.Modular_IO (SOAP.Types.Unsigned_Int);

      -----------------------
      -- Unsigned_Short_IO --
      -----------------------

      package Unsigned_Short_IO is
        new Text_IO.Modular_IO (SOAP.Types.Unsigned_Short);

      ----------------------
      -- Unsigned_Byte_IO --
      ----------------------

      package Unsigned_Byte_IO is
        new Text_IO.Modular_IO (SOAP.Types.Unsigned_Byte);

      ---------
      -- Put --
      ---------

      procedure Put (V : Integer) is
      begin
         Integer_Text_IO.Put (V, Width => 0);
      end Put;

      procedure Put (V : SOAP.Types.Long) is
      begin
         Long_IO.Put (V, Width => 0);
      end Put;

      procedure Put (V : SOAP.Types.Short) is
      begin
         Short_IO.Put (V, Width => 0);
      end Put;

      procedure Put (V : SOAP.Types.Byte) is
      begin
         Byte_IO.Put (V, Width => 0);
      end Put;

      procedure Put (V : SOAP.Types.Unsigned_Long) is
      begin
         Unsigned_Long_IO.Put (V, Width => 0);
      end Put;

      procedure Put (V : SOAP.Types.Unsigned_Int) is
      begin
         Unsigned_Int_IO.Put (V, Width => 0);
      end Put;

      procedure Put (V : SOAP.Types.Unsigned_Short) is
      begin
         Unsigned_Short_IO.Put (V, Width => 0);
      end Put;

      procedure Put (V : SOAP.Types.Unsigned_Byte) is
      begin
         Unsigned_Byte_IO.Put (V, Width => 0);
      end Put;

      procedure Put (V : Boolean) is
      begin
         Put (Characters.Handling.To_Lower (Boolean'Image (V)));
      end Put;

      procedure Put_Float (V : String) is
         K : Natural;
      begin
         K := Strings.Fixed.Index (V, ".");
         loop
            K := K + 1;
            exit when V (K + 1) = '0';
         end loop;
         Put (Strings.Fixed.Trim (V (V'First .. K), Strings.Left));
      end Put_Float;

      procedure Put (V : Float) is
         Buffer : String (1 .. 30);
      begin
         Float_IO.Put (Buffer, V, Exp => 0);
         Put_Float (Buffer);
      end Put;

      procedure Put (V : Long_Float) is
         Buffer : String (1 .. 30);
      begin
         Double_IO.Put (Buffer, V, Exp => 0);
         Put_Float (Buffer);
      end Put;

      procedure Put (V : WSDL_6.Color) is
      begin
         case V is
            when WSDL_6.Red   => Put ("Red");
            when WSDL_6.Green => Put ("GrEEn");
            when WSDL_6.Blue  => Put ("Blue");
         end case;
         New_Line;
      end Put;

      -------------
      -- Put_Rec --
      -------------

      procedure Put_Rec (Rec : WSDL_6.Rec) is
      begin
         Put_Line ("-----");
         Put (Rec.A); New_Line;
         Put (Rec.B); New_Line;
         Put (Rec.C); New_Line;
         Put (Rec.D); New_Line;
         Put (To_String (Rec.E)); New_Line;
         Put (Rec.F); New_Line;
         Put ("-----"); New_Line;
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

      C_Rec : WSDL_6.Complex_Rec;
      C_Res : WSDL_6.Complex_Rec;

   begin
      Put (WSDL_6_Service.Client.Next (WSDL_6.Red));

      Put (WSDL_6_Service.Client.Echo_Int (8)); New_Line;
      Put (WSDL_6_Service.Client.Echo_Int (3)); New_Line;
      Put (WSDL_6_Service.Client.Echo_Short (987)); New_Line;
      Put (WSDL_6_Service.Client.Echo_Long (-543876)); New_Line;
      Put (WSDL_6_Service.Client.Echo_Byte (-102)); New_Line;
      Put (WSDL_6_Service.Client.Echo_Unsigned_Long (101_666_666)); New_Line;
      Put (WSDL_6_Service.Client.Echo_Unsigned_Int (8_654)); New_Line;
      Put (WSDL_6_Service.Client.Echo_Unsigned_Short (65_000)); New_Line;
      Put (WSDL_6_Service.Client.Echo_Unsigned_Byte (101)); New_Line;
      Put (WSDL_6_Service.Client.Echo_Float (89.12)); New_Line;
      Put (WSDL_6_Service.Client.Echo_Double (998877.123456)); New_Line;
      Put (WSDL_6_Service.Client.Echo_Boolean (True));
      New_Line;
      Put (WSDL_6_Service.Client.Echo_Boolean (False));
      New_Line;

      --  Rec

      Res := WSDL_6_Service.Client.Echo_Rec (R);
      Put_Rec (Res);

      --  New_Rec

      N_Res := WSDL_6_Service.Client.Echo_New_Rec (NR);
      Put (N_Res.NC);
      Put_Rec (N_Res.NR);

      --  Array

      for K in I_Arr'Range loop
         I_Arr (K) := K - 1;
      end loop;

      A_Res := WSDL_6_Service.Client.Echo_Set (I_Arr);

      Put_Line ("array");

      for K in A_Res'Range loop
         Put (A_Res (K)); New_Line;
      end loop;

      --  Array of Rec

      for K in R_Arr'Range loop
         R_Arr (K)
           := (K - 1, Float (K - 1), Long_Float (K - 1),
               Character'Val (K + Character'Pos ('a') - 1),
               To_Unbounded_String ("This is number " & Integer'Image (K - 1)),
               K mod 2 = 0);
      end loop;

      R_Res := WSDL_6_Service.Client.Echo_Set_Rec (R_Arr);

      Put_Line ("array of rec");

      for K in R_Res'Range loop
         Put_Rec (R_Res (K));
      end loop;

      --  Record with array

      Put_Line ("array in record");

      C_Rec.SI := WSDL_6.Set_Of_Int_Safe_Pointer.To_Safe_Pointer
        (WSDL_6.Set_Of_Int'(1 | 4 | 8 => 6, 2 | 3 | 5 .. 7 | 9 .. 40 => 2));

      C_Res := WSDL_6_Service.Client.Echo_Complex_Rec (C_Rec);

      for K in C_Res.SI.Item'Range loop
         Put (C_Res.SI.Item (K)); New_Line;
      end loop;
   end Client;

   WS   : AWS.Server.HTTP;
   Conf : Config.Object;
   Disp : WSDL_6_Service.CB.Handler;

begin
   Config.Set.Server_Host (Conf, "localhost");
   Config.Set.Server_Port (Conf, WSDL_6_Service.Server.Port);

   Disp := SOAP.Dispatchers.Callback.Create
     (WSDL_6_Server.HTTP_CB'Access,
      WSDL_6_Service.CB.SOAP_CB'Access,
      WSDL_6_Service.Schema);

   AWS.Server.Start (WS, Disp, Conf);

   AWS.Net.Log.Callbacks.Initialize
     ("wsdl_6.netlog", AWS.Net.Log.Callbacks.Text'Access);

   if Ada.Command_Line.Argument_Count = 1
     and then Ada.Command_Line.Argument (1) = "-j"
   then
      AWS.Server.Wait (AWS.Server.Forever);
      AWS.Net.Log.Callbacks.Finalize;

   else
      Client;

      AWS.Net.Log.Callbacks.Finalize;
      AWS.Server.Shutdown (WS);
   end if;
end WSDL_6_Main;
