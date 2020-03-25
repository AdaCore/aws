------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                       Copyright (C) 2020, AdaCore                        --
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

with Ada.Float_Text_IO;
with Ada.Long_Float_Text_IO;
with Ada.Text_IO;

with AWS.Config.Set;
with AWS.Server;

with SOAP.Dispatchers.Callback;

with WSDL_NaN_Server;
with WSDL_NaN_Service.Client;

procedure WSDL_NaN_Main is

   use Ada;
   use AWS;

   function Float_Infinity return Float
     with Import, Convention => Intrinsic, External_Name => "__builtin_inff";

   function Long_Float_Infinity return Long_Float
     with Import, Convention => Intrinsic, External_Name => "__builtin_inf";

   procedure T_Float (V : Float) is
      Min : constant Float := 10.0 ** (-Float'Digits);
      Max : constant Float := 10.0 ** (Float'Digits);
      F   : Float := 0.0;
   begin
      F := WSDL_NaN_Service.Client.Echo_F (V);
      if F <= Min or else V >= Max then
         Float_Text_IO.Put (F);
      else
         Float_Text_IO.Put (F, Exp => 0);
      end if;
      Text_IO.New_Line;
   end T_Float;

   procedure T_Double (V : Long_Float) is
      Min : constant Long_Float := 10.0 ** (-Long_Float'Digits);
      Max : constant Long_Float := 10.0 ** (Long_Float'Digits);
      F   : Long_Float := 0.0;
   begin
      F := WSDL_NaN_Service.Client.Echo_D (V);
      if F <= Min or else V >= Max then
         Long_Float_Text_IO.Put (F);
      else
         Long_Float_Text_IO.Put (F, Exp => 0);
      end if;
      Text_IO.New_Line;
   end T_Double;

   WS   : Server.HTTP;

   H    : WSDL_NaN_Server.Handler;

   Conf : Config.Object := Config.Get_Current;

   D    : Long_Float;

begin
   H := SOAP.Dispatchers.Callback.Create
     (WSDL_NaN_Server.HTTP_CB'Access,
      WSDL_NaN_Server.SOAP_CB'Access,
      Schema => WSDL_NaN_Service.Schema);

   Config.Set.Server_Host (Conf, "localhost");
   Config.Set.Server_Port (Conf, 7871);

   Server.Start (WS, H, Conf);

   --  Float

   T_Float (1.1);
   T_Float (0.00001);
   T_Float (0.000001);
   T_Float (1.0E8);
   T_Float (1.0E-15);
   T_Float (+Float_Infinity);
   T_Float (-Float_Infinity);
   T_Float (Float'Invalid_Value);

   --  Long Float

   T_Double (1.2);
   T_Double (0.00000000000001);
   T_Double (0.000000000000001);
   T_Double (1.0E16);
   T_Double (1.0E-25);
   T_Double (+Long_Float_Infinity);
   T_Double (-Long_Float_Infinity);
   T_Double (Long_Float'Invalid_Value);

   Server.Shutdown (WS);
end WSDL_NaN_Main;
