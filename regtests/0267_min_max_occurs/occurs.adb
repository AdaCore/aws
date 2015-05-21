------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                       Copyright (C) 2015, AdaCore                        --
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

with Ada.Strings.Unbounded;
with Ada.Text_IO;

with AWS.Config.Set;
with AWS.Server.Status;
with SOAP.Dispatchers.Callback;

with OccursServices.Client;
with OccursServices.Server;
with OccursServices.Types;

with n81.n67.n51.n47.hecateewebservices.encodedtypes.
  t_battle_dimension_type_pkg;

with www.w3.org.n2001.XMLSchema.string_Set_Type_Pkg;

with Occurs_CB;

procedure Occurs is

   use Ada.Strings.Unbounded;
   use AWS;

   use n81.n67.n51.n47.hecateewebservices.encodedtypes.
     t_battle_dimension_type_pkg;
   use OccursServices.Types;
   use www.w3.org.n2001.XMLSchema.string_Set_Type_Pkg;

   function "+" (Str : String)
     return Unbounded_String renames To_Unbounded_String;

   WS   : Server.HTTP;
   H    : Occurs_CB.Handler;
   Conf : Config.Object := Config.Get_Current;

   Af : constant ArrayOfFloat_Type := (1.2, 1.6, 8.0);
   T  : T_Track_Type;

begin
   Ada.Text_IO.Put_Line ("start");

   H := SOAP.Dispatchers.Callback.Create
     (Occurs_CB.HTTP_CB'Access, Occurs_CB.SOAP_CB'Access);

   Config.Set.Server_Host (Conf, "localhost");
   Config.Set.Server_Port (Conf, 0);

   AWS.Server.Start (WS, H, Conf);

   T.p_battleDimension := AIR;
   T.p_tag             := string_Set_Type_Safe_Pointer.To_Safe_Pointer
                            ((+"one", +"two"));

   OccursServices.Client.DisplayTrack
     (T, Af, Endpoint => Server.Status.Local_URL (WS));

   AWS.Server.Shutdown (WS);
end Occurs;
