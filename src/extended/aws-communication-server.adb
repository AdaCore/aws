------------------------------------------------------------------------------
--                             Ada Web Server                               --
--                                                                          --
--                    Copyright (C) 2000-2014, AdaCore                      --
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

with Ada.Unchecked_Conversion;

with AWS.Config.Set;
with AWS.Messages;
with AWS.Parameters;
with AWS.Server;
with AWS.Status;
with AWS.Utils;

package body AWS.Communication.Server is

   Com_Server : AWS.Server.HTTP;
   --  The server that will handle all communication requests

   Context    : T_Access;
   --  The context kept for each server

   type Internal_Callback is not null access
     function (Request : Status.Data) return Response.Data;
   --  This is the internal callback access type. It is not possible to use
   --  Receive'Access for as the callback address as the Response.Callback is
   --  outside generic package. We then use Unchecked_Conversion to convert
   --  value from Internal_Callback to Callback. Note that this type
   --  definition must match exactly the Response.Callback definition.

   function To_Callback is
      new Ada.Unchecked_Conversion (Internal_Callback, Response.Callback);
   --  Conversion function from the internal callback representation to the
   --  standard and user visible callback type.

   function Receive (Request : Status.Data) return Response.Data;
   --  Handle communication server message

   -------------
   -- Receive --
   -------------

   function Receive (Request : Status.Data) return Response.Data is
      URI   : constant String              := Status.URI (Request);
      P_Set : constant AWS.Parameters.List := Status.Parameters (Request);

      procedure Fill_Parameter_Set;
      --  Put all paramters into the PS structure

      --  ??? there is a limit of 100 parameters, seems enough anyway

      PS : Parameter_Set (1 .. 100);
      I  : Natural := 0;

      ------------------------
      -- Fill_Parameter_Set --
      ------------------------

      procedure Fill_Parameter_Set is
      begin
         for K in PS'Range loop
            declare
               P : constant String := 'P' & Utils.Image (K);
            begin
               if AWS.Parameters.Get (P_Set, P) /= "" then
                  I := I + 1;
                  PS (I)
                    := To_Unbounded_String (AWS.Parameters.Get (P_Set, P));
               end if;
            end;
         end loop;
      end Fill_Parameter_Set;

   begin
      if URI = AWS_Com then
         Fill_Parameter_Set;

         return Callback
           (AWS.Parameters.Get (P_Set, "HOST"),
            AWS.Parameters.Get (P_Set, "NAME"),
            Context,
            PS (1 .. I));

      else
         return Response.Acknowledge
           (Messages.S412, "AWS communication message error!");
      end if;
   end Receive;

   --------------
   -- Shutdown --
   --------------

   procedure Shutdown is
   begin
      AWS.Server.Shutdown (Com_Server);
   end Shutdown;

   -----------
   -- Start --
   -----------

   procedure Start
     (Port : Positive; Context : T_Access; Host : String := "")
   is
      CB  : constant Internal_Callback := Receive'Access;
      CNF : Config.Object;
   begin
      Server.Context := Context;

      Config.Set.Server_Name    (CNF, "Communication Server");
      Config.Set.Server_Host    (CNF, Host);
      Config.Set.Server_Port    (CNF, Port);
      Config.Set.Max_Connection (CNF, 1);

      AWS.Server.Start (Com_Server, To_Callback (CB), Config => CNF);
   end Start;

end AWS.Communication.Server;
