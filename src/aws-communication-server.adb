------------------------------------------------------------------------------
--                             Ada Web Server                               --
--                                                                          --
--                         Copyright (C) 2000-2003                          --
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

with Ada.Unchecked_Conversion;

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

   type Internal_Callback is new Response.Callback;
   --  This is the internal callback access type. It is not possible to use
   --  Receive'Access for as the callback address as the Response.Callback is
   --  outside generic package. We then use Unchecked_Conversion to convert
   --  value from Internal_Callback to Callback.

   function To_Callback is
      new Ada.Unchecked_Conversion (Internal_Callback, Response.Callback);
   --  Conversion function from the internal callback representation to the
   --  standard and user visible callback type.

   function Receive (Request : in Status.Data) return Response.Data;
   --  Handle communication server message.

   -------------
   -- Receive --
   -------------

   function Receive (Request : in Status.Data) return Response.Data is
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

   procedure Start (Port : in Positive; Context : in T_Access) is
      CB : constant Internal_Callback := Receive'Access;
   begin
      Server.Context := Context;

      AWS.Server.Start
        (Com_Server, "Communication Server",
         Max_Connection => 1,
         Port           => Port,
         Callback       => To_Callback (CB));
   end Start;

end AWS.Communication.Server;
