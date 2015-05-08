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

with Ada.Text_IO;

with SOAP.Message.Payload;
with SOAP.Message.XML;

with Deriveconst_Demo.Server;
with Deriveconst_Demo.Types;

package body CB is

   use Ada;

   procedure Call (Params : Deriveconst_Demo.Types.Big_Type);

   function Call_CB is new Deriveconst_Demo.Server.Call_CB (Call);

   ----------
   -- Call --
   ----------

   procedure Call (Params : Deriveconst_Demo.Types.Big_Type) is
   begin
      Text_IO.Put_Line ("Call" & Integer'Image (Params.Id));
   end Call;

   --------
   -- CB --
   --------

   function CB (Request : AWS.Status.Data) return AWS.Response.Data is
      SOAPAction : constant String := AWS.Status.SOAPAction (Request);
      Payload    : aliased String := AWS.Status.Payload (Request);
      Pl         : SOAP.Message.Payload.Object :=
                     SOAP.Message.XML.Load_Payload
                       (Payload, Schema => Deriveconst_Demo.Schema);
   begin
      if SOAPAction = "call" then
         return Call_Cb (SOAPAction, Pl, Request);

      else
         return AWS.Response.Build
           ("text/html",
            "<p>This is not a SOAP action !");
      end if;
   end CB;

end CB;
