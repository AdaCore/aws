------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                            Copyright (C) 2003                            --
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

with Ada.Text_IO;

with AWS.Config.Set;
with AWS.Messages;
with AWS.Response;
with AWS.Server.Log;
with AWS.Services.Split_Pages;
with AWS.Services.Dispatchers.Transient_Pages;
with AWS.Status;
with AWS.Templates;
with AWS.Utils;

procedure Split is

   use Ada;
   use AWS;

   WS : Server.HTTP;

   Disp : Services.Dispatchers.Transient_Pages.Handler;
   Conf : Config.Object := Config.Get_Current;

   function Vect
     (Prefix : in String; Size : in Positive)
      return Templates.Vector_Tag;
   --  Returns a vector tag with Size values, each having the given Prefix

   --------
   -- CB --
   --------

   function CB (Request : in Status.Data) return Response.Data is
      use Templates;

      URI : constant String := Status.URI (Request);

      T1 : Translate_Table
        := (Assoc ("ONE", "one"), Assoc ("TWO", "2"), Assoc ("THREE", "3"));

      T2 : Translate_Table
        := (Assoc ("V1", Vect ("vector 1 - value ", 300)),
            Assoc ("V2", Vect ("vector 2 - value ", 280)));

   begin
      if URI = "/main" then
         return Services.Split_Pages.Parse ("split.thtml", T1, T2, 15, 10);
      else

         return Response.Acknowledge (Messages.S404);
      end if;
   end CB;

   ----------
   -- Vect --
   ----------

   function Vect
     (Prefix : in String; Size : in Positive)
      return Templates.Vector_Tag
   is
      use type Templates.Vector_Tag;
      V : Templates.Vector_Tag;
   begin
      for K in 1 .. Size loop
         V := V & (Prefix & Utils.Image (K));
      end loop;

      return V;
   end Vect;

begin
   Config.Set.Server_Port (Conf, 8080);
   Config.Set.Max_Connection (Conf, 5);

   Services.Dispatchers.Transient_Pages.Register
     (Disp, CB'Unrestricted_Access);

   Server.Start (WS, Disp, Conf);
   Server.Log.Start_Error (WS);

   Ada.Text_IO.Put_Line ("started, press key Q to exit");

   Server.Wait (Server.Q_Key_Pressed);

   Server.Shutdown (WS);
   Ada.Text_IO.Put_Line ("shutdown");
end Split;
