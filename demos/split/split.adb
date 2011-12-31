------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2003-2012, AdaCore                     --
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

with AWS.Config.Set;
with AWS.Messages;
with AWS.Response;
with AWS.Server.Log;
with AWS.Services.Split_Pages.Alpha;
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
     (Prefix : String; Size : Positive)
      return Templates.Vector_Tag;
   --  Returns a vector tag with Size values, each having the given Prefix

   --------
   -- CB --
   --------

   function CB (Request : Status.Data) return Response.Data is
      use Templates;

      URI : constant String := Status.URI (Request);

      T1 : Translate_Table
        := (Assoc ("ONE", "one"), Assoc ("TWO", "2"), Assoc ("THREE", "3"));

      T2 : Translate_Table
        := (Assoc ("V1", Vect ("vector 1 - value ", 300)),
            Assoc ("V2", Vect ("vector 2 - value ", 280)));

      T3 : Translate_Table
        := (Assoc ("V1", Vect ("vector 1 - value ", 300)),
            Assoc ("V2", Vect ("vector 2 - value ", 280)),
            Assoc ("KEY", Vect ("key", 300)));

      A_Splitter : Services.Split_Pages.Alpha.Splitter;

   begin
      if URI = "/main" then
         return Services.Split_Pages.Parse ("split.thtml", T1, T2, 15, 10);

      elsif URI = "/alpha" then
         Services.Split_Pages.Alpha.Set_Key (A_Splitter, "KEY");
         return Services.Split_Pages.Parse
           ("split.thtml", T1 & Assoc ("ALPHA", "true"), T3, A_Splitter);

      else
         return Response.Acknowledge (Messages.S404);
      end if;
   end CB;

   ----------
   -- Vect --
   ----------

   function Vect
     (Prefix : String; Size : Positive)
      return Templates.Vector_Tag
   is
      use type Templates.Vector_Tag;
      N : constant Natural := (Size / 26) + 1;
      V : Templates.Vector_Tag;
      A : Natural := 0;
      C : Character := 'A';
   begin
      if Prefix = "key" then
         V := V & "" & "" & "1_key" & "5_key" & "9_key";
      end if;

      for K in 1 .. Size loop
         if Prefix = "key" then
            V := V & (C & "_" & Prefix & Utils.Image (K));
            A := A + 1;

            if A = N then
               C := Character'Succ (C);
               A := 0;
            end if;
         else
            V := V & (Prefix & Utils.Image (K));
         end if;
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
